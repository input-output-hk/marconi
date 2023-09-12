{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Allow the execution of indexers on a Cardano node using the chain sync protocol
module Marconi.ChainIndex.Experimental.Runner (
  runIndexer,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (
  BlockEvent (BlockEvent),
  ChainSyncEvent (RollBackward, RollForward),
  ChainSyncEventException (NoIntersectionFound),
  withChainSyncBlockEventStream,
 )
import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Trace qualified as Trace
import Control.Concurrent qualified as Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Exception (catch)
import Control.Monad.Except (ExceptT, void)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Marconi.ChainIndex.Experimental.Extract.WithDistance (WithDistance)
import Marconi.ChainIndex.Experimental.Extract.WithDistance qualified as Distance
import Marconi.ChainIndex.Experimental.Indexers.Orphans qualified ()
import Marconi.ChainIndex.Logging (chainSyncEventStreamLogging)
import Marconi.ChainIndex.Node.Client.Retry (RetryConfig, withNodeConnectRetry)
import Marconi.ChainIndex.Types (SecurityParam)
import Marconi.Core.Experiment qualified as Core
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Streaming qualified as S
import Streaming.Prelude qualified as S

-- | Wraps as a datatype log message emitted by the 'runIndexer' et al. functions.
data RunIndexerLog
  = -- | The last sync points of all indexers that will be used to start the chain-sync protocol.
    StartingPointsLog [C.ChainPoint]
  | NoIntersectionFoundLog

instance PP.Pretty RunIndexerLog where
  pretty (StartingPointsLog lastSyncPoints) =
    "Possible starting points for chain-sync protocol are" PP.<+> PP.pretty lastSyncPoints
  pretty NoIntersectionFoundLog = "No intersection found"

type instance Core.Point BlockEvent = C.ChainPoint

{- | Connect to the given socket to start a chain sync protocol and start indexing it with the
given indexer.

If you want to start several indexers, use @runIndexers@.
-}
runIndexer
  :: ( WithDistance BlockEvent ~ event
     , Core.IsIndex (ExceptT Core.IndexerError IO) event indexer
     , Core.Closeable IO indexer
     )
  => Trace IO Text
  -> SecurityParam
  -> RetryConfig
  -> FilePath
  -> C.NetworkId
  -> NonEmpty C.ChainPoint
  -> indexer (WithDistance BlockEvent)
  -> IO ()
runIndexer trace securityParam retryConfig socketPath networkId startingPoints indexer = do
  withNodeConnectRetry trace retryConfig socketPath $ do
    Trace.logInfo trace $
      PP.renderStrict $
        PP.layoutPretty PP.defaultLayoutOptions $
          PP.pretty $
            StartingPointsLog $
              NonEmpty.toList startingPoints
    eventQueue <- STM.newTBQueueIO $ fromIntegral securityParam
    cBox <- Concurrent.newMVar indexer
    let runChainSyncStream =
          withChainSyncBlockEventStream
            socketPath
            networkId
            (NonEmpty.toList startingPoints)
            (mkEventStream eventQueue . chainSyncEventStreamLogging trace)
        whenNoIntersectionFound NoIntersectionFound =
          Trace.logError trace $
            PP.renderStrict $
              PP.layoutPretty PP.defaultLayoutOptions $
                PP.pretty NoIntersectionFoundLog
    Async.concurrently_
      (void $ runChainSyncStream `catch` whenNoIntersectionFound)
      (Core.processQueue eventQueue cBox)

-- | Event preprocessing, to ease the coordinator work
mkEventStream
  :: STM.TBQueue (Core.ProcessedInput (WithDistance BlockEvent))
  -> S.Stream (S.Of (ChainSyncEvent BlockEvent)) IO r
  -> IO r
mkEventStream q =
  let blockTimed :: BlockEvent -> C.ChainTip -> Core.Timed C.ChainPoint (WithDistance BlockEvent)
      blockTimed
        (BlockEvent b@(C.BlockInMode block _) epochNo' bt)
        tip =
          let (C.Block (C.BlockHeader slotNo hsh currentBlockNo) _) = block
              blockWithDistance = Distance.attachDistance currentBlockNo tip (BlockEvent b epochNo' bt)
           in Core.Timed (C.ChainPoint slotNo hsh) blockWithDistance

      processEvent
        :: ChainSyncEvent BlockEvent
        -> Core.ProcessedInput (WithDistance BlockEvent)
      processEvent (RollForward x ct) = Core.Index $ Just <$> blockTimed x ct
      processEvent (RollBackward x _) = Core.Rollback x
   in S.mapM_ $ STM.atomically . STM.writeTBQueue q . processEvent
