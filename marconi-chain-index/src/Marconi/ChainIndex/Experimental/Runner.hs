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
import Cardano.BM.Trace qualified as Trace
import Control.Concurrent qualified as Concurrent
import Control.Concurrent.STM qualified as STM
import Control.Exception (catch)
import Control.Monad.Except (ExceptT, void)

import Control.Monad.State.Strict (MonadState (put), State, gets)
import Data.Map (Map)
import Data.Map qualified as Map
import Marconi.ChainIndex.Experimental.Extract.WithDistance (WithDistance, chainDistance, getEvent)
import Marconi.ChainIndex.Experimental.Extract.WithDistance qualified as Distance
import Marconi.ChainIndex.Experimental.Indexers.Orphans qualified ()
import Marconi.ChainIndex.Logging (chainSyncEventStreamLogging)
import Marconi.ChainIndex.Node.Client.Retry (withNodeConnectRetry)
import Marconi.ChainIndex.Types (
  BlockEvent (blockInMode),
  RunIndexerConfig (RunIndexerConfig),
  SecurityParam,
 )
import Marconi.Core.Experiment qualified as Core
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Streaming qualified as S
import Streaming.Prelude qualified as S

-- | Wraps as a datatype log message emitted by the 'runIndexer' et al. functions.
data RunIndexerLog
  = -- | The last sync points of all indexers that will be used to start the chain-sync protocol.
    StartingPointLog C.ChainPoint
  | NoIntersectionFoundLog

instance PP.Pretty RunIndexerLog where
  pretty (StartingPointLog lastSyncPoint) =
    "The starting point for the chain-sync protocol is" PP.<+> PP.pretty lastSyncPoint
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
  => RunIndexerConfig
  -> indexer (WithDistance BlockEvent)
  -> IO ()
runIndexer
  ( RunIndexerConfig
      trace
      retryConfig
      securityParam
      networkId
      startingPoint
      socketPath
    )
  indexer = do
    withNodeConnectRetry trace retryConfig socketPath $ do
      Trace.logInfo trace $
        PP.renderStrict $
          PP.layoutPretty PP.defaultLayoutOptions $
            PP.pretty $
              StartingPointLog startingPoint
      eventQueue <- STM.newTBQueueIO $ fromIntegral securityParam
      cBox <- Concurrent.newMVar indexer
      let runChainSyncStream =
            withChainSyncBlockEventStream
              socketPath
              networkId
              [startingPoint]
              (mkEventStream eventQueue . chainSyncEventStreamLogging trace)
          whenNoIntersectionFound NoIntersectionFound =
            Trace.logError trace $
              PP.renderStrict $
                PP.layoutPretty PP.defaultLayoutOptions $
                  PP.pretty NoIntersectionFoundLog
      void $ Concurrent.forkIO $ runChainSyncStream `catch` whenNoIntersectionFound
      Core.processQueue (stablePointComputation securityParam) Map.empty eventQueue cBox

stablePointComputation
  :: SecurityParam
  -> Core.Timed C.ChainPoint (Maybe (WithDistance BlockEvent))
  -> State (Map C.BlockNo C.ChainPoint) (Maybe C.ChainPoint)
stablePointComputation _s (Core.Timed _ Nothing) = pure Nothing
stablePointComputation s (Core.Timed point (Just event)) =
  if chainDistance event > fromIntegral s
    then do
      put mempty
      pure $ Just point
    else do
      let currentBlock = getBlockNo . blockInMode . getEvent $ event
          lastVolatileBlock = currentBlock + fromIntegral (chainDistance event) - fromIntegral s
      (immutable, volatile) <- gets (Map.spanAntitone (< lastVolatileBlock))
      put (Map.insert currentBlock point volatile)
      pure $ case Map.elems immutable of
        [] -> Nothing
        xs -> Just $ last xs

getBlockNo :: C.BlockInMode C.CardanoMode -> C.BlockNo
getBlockNo (C.BlockInMode block _eraInMode) =
  case C.getBlockHeader block of C.BlockHeader _ _ b -> b

-- | Event preprocessing, to ease the coordinator work
mkEventStream
  :: STM.TBQueue (Core.ProcessedInput C.ChainPoint (WithDistance BlockEvent))
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
        -> Core.ProcessedInput C.ChainPoint (WithDistance BlockEvent)
      processEvent (RollForward x ct) = Core.Index $ Just <$> blockTimed x ct
      processEvent (RollBackward x _) = Core.Rollback x
   in S.mapM_ $ STM.atomically . STM.writeTBQueue q . processEvent
