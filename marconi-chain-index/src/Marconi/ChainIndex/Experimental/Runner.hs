{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Allow the execution of indexers on a Cardano node using the chain sync protocol
module Marconi.ChainIndex.Experimental.Runner (
  runIndexer,
  runIndexers,
) where

import Cardano.Api qualified as C
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace qualified as Trace
import Cardano.BM.Tracing qualified as Tracing
import Cardano.Streaming (
  BlockEvent (BlockEvent),
  ChainSyncEvent (RollBackward, RollForward),
  ChainSyncEventException (NoIntersectionFound),
  withChainSyncEventEpochNoStream,
 )

import Control.Concurrent qualified as Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Exception (catch, throwIO)
import Control.Monad (forever)
import Control.Monad.Except (ExceptT, runExceptT)

import Data.Text qualified as Text
import Data.Void (Void)

import Marconi.ChainIndex.Experimental.Extract.WithDistance (WithDistance)
import Marconi.ChainIndex.Logging (chainSyncEventStreamLogging)
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Core.Experiment qualified as Core

import Marconi.ChainIndex.Experimental.Extract.WithDistance qualified as Distance
import Marconi.ChainIndex.Experimental.Indexers.Orphans qualified ()
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Text qualified as Pretty
import Streaming qualified as S
import Streaming.Prelude qualified as S

type instance Core.Point BlockEvent = C.ChainPoint

{- | Connect to the given socket to start a chain sync protocol and start indexing it with the
given indexer.

If you want to start several indexers, user @runIndexers@.
-}
runIndexer
  :: (Core.IsIndex (ExceptT Core.IndexerError IO) (WithDistance BlockEvent) indexer)
  => FilePath
  -> C.NetworkId
  -> C.ChainPoint
  -> Text.Text
  -> indexer (WithDistance BlockEvent)
  -> IO ()
runIndexer socketPath networkId _startingPoint traceName indexer = do
  securityParam <- Utils.toException $ Utils.querySecurityParam @Void networkId socketPath
  eventQueue <- STM.newTBQueueIO $ fromIntegral securityParam
  cBox <- Concurrent.newMVar indexer
  c <- Tracing.defaultConfigStdout
  Async.concurrently_
    ( withTrace c traceName $ \trace ->
        let io =
              withChainSyncEventEpochNoStream
                socketPath
                networkId
                [Core.genesis]
                (mkEventStream eventQueue . chainSyncEventStreamLogging trace)
            handleException NoIntersectionFound =
              Trace.logError trace $
                Pretty.renderStrict $
                  Pretty.layoutPretty
                    Pretty.defaultLayoutOptions
                    "No intersection found"
         in io `catch` handleException
    )
    (readEvent eventQueue cBox)

-- | Run several indexers under a unique coordinator
runIndexers
  :: FilePath
  -> C.NetworkId
  -> C.ChainPoint
  -> Text.Text
  -> [Core.Worker (WithDistance BlockEvent) C.ChainPoint]
  -> IO ()
runIndexers socketPath networkId _startingPoint traceName indexers = do
  coordinator <- Core.mkCoordinator indexers
  runIndexer socketPath networkId _startingPoint traceName coordinator

-- | Process the next event in the queue with the coordinator
readEvent
  :: ( Ord (Core.Point event)
     , Core.IsIndex (ExceptT Core.IndexerError IO) event indexer
     )
  => STM.TBQueue (Core.ProcessedInput event)
  -> Concurrent.MVar (indexer event)
  -> IO r
readEvent q cBox = forever $ do
  e <- STM.atomically $ STM.readTBQueue q
  Concurrent.modifyMVar_ cBox $ \c -> do
    mres <- runExceptT (Core.step c e)
    case mres of
      Left (err :: Core.IndexerError) -> throwIO err
      Right res -> pure res

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
