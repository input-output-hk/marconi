{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Experimental.Indexers where

import Cardano.Api qualified as C
import Cardano.BM.Configuration.Static (defaultConfigStdout)
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.Streaming (
  ChainSyncEvent (RollBackward, RollForward),
  ChainSyncEventException (NoIntersectionFound),
  withChainSyncEventEpochNoStream,
 )
import Control.Concurrent (MVar, modifyMVar_, newMVar)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (TBQueue, atomically, newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Exception (catch)
import Control.Monad (forever)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Void (Void)
import Marconi.ChainIndex.Experimental.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Logging (chainSyncEventStreamLogging)
import Marconi.ChainIndex.Types (
  SecurityParam,
  UtxoIndexerConfig (UtxoIndexerConfig),
  ucEnableUtxoTxOutRef,
  ucTargetAddresses,
 )
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Core.Experiment qualified as Core
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Streaming.Prelude qualified as S

type instance Core.Point (C.BlockInMode C.CardanoMode, C.EpochNo, POSIXTime) = C.ChainPoint

type UtxoIndexer = Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer Utxo.UtxoEvent

-- | Extract the timed information from a block
blockTimed
  :: (C.BlockInMode C.CardanoMode, a, t)
  -> Core.Timed C.ChainPoint (C.BlockInMode C.CardanoMode, a, t)
blockTimed b@(C.BlockInMode (C.Block (C.BlockHeader slotNo hsh _) _) _, _, _) =
  Core.Timed (C.ChainPoint slotNo hsh) b

-- | Create a worker for the utxo indexer
utxoWorker -- Should go in Utxo module?
  :: FilePath
  -> SecurityParam
  -> IO (MVar UtxoIndexer, Core.Worker (C.BlockInMode C.CardanoMode, C.EpochNo, POSIXTime) C.ChainPoint)
utxoWorker dbPath depth = do
  c <- Utxo.initSQLite dbPath -- TODO handle error
  let utxoIndexerConfig =
        -- TODO We forgot the TargetAddress filtering logic for now for the Experimental Indexers.Utxo module
        UtxoIndexerConfig{ucTargetAddresses = Nothing, ucEnableUtxoTxOutRef = True}
      extract (C.BlockInMode block _, _, _) = Utxo.getUtxoEventsFromBlock utxoIndexerConfig block
  Core.createWorker (pure . extract) $ Utxo.mkMixedIndexer c depth

-- | Process the next event in the queue with the coordinator
readEvent
  :: TBQueue (Core.ProcessedInput (C.BlockInMode C.CardanoMode, C.EpochNo, POSIXTime))
  -> MVar (Core.Coordinator (C.BlockInMode C.CardanoMode, C.EpochNo, POSIXTime))
  -> IO r
readEvent q cBox = forever $ do
  e <- atomically $ readTBQueue q
  modifyMVar_ cBox $ \c -> Utils.toException $ Core.step c e

-- | Event preprocessing, to ease the coordinator work
mkEventStream
  :: TBQueue (Core.ProcessedInput (C.BlockInMode C.CardanoMode, C.EpochNo, POSIXTime))
  -> S.Stream (S.Of (ChainSyncEvent (C.BlockInMode C.CardanoMode, C.EpochNo, POSIXTime))) IO r
  -> IO r
mkEventStream q =
  let processEvent
        :: ChainSyncEvent (C.BlockInMode C.CardanoMode, C.EpochNo, POSIXTime)
        -> Core.ProcessedInput (C.BlockInMode C.CardanoMode, C.EpochNo, POSIXTime)
      processEvent (RollForward x _) = Core.Index $ Just <$> blockTimed x
      processEvent (RollBackward x _) = Core.Rollback x
   in S.mapM_ $ atomically . writeTBQueue q . processEvent

-- | Start the utxo indexer (the only one we have so far)
runIndexers
  :: FilePath
  -> C.NetworkId
  -> C.ChainPoint
  -> Text.Text
  -> [Core.Worker (C.BlockInMode C.CardanoMode, C.EpochNo, POSIXTime) C.ChainPoint]
  -- ^ base dir for indexers
  -> IO ()
runIndexers socketPath networkId _startingPoint traceName workers = do
  securityParam <- Utils.toException $ Utils.querySecurityParam @Void networkId socketPath
  eventQueue <- newTBQueueIO $ fromIntegral securityParam
  coordinator <- Core.mkCoordinator workers
  cBox <- newMVar coordinator
  c <- defaultConfigStdout
  concurrently_
    ( withTrace c traceName $ \trace ->
        let io =
              withChainSyncEventEpochNoStream
                socketPath
                networkId
                [Core.genesis]
                (mkEventStream eventQueue . chainSyncEventStreamLogging trace)
            handleException NoIntersectionFound =
              logError trace $
                renderStrict $
                  layoutPretty
                    defaultLayoutOptions
                    "No intersection found"
         in io `catch` handleException
    )
    (readEvent eventQueue cBox)
