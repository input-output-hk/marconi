{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Experimental.Indexers where

import Cardano.Api qualified as C
import Cardano.BM.Configuration.Static (defaultConfigStdout)
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.Streaming (
  ChainSyncEvent (RollBackward, RollForward),
  ChainSyncEventException (NoIntersectionFound),
  withChainSyncEventStream,
 )
import Control.Concurrent (MVar, modifyMVar_, newMVar)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (TBQueue, atomically, newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Exception (Exception, catch, throw)
import Control.Monad (forever)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Text qualified as Text
import Marconi.ChainIndex.Experimental.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Logging (logging)
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
import System.FilePath ((</>))

type instance Core.Point (C.BlockInMode C.CardanoMode) = C.ChainPoint

type UtxoIndexer = Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer Utxo.UtxoEvent

-- | Extract the timed information from a block
blockTimed
  :: C.BlockInMode C.CardanoMode
  -> Core.Timed C.ChainPoint (C.BlockInMode C.CardanoMode)
blockTimed b@(C.BlockInMode (C.Block (C.BlockHeader slotNo hsh _) _) _) =
  Core.Timed (C.ChainPoint slotNo hsh) b

-- | Create a worker for the utxo indexer
utxoWorker -- Should go in Utxo module?
  :: FilePath
  -> SecurityParam
  -> IO (MVar UtxoIndexer, Core.Worker (C.BlockInMode C.CardanoMode) C.ChainPoint)
utxoWorker dbPath depth = do
  c <- Utxo.initSQLite dbPath -- TODO handle error
  let utxoIndexerConfig =
        -- TODO We forgot the TargetAddress filtering logic for now for the Experimental Indexers.Utxo module
        UtxoIndexerConfig{ucTargetAddresses = Nothing, ucEnableUtxoTxOutRef = True}
      extract (C.BlockInMode block _) = Utxo.getUtxoEventsFromBlock utxoIndexerConfig block
  Core.createWorker (pure . extract) $ Utxo.mkMixedIndexer c depth

-- | Process the next event in the queue with the coordinator
readEvent
  :: TBQueue (Core.ProcessedInput (C.BlockInMode C.CardanoMode))
  -> MVar (Core.Coordinator (C.BlockInMode C.CardanoMode))
  -> IO r
readEvent q cBox = forever $ do
  e <- atomically $ readTBQueue q
  modifyMVar_ cBox $ \c -> toException $ Core.step c e

-- | Event preprocessing, to ease the coordinator work
mkEventStream
  :: TBQueue (Core.ProcessedInput (C.BlockInMode C.CardanoMode))
  -> S.Stream (S.Of (ChainSyncEvent (C.BlockInMode C.CardanoMode))) IO r
  -> IO r
mkEventStream q =
  let processEvent
        :: ChainSyncEvent (C.BlockInMode C.CardanoMode)
        -> Core.ProcessedInput (C.BlockInMode C.CardanoMode)
      processEvent (RollForward x _) = Core.Index $ blockTimed x
      processEvent (RollBackward x _) = Core.Rollback x
   in S.mapM_ $ atomically . writeTBQueue q . processEvent

-- | Start the utxo indexer (the only one we have so far)
runIndexers
  :: FilePath
  -> C.NetworkId
  -> C.ChainPoint
  -> Text.Text
  -> FilePath
  -- ^ base dir for indexers
  -> IO ()
runIndexers socketPath networkId _startingPoint traceName dbDir = do
  securityParam <- toException $ Utils.querySecurityParam networkId socketPath
  eventQueue <- newTBQueueIO $ fromIntegral securityParam
  (_, worker) <-
    utxoWorker
      (dbDir </> "utxo.db")
      securityParam
  coordinator <- Core.mkCoordinator [worker]
  cBox <- newMVar coordinator
  c <- defaultConfigStdout
  concurrently_
    ( withTrace c traceName $ \trace ->
        let io = withChainSyncEventStream socketPath networkId [Core.genesis] (mkEventStream eventQueue . logging trace)
            handleException NoIntersectionFound =
              logError trace $
                renderStrict $
                  layoutPretty
                    defaultLayoutOptions
                    "No intersection found"
         in io `catch` handleException
    )
    (readEvent eventQueue cBox)

toException :: Exception err => ExceptT err IO a -> IO a
toException mx = do
  x <- runExceptT mx
  case x of
    Left err -> throw err
    Right res -> pure res
