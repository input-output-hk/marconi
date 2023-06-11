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
import Control.Concurrent (MVar)
import Control.Exception (Exception, catch, throw)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Text qualified as Text
import Marconi.ChainIndex.Experimental.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Logging (logging)
import Marconi.ChainIndex.Types (SecurityParam)
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Core.Experiment qualified as Core
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Streaming.Prelude qualified as S

type instance Core.Point (C.BlockInMode C.CardanoMode) = C.ChainPoint

type UtxoIndexer = Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer Utxo.UtxoEvent

-- | Extract the timed information from a block
blockTimedEvent
  :: C.BlockInMode C.CardanoMode
  -> Core.TimedEvent (C.BlockInMode C.CardanoMode)
blockTimedEvent b@(C.BlockInMode (C.Block (C.BlockHeader slotNo hsh _) _) _) =
  Core.TimedEvent (C.ChainPoint slotNo hsh) b

utxoWorker
  :: SecurityParam
  -> FilePath
  -> IO (MVar UtxoIndexer, Core.Worker (C.BlockInMode C.CardanoMode) C.ChainPoint)
utxoWorker depth dbPath = do
  c <- Utxo.initSQLite dbPath -- TODO handle error
  let extract (C.BlockInMode block _) = Utxo.getUtxoEventsFromBlock Nothing block
  Core.createWorker (pure . extract) $ Utxo.mkMixedIndexer c (3 * depth)

mkIndexerStream
  :: Core.Coordinator (C.BlockInMode C.CardanoMode)
  -> S.Stream (S.Of (ChainSyncEvent (C.BlockInMode C.CardanoMode))) IO r
  -> IO r
mkIndexerStream c =
  let processEvent
        :: ChainSyncEvent (C.BlockInMode C.CardanoMode)
        -> Core.ProcessedInput (C.BlockInMode C.CardanoMode)
      processEvent (RollForward x _) = Core.Index $ blockTimedEvent x
      processEvent (RollBackward x _) = Core.Rollback x
   in S.mapM_ $ toException . Core.step c . processEvent

runIndexers
  :: FilePath
  -> C.NetworkId
  -> C.ChainPoint
  -> Text.Text
  -> IO ()
runIndexers socketPath networkId _startingPoint traceName = do
  securityParam <- toException $ Utils.querySecurityParam networkId socketPath
  (_, worker) <-
    utxoWorker
      securityParam
      "/Users/nicolasbiri/IOG/marco/marconi-experimental/utxo.db"
  coordinator <- Core.start [worker]
  c <- defaultConfigStdout
  withTrace c traceName $ \trace ->
    let io = withChainSyncEventStream socketPath networkId [Core.genesis] (mkIndexerStream coordinator . logging trace)
        handleException NoIntersectionFound =
          logError trace $
            renderStrict $
              layoutPretty
                defaultLayoutOptions
                "No intersection found"
     in io `catch` handleException

toException :: Exception err => ExceptT err IO a -> IO a
toException mx = do
  x <- runExceptT mx
  case x of
    Left err -> throw err
    Right res -> pure res
