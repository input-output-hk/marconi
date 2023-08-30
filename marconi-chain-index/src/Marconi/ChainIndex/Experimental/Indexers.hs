{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Experimental.Indexers where

import Cardano.Api qualified as C
import Cardano.BM.Tracing qualified as BM
import Cardano.Streaming (
  BlockEvent (BlockEvent),
 )
import Control.Concurrent (MVar)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (ExceptT, MonadError, MonadTrans (lift))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Marconi.ChainIndex.Experimental.Extract.WithDistance (WithDistance)
import Marconi.ChainIndex.Experimental.Indexers.BlockInfo qualified as Block
import Marconi.ChainIndex.Experimental.Indexers.Coordinator (coordinatorWorker, standardCoordinator)
import Marconi.ChainIndex.Experimental.Indexers.Datum qualified as Datum
import Marconi.ChainIndex.Experimental.Indexers.MintTokenEvent qualified as MintToken
import Marconi.ChainIndex.Experimental.Indexers.Spent qualified as Spent
import Marconi.ChainIndex.Experimental.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Experimental.Indexers.UtxoQuery qualified as UtxoQuery
import Marconi.ChainIndex.Experimental.Indexers.Worker (StandardWorkerConfig (StandardWorkerConfig))
import Marconi.ChainIndex.Types (TxIndexInBlock)
import Marconi.Core.Experiment qualified as Core
import System.FilePath ((</>))

data AnyTxBody = forall era. (C.IsCardanoEra era) => AnyTxBody TxIndexInBlock (C.TxBody era)
type instance Core.Point [AnyTxBody] = C.ChainPoint

{- | Build all the indexers of marconi-chain-index
(all those which are available with the new implementation)
and expose a single coordinator to operate them
-}
buildIndexers
  :: Core.CatchupConfig
  -> Utxo.UtxoIndexerConfig
  -> BM.Trace IO Text
  -> FilePath
  -> ExceptT
      Core.IndexerError
      IO
      -- Query part (should probably be wrapped in a more complex object later)
      ( UtxoQuery.UtxoQueryIndexer IO
      , -- Indexing part
        Core.WithTrace IO Core.Coordinator (WithDistance BlockEvent)
      )
buildIndexers catchupConfig utxoConfig logger path = do
  let mainLogger = BM.contramap (fmap (fmap $ Text.pack . show)) logger
      txBodyCoordinatorLogger = BM.appendName "txBody" mainLogger

  (blockInfoMVar, blockInfoWorker) <- blockInfoBuilder catchupConfig mainLogger path

  (utxoMVar, utxoWorker) <- utxoBuilder catchupConfig utxoConfig txBodyCoordinatorLogger path
  (spentMVar, spentWorker) <- spentBuilder catchupConfig txBodyCoordinatorLogger path
  (datumMVar, datumWorker) <- datumBuilder catchupConfig txBodyCoordinatorLogger path
  (_mintTokenMVar, mintTokenWorker) <- mintBuilder catchupConfig txBodyCoordinatorLogger path

  let getTxBody :: (C.IsCardanoEra era) => TxIndexInBlock -> C.Tx era -> AnyTxBody
      getTxBody ix tx = AnyTxBody ix (C.getTxBody tx)
      toTxBodys :: BlockEvent -> [AnyTxBody]
      toTxBodys (BlockEvent (C.BlockInMode (C.Block _ txs) _) _ _) = zipWith getTxBody [0 ..] txs

  coordinatorTxBodyWorkers <-
    buildTxBodyCoordinator
      txBodyCoordinatorLogger
      (pure . Just . fmap toTxBodys)
      [utxoWorker, spentWorker, datumWorker, mintTokenWorker]

  queryIndexer <-
    lift $
      UtxoQuery.mkUtxoSQLiteQuery $
        UtxoQuery.UtxoQueryAggregate utxoMVar spentMVar datumMVar blockInfoMVar

  mainCoordinator <-
    lift $ standardCoordinator mainLogger [blockInfoWorker, coordinatorTxBodyWorkers]

  pure (queryIndexer, mainCoordinator)

-- | Build and start a coordinator of a bunch of workers that takes an @AnyTxBody@ as an input
buildTxBodyCoordinator
  :: (MonadIO m, Ord (Core.Point event))
  => BM.Trace IO (Core.IndexerEvent (Core.Point event))
  -> (WithDistance input -> IO (Maybe event))
  -> [Core.Worker event (Core.Point event)]
  -> m (Core.Worker (WithDistance input) (Core.Point event))
buildTxBodyCoordinator logger extract workers =
  snd <$> coordinatorWorker "TxBody coordinator" logger extract workers

-- | Configure and start the @BlockInfo@ indexer
blockInfoBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => Core.CatchupConfig
  -> BM.Trace m (Core.IndexerEvent C.ChainPoint)
  -> FilePath
  -> n
      ( MVar (Block.StandardBlockInfoIndexer m)
      , Core.WorkerM m (WithDistance BlockEvent) (Core.Point Block.BlockInfo)
      )
blockInfoBuilder catchupConfig logger path =
  let extractBlockInfo :: BlockEvent -> Block.BlockInfo
      extractBlockInfo (BlockEvent (C.BlockInMode b _) eno t) = Block.fromBlockEratoBlockInfo b eno t
      blockInfoWorkerConfig =
        StandardWorkerConfig
          "BlockInfo"
          catchupConfig
          (pure . Just . extractBlockInfo)
          (BM.appendName "blockInfo" logger)
   in Block.blockInfoWorker blockInfoWorkerConfig (path </> "blockInfo.db")

-- | Configure and start the @Utxo@ indexer
utxoBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => Core.CatchupConfig
  -> Utxo.UtxoIndexerConfig
  -> BM.Trace m (Core.IndexerEvent C.ChainPoint)
  -> FilePath
  -> n
      ( MVar (Utxo.StandardUtxoIndexer m)
      , Core.WorkerM m (WithDistance [AnyTxBody]) (Core.Point Utxo.UtxoEvent)
      )
utxoBuilder catchupConfig utxoConfig logger path =
  let extractUtxos :: AnyTxBody -> [Utxo.Utxo]
      extractUtxos (AnyTxBody indexInBlock txb) = Utxo.getUtxosFromTxBody indexInBlock txb
      utxoWorkerConfig =
        StandardWorkerConfig
          "Utxo"
          catchupConfig
          (pure . NonEmpty.nonEmpty . (>>= extractUtxos))
          (BM.appendName "utxo" logger)
   in Utxo.utxoWorker utxoWorkerConfig utxoConfig (path </> "utxo.db")

-- | Configure and start the @SpentInfo@ indexer
spentBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => Core.CatchupConfig
  -> BM.Trace m (Core.IndexerEvent C.ChainPoint)
  -> FilePath
  -> n
      ( MVar (Spent.StandardSpentIndexer m)
      , Core.WorkerM m (WithDistance [AnyTxBody]) (Core.Point Spent.SpentInfoEvent)
      )
spentBuilder catchupConfig logger path =
  let extractSpent :: AnyTxBody -> [Spent.SpentInfo]
      extractSpent (AnyTxBody _ txb) = Spent.getInputs txb
      spentWorkerConfig =
        StandardWorkerConfig
          "spent"
          catchupConfig
          (pure . NonEmpty.nonEmpty . (>>= extractSpent))
          (BM.appendName "spent" logger)
   in Spent.spentWorker spentWorkerConfig (path </> "spent.db")

-- | Configure and start the @Datum@ indexer
datumBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => Core.CatchupConfig
  -> BM.Trace m (Core.IndexerEvent C.ChainPoint)
  -> FilePath
  -> n
      ( MVar (Datum.StandardDatumIndexer m)
      , Core.WorkerM m (WithDistance [AnyTxBody]) (Core.Point Datum.DatumEvent)
      )
datumBuilder catchupConfig logger path =
  let extractDatum :: AnyTxBody -> [Datum.DatumInfo]
      extractDatum (AnyTxBody _ txb) = Datum.getDataFromTxBody txb
      datumWorkerConfig =
        StandardWorkerConfig
          "datum"
          catchupConfig
          (pure . NonEmpty.nonEmpty . (>>= extractDatum))
          (BM.appendName "datum" logger)
   in Datum.datumWorker datumWorkerConfig (path </> "datum.db")

-- | Configure and start the @MintToken@ indexer
mintBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => Core.CatchupConfig
  -> BM.Trace m (Core.IndexerEvent C.ChainPoint)
  -> FilePath
  -> n
      ( MVar (MintToken.StandardMintTokenEventIndexer m)
      , Core.WorkerM m (WithDistance [AnyTxBody]) (Core.Point Datum.DatumEvent)
      )
mintBuilder catchupConfig logger path =
  let extractMint :: AnyTxBody -> [MintToken.MintTokenEvent]
      extractMint (AnyTxBody ix txb) = MintToken.extractEventsFromTx ix txb
      mintTokenWorkerConfig =
        StandardWorkerConfig
          "mintToken"
          catchupConfig
          (pure . fmap MintToken.MintTokenEvents . NonEmpty.nonEmpty . (>>= extractMint))
          (BM.appendName "mintToken" logger)
   in MintToken.mintTokenEventWorker mintTokenWorkerConfig (path </> "mint.db")
