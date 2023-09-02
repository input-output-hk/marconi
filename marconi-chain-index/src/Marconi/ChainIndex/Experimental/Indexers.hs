{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Experimental.Indexers where

import Cardano.Api qualified as C
import Cardano.BM.Tracing qualified as BM
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (ExceptT, MonadError, MonadTrans (lift))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Marconi.ChainIndex.Experimental.Extract.WithDistance (WithDistance)
import Marconi.ChainIndex.Experimental.Indexers.BlockInfo qualified as Block
import Marconi.ChainIndex.Experimental.Indexers.Coordinator (coordinatorWorker, standardCoordinator)
import Marconi.ChainIndex.Experimental.Indexers.Datum qualified as Datum
import Marconi.ChainIndex.Experimental.Indexers.EpochState qualified as EpochState
import Marconi.ChainIndex.Experimental.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.ChainIndex.Experimental.Indexers.Spent qualified as Spent
import Marconi.ChainIndex.Experimental.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Experimental.Indexers.UtxoQuery qualified as UtxoQuery
import Marconi.ChainIndex.Experimental.Indexers.Worker (
  StandardWorker (StandardWorker),
  StandardWorkerConfig (StandardWorkerConfig),
 )
import Marconi.ChainIndex.Types (
  BlockEvent (BlockEvent),
  SecurityParam,
  TxIndexInBlock,
  blockInMode,
 )
import Marconi.Core.Experiment qualified as Core
import System.FilePath ((</>))

data AnyTxBody = forall era. (C.IsCardanoEra era) => AnyTxBody TxIndexInBlock (C.TxBody era)
type instance Core.Point [AnyTxBody] = C.ChainPoint

{- | Build all the indexers of marconi-chain-index
(all those which are available with the new implementation)
and expose a single coordinator to operate them
-}
buildIndexers
  :: SecurityParam
  -> Core.CatchupConfig
  -> Utxo.UtxoIndexerConfig
  -> MintTokenEvent.MintTokenEventConfig
  -> EpochState.EpochStateConfig
  -> BM.Trace IO Text
  -> FilePath
  -> ExceptT
      Core.IndexerError
      IO
      -- Query part (should probably be wrapped in a more complex object later)
      ( [C.ChainPoint]
      , UtxoQuery.UtxoQueryIndexer IO
      , -- Indexing part
        Core.WithTrace IO Core.Coordinator (WithDistance BlockEvent)
      )
buildIndexers securityParam catchupConfig utxoConfig mintEventConfig epochStateConfig logger path = do
  let mainLogger = BM.contramap (fmap (fmap $ Text.pack . show)) logger
      txBodyCoordinatorLogger = BM.appendName "txBody" mainLogger

  StandardWorker blockInfoMVar blockInfoWorker <-
    blockInfoBuilder securityParam catchupConfig mainLogger path

  StandardWorker _epochStateMVar epochStateWorker <-
    epochStateBuilder securityParam catchupConfig epochStateConfig mainLogger path

  StandardWorker utxoMVar utxoWorker <-
    utxoBuilder securityParam catchupConfig utxoConfig txBodyCoordinatorLogger path
  StandardWorker spentMVar spentWorker <-
    spentBuilder securityParam catchupConfig txBodyCoordinatorLogger path
  StandardWorker datumMVar datumWorker <-
    datumBuilder securityParam catchupConfig txBodyCoordinatorLogger path
  StandardWorker _mintTokenMVar mintTokenWorker <-
    mintBuilder securityParam catchupConfig mintEventConfig txBodyCoordinatorLogger path

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
    lift $
      standardCoordinator
        mainLogger
        [blockInfoWorker, epochStateWorker, coordinatorTxBodyWorkers]

  resumePoints <- Core.lastSyncPoints (fromIntegral securityParam + 1) mainCoordinator

  -- TODO Create a dedicated return type for it instead of a tuple.
  -- However, we should wait until we have more stuff in the query side before we do it.
  pure (resumePoints, queryIndexer, mainCoordinator)

-- | Build and start a coordinator of a bunch of workers that takes an @AnyTxBody@ as an input
buildTxBodyCoordinator
  :: (MonadIO m, Ord (Core.Point event))
  => BM.Trace IO (Core.IndexerEvent (Core.Point event))
  -> (WithDistance input -> IO (Maybe event))
  -> [Core.Worker event (Core.Point event)]
  -> m (Core.Worker (WithDistance input) (Core.Point event))
buildTxBodyCoordinator logger extract workers =
  Core.worker <$> coordinatorWorker "TxBody coordinator" logger extract workers

-- | Configure and start the @BlockInfo@ indexer
blockInfoBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => SecurityParam
  -> Core.CatchupConfig
  -> BM.Trace m (Core.IndexerEvent C.ChainPoint)
  -> FilePath
  -> n (StandardWorker m BlockEvent Block.BlockInfo Core.SQLiteIndexer)
blockInfoBuilder securityParam catchupConfig logger path =
  let extractBlockInfo :: BlockEvent -> Block.BlockInfo
      extractBlockInfo (BlockEvent (C.BlockInMode b _) eno t) = Block.fromBlockEratoBlockInfo b eno t
      blockInfoWorkerConfig =
        StandardWorkerConfig
          "BlockInfo"
          securityParam
          catchupConfig
          (pure . Just . extractBlockInfo)
          (BM.appendName "blockInfo" logger)
   in Block.blockInfoWorker blockInfoWorkerConfig (path </> "blockInfo.db")

-- | Configure and start the @Utxo@ indexer
utxoBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => SecurityParam
  -> Core.CatchupConfig
  -> Utxo.UtxoIndexerConfig
  -> BM.Trace m (Core.IndexerEvent C.ChainPoint)
  -> FilePath
  -> n (StandardWorker m [AnyTxBody] Utxo.UtxoEvent Core.SQLiteIndexer)
utxoBuilder securityParam catchupConfig utxoConfig logger path =
  let extractUtxos :: AnyTxBody -> [Utxo.Utxo]
      extractUtxos (AnyTxBody indexInBlock txb) = Utxo.getUtxosFromTxBody indexInBlock txb
      utxoWorkerConfig =
        StandardWorkerConfig
          "Utxo"
          securityParam
          catchupConfig
          (pure . NonEmpty.nonEmpty . (>>= extractUtxos))
          (BM.appendName "utxo" logger)
   in Utxo.utxoWorker utxoWorkerConfig utxoConfig (path </> "utxo.db")

-- | Configure and start the @SpentInfo@ indexer
spentBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => SecurityParam
  -> Core.CatchupConfig
  -> BM.Trace m (Core.IndexerEvent C.ChainPoint)
  -> FilePath
  -> n (StandardWorker m [AnyTxBody] Spent.SpentInfoEvent Core.SQLiteIndexer)
spentBuilder securityParam catchupConfig logger path =
  let extractSpent :: AnyTxBody -> [Spent.SpentInfo]
      extractSpent (AnyTxBody _ txb) = Spent.getInputs txb
      spentWorkerConfig =
        StandardWorkerConfig
          "spent"
          securityParam
          catchupConfig
          (pure . NonEmpty.nonEmpty . (>>= extractSpent))
          (BM.appendName "spent" logger)
   in Spent.spentWorker spentWorkerConfig (path </> "spent.db")

-- | Configure and start the @Datum@ indexer
datumBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => SecurityParam
  -> Core.CatchupConfig
  -> BM.Trace m (Core.IndexerEvent C.ChainPoint)
  -> FilePath
  -> n (StandardWorker m [AnyTxBody] Datum.DatumEvent Core.SQLiteIndexer)
datumBuilder securityParam catchupConfig logger path =
  let extractDatum :: AnyTxBody -> [Datum.DatumInfo]
      extractDatum (AnyTxBody _ txb) = Datum.getDataFromTxBody txb
      datumWorkerConfig =
        StandardWorkerConfig
          "datum"
          securityParam
          catchupConfig
          (pure . NonEmpty.nonEmpty . (>>= extractDatum))
          (BM.appendName "datum" logger)
   in Datum.datumWorker datumWorkerConfig (path </> "datum.db")

-- | Configure and start the @MintToken@ indexer
mintBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => SecurityParam
  -> Core.CatchupConfig
  -> MintTokenEvent.MintTokenEventConfig
  -> BM.Trace m (Core.IndexerEvent C.ChainPoint)
  -> FilePath
  -> n (StandardWorker m [AnyTxBody] MintTokenEvent.MintTokenBlockEvents Core.SQLiteIndexer)
mintBuilder securityParam catchupConfig mintEventConfig logger path =
  let extractMint :: AnyTxBody -> [MintTokenEvent.MintTokenEvent]
      extractMint (AnyTxBody ix txb) = MintTokenEvent.extractEventsFromTx ix txb
      mintTokenWorkerConfig =
        StandardWorkerConfig
          "mintToken"
          securityParam
          catchupConfig
          (pure . fmap MintTokenEvent.MintTokenBlockEvents . NonEmpty.nonEmpty . (>>= extractMint))
          (BM.appendName "mintToken" logger)
   in MintTokenEvent.mkMintTokenEventWorker mintTokenWorkerConfig mintEventConfig (path </> "mint.db")

-- | Configure and start the @EpochState@ indexer
epochStateBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => SecurityParam
  -> Core.CatchupConfig
  -> EpochState.EpochStateConfig
  -> BM.Trace m (Core.IndexerEvent C.ChainPoint)
  -> FilePath
  -> n (StandardWorker m BlockEvent (C.BlockInMode C.CardanoMode) EpochState.EpochStateIndexer)
epochStateBuilder securityParam catchupConfig epochStateConfig logger path =
  let epochStateWorkerConfig =
        StandardWorkerConfig
          "epochState"
          securityParam
          catchupConfig
          (pure . Just . blockInMode)
          (BM.appendName "epochState" logger)
   in EpochState.mkEpochStateWorker epochStateWorkerConfig epochStateConfig (path </> "epochState")
