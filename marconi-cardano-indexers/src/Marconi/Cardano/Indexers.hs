{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Marconi.Cardano.Indexers where

import Cardano.Api.Extended qualified as C
import Cardano.BM.Tracing qualified as BM
import Control.Arrow ((<<<))
import Control.Concurrent (MVar)
import Control.Lens (makeLenses, (&), (?~))
import Control.Lens qualified as Lens
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (ExceptT, MonadError, MonadTrans (lift))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance)
import Marconi.Cardano.Core.Extract.WithDistance qualified as WithDistance
import Marconi.Cardano.Core.Indexer.Worker (
  StandardIndexer,
  StandardWorker (StandardWorker),
  StandardWorkerConfig (StandardWorkerConfig, eventExtractor, logger, workerName),
 )
import Marconi.Cardano.Core.Transformer.WithSyncStats (WithSyncStats)
import Marconi.Cardano.Core.Types (
  BlockEvent (BlockEvent),
  BlockRange,
  MarconiTrace,
  SecurityParam,
  TipAndBlock (TipAndBlock),
  TxIndexInBlock,
  blockRangeFst,
 )
import Marconi.Cardano.Indexers.BlockInfo qualified as BlockInfo
import Marconi.Cardano.Indexers.ChainTip qualified as ChainTip
import Marconi.Cardano.Indexers.Coordinator (coordinatorWorker, syncStatsCoordinator)
import Marconi.Cardano.Indexers.CurrentSyncPointQuery qualified as CurrentSyncPoint
import Marconi.Cardano.Indexers.Datum qualified as Datum
import Marconi.Cardano.Indexers.EpochNonce qualified as Nonce
import Marconi.Cardano.Indexers.EpochSDD qualified as SDD
import Marconi.Cardano.Indexers.ExtLedgerStateCoordinator (
  EpochMetadata,
  ExtLedgerStateEvent,
  buildExtLedgerStateEventIndexer,
 )
import Marconi.Cardano.Indexers.ExtLedgerStateCoordinator qualified as ExtLedgerStateCoordinator
import Marconi.Cardano.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.Cardano.Indexers.MintTokenEventQuery (
  MintTokenEventIndexerQuery (MintTokenEventIndexerQuery),
 )
import Marconi.Cardano.Indexers.SnapshotBlockEvent (
  SnapshotBlockEvent (SnapshotBlockEvent),
  SnapshotMetadata,
  SnapshotWorkerConfig (SnapshotWorkerConfig, blockRange, currentBlockNo),
  getConfigCodec,
 )
import Marconi.Cardano.Indexers.SnapshotBlockEvent qualified as SnapshotBlockEvent
import Marconi.Cardano.Indexers.Spent qualified as Spent
import Marconi.Cardano.Indexers.Utxo qualified as Utxo
import Marconi.Cardano.Indexers.UtxoQuery qualified as UtxoQuery
import Marconi.Core qualified as Core
import Marconi.Core.Preprocessor qualified as Core
import System.FilePath ((</>))

data AnyTxBody = forall era. (C.IsCardanoEra era) => AnyTxBody C.BlockNo TxIndexInBlock (C.TxBody era)
type instance
  Core.Point (ExtLedgerStateCoordinator.ExtLedgerStateEvent, WithDistance BlockEvent) =
    C.ChainPoint
type instance Core.Point (Either C.ChainTip (WithDistance BlockEvent)) = C.ChainPoint
type instance Core.Point BlockEvent = C.ChainPoint
type instance Core.Point C.ChainTip = C.ChainPoint
type instance Core.Point [AnyTxBody] = C.ChainPoint

-- Convenience aliases for indexers
type Coordinator = Core.WithTrace IO Core.Coordinator TipAndBlock
type SyncStatsCoordinator = WithSyncStats (Core.WithTrace IO Core.Coordinator) TipAndBlock

type ChainTipIndexer = ChainTip.ChainTipIndexer IO
type MintTokenEventIndexer =
  StandardIndexer IO Core.SQLiteIndexer MintTokenEvent.MintTokenBlockEvents
type BlockInfoIndexer = StandardIndexer IO Core.SQLiteIndexer BlockInfo.BlockInfo
type UtxoIndexer = UtxoQuery.UtxoQueryIndexer IO
type CurrentSyncPointIndexer =
  Core.WithTrace IO CurrentSyncPoint.CurrentSyncPointQueryIndexer TipAndBlock
type EpochNonceIndexer = Core.WithTrace IO Core.SQLiteIndexer Nonce.EpochNonce
type EpochSDDIndexer = Core.WithTrace IO Core.SQLiteIndexer (NonEmpty SDD.EpochSDD)

-- | Container for all the queryable indexers of marconi-chain-index.
data MarconiCardanoQueryables = MarconiCardanoQueryables
  { _queryableEpochNonce :: !(MVar EpochNonceIndexer)
  , _queryableEpochSDD :: !(MVar EpochSDDIndexer)
  , _queryableMintToken :: !(MintTokenEventIndexerQuery MintTokenEvent.MintTokenBlockEvents)
  , _queryableUtxo :: !UtxoIndexer
  , _queryableCurrentSyncPoint :: !CurrentSyncPointIndexer
  }

makeLenses 'MarconiCardanoQueryables

{- | Build all the indexers of marconi-chain-index
and expose a single coordinator to operate them
-}
buildIndexers
  :: SecurityParam
  -> Core.CatchupConfig
  -> Utxo.UtxoIndexerConfig
  -> MintTokenEvent.MintTokenEventConfig
  -> ExtLedgerStateCoordinator.ExtLedgerStateWorkerConfig IO (WithDistance BlockEvent)
  -> BM.Trace IO Text
  -> MarconiTrace IO
  -> FilePath
  -> ExceptT
      Core.IndexerError
      IO
      ( C.ChainPoint
      , MarconiCardanoQueryables
      , SyncStatsCoordinator
      )
buildIndexers
  securityParam
  catchupConfig
  utxoConfig
  mintEventConfig
  epochStateConfig
  textLogger
  prettyLogger
  path = do
    let mainLogger :: BM.Trace IO (Core.IndexerEvent C.ChainPoint)
        mainLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
        blockEventTextLogger = BM.appendName "blockEvent" textLogger
        blockEventLogger = BM.appendName "blockEvent" mainLogger
        txBodyCoordinatorLogger = BM.appendName "txBody" blockEventTextLogger
        epochStateTextLogger = BM.appendName "epochState" blockEventTextLogger
        epochSDDTextLogger = BM.appendName "epochSDD" epochStateTextLogger
        epochNonceTextLogger = BM.appendName "epochNonce" epochStateTextLogger

    StandardWorker blockInfoMVar blockInfoWorker <-
      blockInfoBuilder securityParam catchupConfig blockEventTextLogger path

    Core.WorkerIndexer epochSDDMVar epochSDDWorker <-
      epochSDDBuilder securityParam catchupConfig epochSDDTextLogger path
    Core.WorkerIndexer epochNonceMVar epochNonceWorker <-
      epochNonceBuilder securityParam catchupConfig epochNonceTextLogger path
    Core.WorkerIndexer _epochStateMVar epochStateWorker <-
      ExtLedgerStateCoordinator.extLedgerStateWorker
        epochStateConfig
        [epochSDDWorker, epochNonceWorker]
        path

    StandardWorker utxoMVar utxoWorker <-
      utxoBuilder securityParam catchupConfig utxoConfig txBodyCoordinatorLogger path
    StandardWorker spentMVar spentWorker <-
      spentBuilder securityParam catchupConfig txBodyCoordinatorLogger path
    StandardWorker datumMVar datumWorker <-
      datumBuilder securityParam catchupConfig txBodyCoordinatorLogger path
    StandardWorker mintTokenMVar mintTokenWorker <-
      mintBuilder securityParam catchupConfig mintEventConfig txBodyCoordinatorLogger path

    let getTxBody :: (C.IsCardanoEra era) => C.BlockNo -> TxIndexInBlock -> C.Tx era -> AnyTxBody
        getTxBody blockNo ix tx = AnyTxBody blockNo ix (C.getTxBody tx)
        toTxBodys :: BlockEvent -> [AnyTxBody]
        toTxBodys (BlockEvent (C.BlockInMode (C.Block (C.BlockHeader _ _ bn) txs) _) _ _) =
          zipWith (getTxBody bn) [0 ..] txs

    coordinatorTxBodyWorkers <-
      buildTxBodyCoordinator
        txBodyCoordinatorLogger
        (pure . Just . fmap toTxBodys)
        [utxoWorker, spentWorker, datumWorker, mintTokenWorker]

    utxoQueryIndexer <-
      Core.withTrace (BM.appendName "utxoQueryEvent" mainLogger)
        <$> ( lift $
                UtxoQuery.mkUtxoSQLiteQuery $
                  UtxoQuery.UtxoQueryAggregate utxoMVar spentMVar datumMVar blockInfoMVar
            )

    blockCoordinator <-
      lift $
        buildBlockEventCoordinator
          blockEventLogger
          [blockInfoWorker, epochStateWorker, coordinatorTxBodyWorkers]

    Core.WorkerIndexer chainTipMVar chainTipWorker <- chainTipBuilder mainLogger path

    mainCoordinator <-
      lift $
        syncStatsCoordinator
          mainLogger
          prettyLogger
          [blockCoordinator, chainTipWorker]

    let currentSyncPointIndexer =
          Core.withTrace (BM.appendName "currentSyncPointEvent" mainLogger) $
            CurrentSyncPoint.CurrentSyncPointQueryIndexer
              mainCoordinator
              blockInfoMVar
              chainTipMVar
        queryables =
          MarconiCardanoQueryables
            epochNonceMVar
            epochSDDMVar
            (MintTokenEventIndexerQuery securityParam mintTokenMVar blockInfoMVar)
            utxoQueryIndexer
            currentSyncPointIndexer

    resumePoint <- Core.lastStablePoint mainCoordinator

    pure (resumePoint, queryables, mainCoordinator)

-- | Build and start a coordinator of a bunch of workers that takes an @AnyTxBody@ as an input
buildBlockEventCoordinator
  :: (MonadIO m)
  => BM.Trace IO (Core.IndexerEvent C.ChainPoint)
  -> [Core.Worker (WithDistance BlockEvent) C.ChainPoint]
  -> m (Core.Worker TipAndBlock C.ChainPoint)
buildBlockEventCoordinator logger' workers =
  let rightToMaybe = \case
        TipAndBlock _ block -> block
   in Core.worker <$> coordinatorWorker "BlockEvent coordinator" logger' (pure . rightToMaybe) workers

-- | Build and start a coordinator of a bunch of workers that takes an @AnyTxBody@ as an input
buildTxBodyCoordinator
  :: (MonadIO m, Ord (Core.Point event), Show (Core.Point event))
  => BM.Trace IO Text
  -> (WithDistance input -> IO (Maybe event))
  -> [Core.Worker event (Core.Point event)]
  -> m (Core.Worker (WithDistance input) (Core.Point event))
buildTxBodyCoordinator textLogger extract workers = do
  let indexerEventLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
  Core.worker <$> coordinatorWorker "TxBody coordinator" indexerEventLogger extract workers

-- | Configure and start the @BlockInfo@ indexer
blockInfoBuilder
  :: (MonadIO n, MonadError Core.IndexerError n)
  => SecurityParam
  -> Core.CatchupConfig
  -> BM.Trace IO Text
  -> FilePath
  -> n (StandardWorker IO BlockEvent BlockInfo.BlockInfo Core.SQLiteIndexer)
blockInfoBuilder securityParam catchupConfig textLogger path =
  let indexerName = "BlockInfo"
      indexerEventLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
      blockInfoDbPath = path </> BlockInfo.dbName
      catchupConfigWithTracer =
        catchupConfig
          & Core.configCatchupEventHook
            ?~ BlockInfo.catchupConfigEventHook indexerName textLogger blockInfoDbPath
      extractBlockInfo :: BlockEvent -> BlockInfo.BlockInfo
      extractBlockInfo (BlockEvent (C.BlockInMode b _) eno t) = BlockInfo.fromBlockEratoBlockInfo b eno t
      blockInfoWorkerConfig =
        StandardWorkerConfig
          indexerName
          securityParam
          catchupConfigWithTracer
          (pure . Just . extractBlockInfo)
          (BM.appendName indexerName indexerEventLogger)
   in BlockInfo.blockInfoWorker blockInfoWorkerConfig (path </> BlockInfo.dbName)

-- | Configure and start the @Utxo@ indexer
utxoBuilder
  :: (MonadIO n, MonadError Core.IndexerError n)
  => SecurityParam
  -> Core.CatchupConfig
  -> Utxo.UtxoIndexerConfig
  -> BM.Trace IO Text
  -> FilePath
  -> n (StandardWorker IO [AnyTxBody] Utxo.UtxoEvent Core.SQLiteIndexer)
utxoBuilder securityParam catchupConfig utxoConfig textLogger path =
  let indexerName = "Utxo"
      indexerEventLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
      utxoDbPath = path </> "utxo.db"
      extractUtxos :: AnyTxBody -> [Utxo.Utxo]
      extractUtxos (AnyTxBody _ indexInBlock txb) = Utxo.getUtxosFromTxBody indexInBlock txb
      catchupConfigWithTracer =
        catchupConfig
          & Core.configCatchupEventHook ?~ Utxo.catchupConfigEventHook textLogger utxoDbPath
      utxoWorkerConfig =
        StandardWorkerConfig
          indexerName
          securityParam
          catchupConfigWithTracer
          (pure . NonEmpty.nonEmpty . (>>= extractUtxos))
          (BM.appendName indexerName indexerEventLogger)
   in Utxo.utxoWorker utxoWorkerConfig utxoConfig utxoDbPath

-- | Configure and start the 'ChainTip' indexer
chainTipBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => BM.Trace m (Core.IndexerEvent C.ChainPoint)
  -> FilePath
  -> n
      ( Core.WorkerIndexer
          m
          TipAndBlock
          C.ChainTip
          (Core.WithTrace m Core.LastEventIndexer)
      )
chainTipBuilder tracer path = do
  let chainTipPath = path </> "chainTip"
      tipOnly (TipAndBlock tip _) = tip
  ChainTip.chainTipWorker tracer tipOnly (ChainTip.ChainTipConfig chainTipPath 2048)

-- | Configure and start the @SpentInfo@ indexer
spentBuilder
  :: (MonadIO n, MonadError Core.IndexerError n)
  => SecurityParam
  -> Core.CatchupConfig
  -> BM.Trace IO Text
  -> FilePath
  -> n (StandardWorker IO [AnyTxBody] Spent.SpentInfoEvent Core.SQLiteIndexer)
spentBuilder securityParam catchupConfig textLogger path =
  let indexerName = "Spent"
      indexerEventLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
      spentDbPath = path </> "spent.db"
      extractSpent :: AnyTxBody -> [Spent.SpentInfo]
      extractSpent (AnyTxBody _ _ txb) = Spent.getInputs txb
      catchupConfigWithTracer =
        catchupConfig
          & Core.configCatchupEventHook ?~ Spent.catchupConfigEventHook textLogger spentDbPath
      spentWorkerConfig =
        StandardWorkerConfig
          indexerName
          securityParam
          catchupConfigWithTracer
          (pure . NonEmpty.nonEmpty . (>>= extractSpent))
          (BM.appendName indexerName indexerEventLogger)
   in Spent.spentWorker spentWorkerConfig spentDbPath

-- | Configure and start the @Datum@ indexer
datumBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => SecurityParam
  -> Core.CatchupConfig
  -> BM.Trace m Text
  -> FilePath
  -> n (StandardWorker m [AnyTxBody] Datum.DatumEvent Core.SQLiteIndexer)
datumBuilder securityParam catchupConfig textLogger path =
  let indexerName = "Datum"
      indexerEventLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
      extractDatum :: AnyTxBody -> [Datum.DatumInfo]
      extractDatum (AnyTxBody _ _ txb) = Datum.getDataFromTxBody txb
      datumWorkerConfig =
        StandardWorkerConfig
          indexerName
          securityParam
          catchupConfig
          (pure . NonEmpty.nonEmpty . (>>= extractDatum))
          (BM.appendName indexerName indexerEventLogger)
   in Datum.datumWorker datumWorkerConfig (path </> "datum.db")

-- | Configure and start the @MintToken@ indexer
mintBuilder
  :: (MonadIO n, MonadError Core.IndexerError n)
  => SecurityParam
  -> Core.CatchupConfig
  -> MintTokenEvent.MintTokenEventConfig
  -> BM.Trace IO Text
  -> FilePath
  -> n (StandardWorker IO [AnyTxBody] MintTokenEvent.MintTokenBlockEvents Core.SQLiteIndexer)
mintBuilder securityParam catchupConfig mintEventConfig textLogger path =
  let indexerName = "MintTokenEvent"
      indexerEventLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
      mintDbPath = path </> "mint.db"
      catchupConfigWithTracer =
        catchupConfig
          & Core.configCatchupEventHook
            ?~ MintTokenEvent.catchupConfigEventHook indexerName textLogger mintDbPath
      extractMint :: AnyTxBody -> [MintTokenEvent.MintTokenEvent]
      extractMint (AnyTxBody bn ix txb) = MintTokenEvent.extractEventsFromTx bn ix txb
      mintTokenWorkerConfig =
        StandardWorkerConfig
          indexerName
          securityParam
          catchupConfigWithTracer
          (pure . fmap MintTokenEvent.MintTokenBlockEvents . NonEmpty.nonEmpty . (>>= extractMint))
          (BM.appendName indexerName indexerEventLogger)
   in MintTokenEvent.mkMintTokenEventWorker mintTokenWorkerConfig mintEventConfig mintDbPath

-- | Configure and start the @EpochNonce@ indexer
epochNonceBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => SecurityParam
  -> Core.CatchupConfig
  -> BM.Trace m Text
  -> FilePath
  -> n
      ( Core.WorkerIndexer
          m
          (ExtLedgerStateCoordinator.ExtLedgerStateEvent, WithDistance BlockEvent)
          Nonce.EpochNonce
          (Core.WithTrace m Core.SQLiteIndexer)
      )
epochNonceBuilder securityParam catchupConfig textLogger path =
  let indexerName = "EpochNonce"
      indexerEventLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
      epochNonceWorkerConfig =
        StandardWorkerConfig
          indexerName
          securityParam
          catchupConfig
          (pure . Nonce.getEpochNonce . fst)
          (BM.appendName indexerName indexerEventLogger)
      getEpochNo (ExtLedgerStateCoordinator.ExtLedgerStateEvent ledgerState _) = Nonce.getEpochNo ledgerState
   in Nonce.epochNonceWorker
        epochNonceWorkerConfig
        (Nonce.EpochNonceWorkerConfig $ fromMaybe 0 . getEpochNo . fst)
        (path </> "epochNonce.db")

-- | Configure and start the @EpochSDD@ indexer
epochSDDBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => SecurityParam
  -> Core.CatchupConfig
  -> BM.Trace m Text
  -> FilePath
  -> n
      ( Core.WorkerIndexer
          m
          (ExtLedgerStateCoordinator.ExtLedgerStateEvent, WithDistance BlockEvent)
          (NonEmpty SDD.EpochSDD)
          (Core.WithTrace m Core.SQLiteIndexer)
      )
epochSDDBuilder securityParam catchupConfig textLogger path =
  let indexerName = "EpochSDD"
      indexerEventLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
      epochSDDWorkerConfig =
        StandardWorkerConfig
          indexerName
          securityParam
          catchupConfig
          (pure . SDD.getEpochSDD . fst)
          (BM.appendName indexerName indexerEventLogger)
      getEpochNo (ExtLedgerStateCoordinator.ExtLedgerStateEvent ledgerState _) = Nonce.getEpochNo ledgerState
   in SDD.epochSDDWorker
        epochSDDWorkerConfig
        (SDD.EpochSDDWorkerConfig $ fromMaybe 0 . getEpochNo . fst)
        (path </> "epochSDD.db")

-- | Configure and start the @SnapshotBlockEvent@ indexer
snapshotBlockEventBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => SecurityParam
  -> Core.CatchupConfig
  -> BM.Trace m Text
  -> FilePath
  -> BlockRange
  -> FilePath
  -> n
      ( Core.WorkerIndexer
          m
          (ExtLedgerStateCoordinator.ExtLedgerStateEvent, WithDistance BlockEvent)
          SnapshotBlockEvent
          ( Core.WithTrace
              m
              (Core.FileIndexer SnapshotMetadata)
          )
      )
snapshotBlockEventBuilder securityParam catchupConfig textLogger path blockRange' nodeConfig =
  let indexerName = "SnapshotBlockEvent"
      indexerEventLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
      standardWorkerConfig =
        StandardWorkerConfig
          indexerName
          securityParam
          catchupConfig
          extractSnapshotBlockEvent
          (BM.appendName indexerName indexerEventLogger)
   in SnapshotBlockEvent.snapshotBlockEventWorker
        standardWorkerConfig
        (SnapshotWorkerConfig (ExtLedgerStateCoordinator.blockNo . fst) blockRange' nodeConfig)
        path

extractSnapshotBlockEvent
  :: (Applicative f)
  => (ExtLedgerStateEvent, WithDistance BlockEvent)
  -> f (Maybe SnapshotBlockEvent)
extractSnapshotBlockEvent =
  pure . Just . SnapshotBlockEvent . WithDistance.getEvent . snd

-- | Builds the coordinators for each sub-chain serializers.
buildIndexersForSnapshot
  :: SecurityParam
  -> Core.CatchupConfig
  -> ExtLedgerStateCoordinator.ExtLedgerStateWorkerConfig IO (WithDistance BlockEvent)
  -> BM.Trace IO Text
  -> MarconiTrace IO
  -> FilePath
  -> [BlockRange]
  -> FilePath
  -> ExceptT
      Core.IndexerError
      IO
      SyncStatsCoordinator
buildIndexersForSnapshot
  securityParam
  catchupConfig
  epochStateConfig
  textLogger
  prettyLogger
  path
  blockRanges
  nodeConfig = do
    let mainLogger :: BM.Trace IO (Core.IndexerEvent C.ChainPoint)
        mainLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
        blockEventTextLogger = BM.appendName "blockEvent" textLogger
        blockEventLogger = BM.appendName "blockEvent" mainLogger
        snapshotBlockEventTextLogger = BM.appendName "snapshotBlockEvent" blockEventTextLogger
        snapshotExtLedgerStateTextLogger = BM.appendName "snapshotBlockEvent" blockEventTextLogger

    snapshotWorkers <-
      for (zip blockRanges [1 :: Integer ..]) $ \(blockRange', no) -> do
        Core.WorkerIndexer _snapshotBlockEventMVar snapshotBlockEventWorker <-
          snapshotBlockEventBuilder
            securityParam
            catchupConfig
            snapshotBlockEventTextLogger
            (path </> show no)
            blockRange'
            nodeConfig
        Core.WorkerIndexer _snapshotExtLedgerStateEventMVar snapshotExtLedgerStateWorker <-
          snapshotExtLedgerStateEventBuilder
            securityParam
            catchupConfig
            snapshotExtLedgerStateTextLogger
            (path </> show no)
            blockRange'
            nodeConfig
        return [snapshotExtLedgerStateWorker, snapshotBlockEventWorker]

    Core.WorkerIndexer _epochStateMVar snapshotWorker <-
      ExtLedgerStateCoordinator.extLedgerStateWorker
        epochStateConfig
        (concat snapshotWorkers)
        path

    blockCoordinator <-
      lift $
        buildBlockEventCoordinator
          blockEventLogger
          [snapshotWorker]

    lift $
      syncStatsCoordinator
        mainLogger
        prettyLogger
        [blockCoordinator]

snapshotExtLedgerStateEventBuilder
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => SecurityParam
  -> Core.CatchupConfig
  -> BM.Trace m Text
  -> FilePath
  -> BlockRange
  -> FilePath
  -> n
      ( Core.WorkerIndexer
          m
          (ExtLedgerStateCoordinator.ExtLedgerStateEvent, WithDistance BlockEvent)
          ExtLedgerStateEvent
          ( Core.WithTrace
              m
              (Core.FileIndexer EpochMetadata)
          )
      )
snapshotExtLedgerStateEventBuilder securityParam catchupConfig textLogger path blockRange' nodeConfig =
  let indexerName = "SnapshotExtLedgerStateEvent"
      indexerEventLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
      standardWorkerConfig =
        StandardWorkerConfig
          indexerName
          securityParam
          catchupConfig
          (pure . Just . fst)
          (BM.appendName indexerName indexerEventLogger)
   in snapshotExtLedgerStateEventWorker
        securityParam
        standardWorkerConfig
        (SnapshotWorkerConfig (ExtLedgerStateCoordinator.blockNo . fst) blockRange' nodeConfig)
        path

snapshotExtLedgerStateEventWorker
  :: forall input m n
   . (MonadIO m, MonadError Core.IndexerError m, MonadIO n)
  => SecurityParam
  -> StandardWorkerConfig n input ExtLedgerStateEvent
  -> SnapshotWorkerConfig input
  -> FilePath
  -> m
      ( Core.WorkerIndexer
          n
          input
          ExtLedgerStateEvent
          (Core.WithTrace n (Core.FileIndexer EpochMetadata))
      )
snapshotExtLedgerStateEventWorker securityParam standardWorkerConfig snapshotBlockEventWorkerConfig path = do
  codecConfig <- getConfigCodec snapshotBlockEventWorkerConfig
  indexer <-
    Core.withTrace (logger standardWorkerConfig)
      <$> buildExtLedgerStateEventIndexer codecConfig securityParam path
  let preprocessor =
        Core.traverseMaybeEvent (lift . eventExtractor standardWorkerConfig)
          <<< justBeforeBlockRangePreprocessor
            (currentBlockNo snapshotBlockEventWorkerConfig)
            (blockRange snapshotBlockEventWorkerConfig)
  Core.createWorkerWithPreprocessing (workerName standardWorkerConfig) preprocessor indexer

justBeforeBlockRangePreprocessor
  :: (Monad m) => (a -> C.BlockNo) -> BlockRange -> Core.Preprocessor m C.ChainPoint a a
justBeforeBlockRangePreprocessor toBlockNo br =
  Core.scanMaybeEvent filterJustBeforeBlockRange Nothing
  where
    filterJustBeforeBlockRange input
      | inputWord == leftRange - offset =
          pure . Just $ input
      | otherwise = pure Nothing
      where
        offset
          | leftRange == 0 = 0
          | otherwise = 1
        leftRange = Lens.view blockRangeFst br
        inputWord = C.unBlockNo . toBlockNo $ input
