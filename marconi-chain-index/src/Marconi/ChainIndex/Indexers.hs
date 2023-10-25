{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Marconi.ChainIndex.Indexers where

import Cardano.Api.Extended qualified as C
import Cardano.BM.Tracing qualified as BM
import Control.Concurrent (MVar)
import Control.Lens (makeLenses, (?~))
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (ExceptT, MonadError, MonadTrans (lift))
import Data.Function ((&))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Marconi.ChainIndex.Extract.WithDistance (WithDistance)
import Marconi.ChainIndex.Indexers.BlockInfo qualified as Block
import Marconi.ChainIndex.Indexers.ChainTip qualified as ChainTip
import Marconi.ChainIndex.Indexers.Coordinator (coordinatorWorker, standardCoordinator)
import Marconi.ChainIndex.Indexers.Datum qualified as Datum
import Marconi.ChainIndex.Indexers.EpochState qualified as EpochState
import Marconi.ChainIndex.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.ChainIndex.Indexers.Spent qualified as Spent
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Indexers.UtxoQuery qualified as UtxoQuery
import Marconi.ChainIndex.Indexers.Worker (
  StandardIndexer,
  StandardWorker (StandardWorker),
  StandardWorkerConfig (StandardWorkerConfig),
 )
import Marconi.ChainIndex.Runner (TipOrBlock (Block, Tip))
import Marconi.ChainIndex.Types (
  BlockEvent (BlockEvent),
  SecurityParam,
  TxIndexInBlock,
  blockInMode,
 )
import Marconi.Core qualified as Core
import System.FilePath ((</>))

data AnyTxBody = forall era. (C.IsCardanoEra era) => AnyTxBody C.BlockNo TxIndexInBlock (C.TxBody era)
type instance Core.Point (Either C.ChainTip (WithDistance BlockEvent)) = C.ChainPoint
type instance Core.Point BlockEvent = C.ChainPoint
type instance Core.Point C.ChainTip = C.ChainPoint
type instance Core.Point [AnyTxBody] = C.ChainPoint

-- Convenience aliases for indexers
type Coordinator = Core.WithTrace IO Core.Coordinator TipOrBlock
type EpochStateIndexer =
  EpochState.EpochStateIndexer
    (WithDistance (Maybe EpochState.ExtLedgerState, C.BlockInMode C.CardanoMode))
type MintTokenEventIndexer =
  StandardIndexer IO Core.SQLiteIndexer MintTokenEvent.MintTokenBlockEvents
type UtxoIndexer = UtxoQuery.UtxoQueryIndexer IO

-- | Container for all the queryable indexers of marconi-chain-index.
data MarconiChainIndexQueryables = MarconiChainIndexQueryables
  { _queryableEpochState :: !(MVar EpochStateIndexer)
  , _queryableMintToken :: !(MVar MintTokenEventIndexer)
  , _queryableUtxo :: !UtxoIndexer
  }

makeLenses 'MarconiChainIndexQueryables

{- | Build all the indexers of marconi-chain-index
(all those which are available with the new implementation)
and expose a single coordinator to operate them
-}
buildIndexers
  :: SecurityParam
  -> Core.CatchupConfig
  -> Utxo.UtxoIndexerConfig
  -> MintTokenEvent.MintTokenEventConfig
  -> EpochState.EpochStateWorkerConfig
  -> BM.Trace IO Text
  -> FilePath
  -> ExceptT
      Core.IndexerError
      IO
      ( C.ChainPoint
      , MarconiChainIndexQueryables
      , Coordinator
      )
buildIndexers securityParam catchupConfig utxoConfig mintEventConfig epochStateConfig textLogger path = do
  let mainLogger :: BM.Trace IO (Core.IndexerEvent C.ChainPoint)
      mainLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
      blockEventTextLogger = BM.appendName "blockEvent" textLogger
      blockEventLogger = BM.appendName "blockEvent" mainLogger
      txBodyCoordinatorLogger = BM.appendName "txBody" blockEventTextLogger

  StandardWorker blockInfoMVar blockInfoWorker <-
    blockInfoBuilder securityParam catchupConfig blockEventTextLogger path

  Core.WorkerIndexer epochStateMVar epochStateWorker <-
    epochStateBuilder securityParam catchupConfig epochStateConfig blockEventTextLogger path

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

  queryIndexer <-
    lift $
      UtxoQuery.mkUtxoSQLiteQuery $
        UtxoQuery.UtxoQueryAggregate utxoMVar spentMVar datumMVar blockInfoMVar

  let queryables =
        MarconiChainIndexQueryables
          epochStateMVar
          mintTokenMVar
          queryIndexer

  blockCoordinator <-
    lift $
      buildBlockEventCoordinator
        blockEventLogger
        [blockInfoWorker, epochStateWorker, coordinatorTxBodyWorkers]

  Core.WorkerIndexer _chainTipMVar chainTipWorker <- chainTipBuilder mainLogger path

  mainCoordinator <- lift $ standardCoordinator mainLogger [blockCoordinator, chainTipWorker]

  resumePoint <- Core.lastStablePoint mainCoordinator

  pure (resumePoint, queryables, mainCoordinator)

-- | Build and start a coordinator of a bunch of workers that takes an @AnyTxBody@ as an input
buildBlockEventCoordinator
  :: (MonadIO m)
  => BM.Trace IO (Core.IndexerEvent C.ChainPoint)
  -> [Core.Worker (WithDistance BlockEvent) C.ChainPoint]
  -> m (Core.Worker TipOrBlock C.ChainPoint)
buildBlockEventCoordinator logger workers =
  let rightToMaybe = \case
        Tip _ -> Nothing
        Block x -> Just x
   in Core.worker <$> coordinatorWorker "BlockEvent coordinator" logger (pure . rightToMaybe) workers

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
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => SecurityParam
  -> Core.CatchupConfig
  -> BM.Trace m Text
  -> FilePath
  -> n (StandardWorker m BlockEvent Block.BlockInfo Core.SQLiteIndexer)
blockInfoBuilder securityParam catchupConfig textLogger path =
  let indexerName = "BlockInfo"
      indexerEventLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
      extractBlockInfo :: BlockEvent -> Block.BlockInfo
      extractBlockInfo (BlockEvent (C.BlockInMode b _) eno t) = Block.fromBlockEratoBlockInfo b eno t
      blockInfoWorkerConfig =
        StandardWorkerConfig
          indexerName
          securityParam
          catchupConfig
          (pure . Just . extractBlockInfo)
          (BM.appendName indexerName indexerEventLogger)
   in Block.blockInfoWorker blockInfoWorkerConfig (path </> "blockInfo.db")

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
          TipOrBlock
          C.ChainTip
          (Core.WithTrace m Core.LastEventIndexer)
      )
chainTipBuilder tracer path = do
  let chainTipPath = path </> "chainTip"
      tipOnly (Tip x) = Just x
      tipOnly _other = Nothing
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

-- | Configure and start the @EpochState@ indexer
epochStateBuilder
  :: (MonadIO n, MonadError Core.IndexerError n)
  => SecurityParam
  -> Core.CatchupConfig
  -> EpochState.EpochStateWorkerConfig
  -> BM.Trace IO Text
  -> FilePath
  -> n
      ( Core.WorkerIndexer
          IO
          (WithDistance BlockEvent)
          (WithDistance (Maybe EpochState.ExtLedgerState, C.BlockInMode C.CardanoMode))
          EpochState.EpochStateIndexer
      )
epochStateBuilder securityParam catchupConfig epochStateConfig textLogger path =
  let indexerName = "EpochState"
      indexerEventLogger = BM.contramap (fmap (fmap $ Text.pack . show)) textLogger
      epochStateWorkerConfig =
        StandardWorkerConfig
          indexerName
          securityParam
          catchupConfig
          (pure . Just . blockInMode)
          (BM.appendName indexerName indexerEventLogger)
   in EpochState.mkEpochStateWorker epochStateWorkerConfig epochStateConfig (path </> "epochState")
