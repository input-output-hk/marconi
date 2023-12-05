{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Generators and helpers for testing @Marconi.Cardano.Indexers@, namely the
'SyncStatsCoordinator'.
-}
module Test.Gen.Marconi.Cardano.Indexers where

import Cardano.Api qualified as C
import Cardano.BM.Tracing qualified as BM
import Control.Concurrent (MVar, withMVar)
import Control.Exception (throwIO)
import Control.Lens (makeLenses, (^.))
import Control.Monad (void, (>=>))
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance)
import Marconi.Cardano.Core.Indexer.Worker (
  StandardWorker (StandardWorker),
 )
import Marconi.Cardano.Core.Indexer.Worker qualified as Core
import Marconi.Cardano.Core.Types (
  BlockEvent (BlockEvent),
  MarconiTrace,
  SecurityParam,
  TxIndexInBlock,
 )
import Marconi.Cardano.Indexers qualified as Indexers
import Marconi.Cardano.Indexers.BlockInfo qualified as BlockInfo
import Marconi.Cardano.Indexers.Coordinator (syncStatsCoordinator)
import Marconi.Cardano.Indexers.CurrentSyncPointQuery qualified as CurrentSyncPoint
import Marconi.Cardano.Indexers.Datum qualified as Datum
import Marconi.Cardano.Indexers.EpochNonce qualified as Nonce
import Marconi.Cardano.Indexers.EpochSDD qualified as SDD
import Marconi.Cardano.Indexers.ExtLedgerStateCoordinator qualified as ExtLedgerStateCoordinator
import Marconi.Cardano.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.Cardano.Indexers.MintTokenEventQuery (
  MintTokenEventIndexerQuery (MintTokenEventIndexerQuery),
 )
import Marconi.Cardano.Indexers.Spent qualified as Spent
import Marconi.Cardano.Indexers.Utxo qualified as Utxo
import Marconi.Cardano.Indexers.UtxoQuery qualified as UtxoQuery
import Marconi.Core qualified as Core
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Mockchain
import Test.Gen.Marconi.Cardano.Indexers.BlockInfo qualified as BlockInfo

data TestBuildIndexersResult = TestBuildIndexersResult
  { _testBuildIndexersResultChainPoint :: C.ChainPoint
  , _testBuildIndexersResultQueryables :: Indexers.MarconiCardanoQueryables
  , _testBuildIndexersResultCoordinator :: Indexers.SyncStatsCoordinator
  , _testBuildIndexersResultBlockInfoIndexer
      :: MVar (Core.StandardIndexer IO Core.SQLiteIndexer BlockInfo.BlockInfo)
  , _testBuildIndexersResultEpochSDD
      :: MVar (Core.WithTrace IO Core.SQLiteIndexer (NonEmpty SDD.EpochSDD))
  , _testBuildIndexersResultEpochNonce
      :: MVar (Core.WithTrace IO Core.SQLiteIndexer Nonce.EpochNonce)
  , _testBuildIndexersResultUtxo :: MVar (Core.StandardIndexer IO Core.SQLiteIndexer Utxo.UtxoEvent)
  , _testBuildIndexersResultSpent
      :: MVar (Core.StandardIndexer IO Core.SQLiteIndexer Spent.SpentInfoEvent)
  , _testBuildIndexersResultDatum :: MVar (Core.StandardIndexer IO Core.SQLiteIndexer Datum.DatumEvent)
  , _testBuildIndexersResultMintTokenEvent
      :: MVar (Core.StandardIndexer IO Core.SQLiteIndexer MintTokenEvent.MintTokenBlockEvents)
  }

makeLenses ''TestBuildIndexersResult

-- | Close all indexers manually.
closeIndexers
  :: TestBuildIndexersResult
  -> IO ()
closeIndexers indexers = do
  withMVar (indexers ^. testBuildIndexersResultBlockInfoIndexer) Core.close
  withMVar (indexers ^. testBuildIndexersResultEpochSDD) Core.close
  withMVar (indexers ^. testBuildIndexersResultEpochNonce) Core.close
  withMVar (indexers ^. testBuildIndexersResultUtxo) Core.close
  withMVar (indexers ^. testBuildIndexersResultSpent) Core.close
  withMVar (indexers ^. testBuildIndexersResultDatum) Core.close
  withMVar (indexers ^. testBuildIndexersResultMintTokenEvent) Core.close
  -- TODO: PLT-8634 does closing the coordinator close all indexers?
  Core.close (indexers ^. testBuildIndexersResultCoordinator)

{- | Index all with a given Mockchain. For tests, you must index each individual indexer
rather than indexing the coordinator. The coordinator takes 'BlockEvent's coming from the
chain-sync stream, which we cannot construct directly.
-}
indexAllWithMockchain
  :: TestBuildIndexersResult
  -> Mockchain.MockchainWithInfoAndDistance C.BabbageEra
  -> IO ()
indexAllWithMockchain indexers chain = do
  -- Conversions needed for different indexers
  let
    toBlockInfoEvents = map (fmap Just) . BlockInfo.getTimedBlockInfoEventsWithInfoAndDistance
  -- TODO: PLT-8634
  withMVar (indexers ^. testBuildIndexersResultBlockInfoIndexer) $
    void . (Core.indexAllEither (toBlockInfoEvents chain) >=> either throwIO pure)
  withMVar (indexers ^. testBuildIndexersResultEpochSDD) Core.close
  withMVar (indexers ^. testBuildIndexersResultEpochNonce) Core.close
  withMVar (indexers ^. testBuildIndexersResultUtxo) Core.close
  withMVar (indexers ^. testBuildIndexersResultSpent) Core.close
  withMVar (indexers ^. testBuildIndexersResultDatum) Core.close
  withMVar (indexers ^. testBuildIndexersResultMintTokenEvent) Core.close

{- | This is a copy-paste version of @Marconi.Cardano.Indexers.'buildIndexers'@
whose sole purpose is to expose the elementary indexer workers inside. That allows us to index
generated events directly, which is useful in testing specific JSON RPC query handlers.
We cannot index the returned coordinator directly because it takes, in essence, BlockEvents which
we cannot generate explicitly.
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
      TestBuildIndexersResult
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
      Indexers.blockInfoBuilder securityParam catchupConfig blockEventTextLogger path

    Core.WorkerIndexer epochSDDMVar epochSDDWorker <-
      Indexers.epochSDDBuilder securityParam catchupConfig epochSDDTextLogger path
    Core.WorkerIndexer epochNonceMVar epochNonceWorker <-
      Indexers.epochNonceBuilder securityParam catchupConfig epochNonceTextLogger path
    Core.WorkerIndexer _epochStateMVar epochStateWorker <-
      ExtLedgerStateCoordinator.extLedgerStateWorker
        epochStateConfig
        [epochSDDWorker, epochNonceWorker]
        path

    StandardWorker utxoMVar utxoWorker <-
      Indexers.utxoBuilder securityParam catchupConfig utxoConfig txBodyCoordinatorLogger path
    StandardWorker spentMVar spentWorker <-
      Indexers.spentBuilder securityParam catchupConfig txBodyCoordinatorLogger path
    StandardWorker datumMVar datumWorker <-
      Indexers.datumBuilder securityParam catchupConfig txBodyCoordinatorLogger path
    StandardWorker mintTokenMVar mintTokenWorker <-
      Indexers.mintBuilder securityParam catchupConfig mintEventConfig txBodyCoordinatorLogger path

    let getTxBody :: (C.IsCardanoEra era) => C.BlockNo -> TxIndexInBlock -> C.Tx era -> Indexers.AnyTxBody
        getTxBody blockNo ix tx = Indexers.AnyTxBody blockNo ix (C.getTxBody tx)
        toTxBodys :: BlockEvent -> [Indexers.AnyTxBody]
        toTxBodys (BlockEvent (C.BlockInMode (C.Block (C.BlockHeader _ _ bn) txs) _) _ _) =
          zipWith (getTxBody bn) [0 ..] txs

    coordinatorTxBodyWorkers <-
      Indexers.buildTxBodyCoordinator
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
        Indexers.buildBlockEventCoordinator
          blockEventLogger
          [blockInfoWorker, epochStateWorker, coordinatorTxBodyWorkers]

    Core.WorkerIndexer chainTipMVar chainTipWorker <- Indexers.chainTipBuilder mainLogger path

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
          Indexers.MarconiCardanoQueryables
            epochNonceMVar
            epochSDDMVar
            (MintTokenEventIndexerQuery securityParam mintTokenMVar blockInfoMVar)
            utxoQueryIndexer
            currentSyncPointIndexer

    resumePoint <- Core.lastStablePoint mainCoordinator

    pure $
      TestBuildIndexersResult
        resumePoint
        queryables
        mainCoordinator
        blockInfoMVar
        epochSDDMVar
        epochNonceMVar
        utxoMVar
        spentMVar
        datumMVar
        mintTokenMVar
