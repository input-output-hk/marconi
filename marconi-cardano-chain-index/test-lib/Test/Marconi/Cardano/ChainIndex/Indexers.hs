{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Generators and helpers for testing @Marconi.Cardano.Indexers@, namely the
'SyncStatsCoordinator'.
-}
module Test.Marconi.Cardano.ChainIndex.Indexers where

import Cardano.Api qualified as C
import Cardano.BM.Tracing qualified as BM
import Control.Concurrent (MVar, modifyMVar_)
import Control.Exception (throwIO)
import Control.Lens (makeLenses, (^.))
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import Marconi.Cardano.ChainIndex.Indexers qualified as Indexers
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance)
import Marconi.Cardano.Core.Indexer.Worker (
  StandardWorker (StandardWorker),
 )
import Marconi.Cardano.Core.Indexer.Worker qualified as Core
import Marconi.Cardano.Core.Types (
  AnyTxBody (AnyTxBody),
  BlockEvent (BlockEvent),
  MarconiTrace,
  SecurityParam,
  TxIndexInBlock,
 )
import Marconi.Cardano.Indexers.BlockInfo (BlockInfo)
import Marconi.Cardano.Indexers.BlockInfo qualified as BlockInfo
import Marconi.Cardano.Indexers.ChainTip qualified as ChainTip
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
import Marconi.Cardano.Indexers.Utxo (UtxoEvent)
import Marconi.Cardano.Indexers.Utxo qualified as Utxo
import Marconi.Cardano.Indexers.UtxoQuery qualified as UtxoQuery
import Marconi.Core qualified as Core
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Test.Mockchain
import Test.Gen.Marconi.Cardano.Indexers.BlockInfo qualified as Test.BlockInfo
import Test.Gen.Marconi.Cardano.Indexers.Datum qualified as Test.Datum
import Test.Gen.Marconi.Cardano.Indexers.MintTokenEvent qualified as Test.MintTokenEvent
import Test.Gen.Marconi.Cardano.Indexers.Spent qualified as Test.Spent
import Test.Gen.Marconi.Cardano.Indexers.Utxo qualified as Test.Utxo

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
closeIndexers indexers = Core.close (indexers ^. testBuildIndexersResultCoordinator)

{- | Index the exposed indexers in 'TestBuildIndexersResult' with a given Mockchain, except
 - for the coordinator and current sync point indexer (within MarconiCardanoQueryables) which can only
 - be indexed with 'BlockEvent' s coming from the chain-sync protocol.
 -
 - NOTE: This does not currently index EpochSDD or EpochNonce indexers.
-}
indexAllWithMockchain
  :: TestBuildIndexersResult
  -> Test.Mockchain.MockchainWithInfoAndDistance C.BabbageEra
  -> IO ()
indexAllWithMockchain indexers chain = do
  -- Conversions needed for different indexers
  let
    chainNoInfo :: Test.Mockchain.MockchainWithDistance C.BabbageEra
    chainNoInfo = Test.Mockchain.mockchainWithInfoAsMockchainWithDistance chain

    toBlockInfoEvents
      :: Test.Mockchain.MockchainWithInfoAndDistance C.BabbageEra
      -> [Core.Timed C.ChainPoint (Maybe (WithDistance (Maybe BlockInfo)))]
    toBlockInfoEvents = map (fmap Just) . Test.BlockInfo.getTimedBlockInfoEventsWithInfoAndDistance

    toUtxoEvents
      :: Test.Mockchain.MockchainWithDistance C.BabbageEra
      -> [Core.Timed C.ChainPoint (Maybe (WithDistance (Maybe UtxoEvent)))]
    toUtxoEvents = map (fmap Just) . Test.Utxo.getTimedUtxosEventsWithDistance

    toSpentsEvents
      :: Test.Mockchain.MockchainWithDistance C.BabbageEra
      -> [Core.Timed C.ChainPoint (Maybe (WithDistance (Maybe (NonEmpty Spent.SpentInfo))))]
    toSpentsEvents = map (fmap Just) . Test.Spent.getTimedSpentsEventsWithDistance

    toDatumsEvents
      :: Test.Mockchain.MockchainWithDistance C.BabbageEra
      -> [Core.Timed C.ChainPoint (Maybe (WithDistance (Maybe (NonEmpty Datum.DatumInfo))))]
    toDatumsEvents = map (fmap Just) . Test.Datum.getTimedDatumsEventsWithDistance

    toMintTokenEvents
      :: Test.Mockchain.MockchainWithDistance C.BabbageEra
      -> [Core.Timed C.ChainPoint (Maybe (WithDistance (Maybe MintTokenEvent.MintTokenBlockEvents)))]
    toMintTokenEvents = map (fmap Just) . Test.MintTokenEvent.getTimedMintTokentEventsWithDistance

  modifyMVar_ (indexers ^. testBuildIndexersResultBlockInfoIndexer) $
    Core.indexAllEither (toBlockInfoEvents chain) >=> either throwIO pure
  modifyMVar_ (indexers ^. testBuildIndexersResultMintTokenEvent) $
    Core.indexAllEither (toMintTokenEvents chainNoInfo) >=> either throwIO pure
  modifyMVar_ (indexers ^. testBuildIndexersResultUtxo) $
    Core.indexAllEither (toUtxoEvents chainNoInfo) >=> either throwIO pure
  modifyMVar_ (indexers ^. testBuildIndexersResultSpent) $
    Core.indexAllEither (toSpentsEvents chainNoInfo) >=> either throwIO pure
  modifyMVar_ (indexers ^. testBuildIndexersResultDatum) $
    Core.indexAllEither (toDatumsEvents chainNoInfo) >=> either throwIO pure

{- | This is a copy-paste version of @Marconi.Cardano.ChainIndex.Indexers.'buildIndexers'@
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
      BlockInfo.blockInfoBuilder securityParam catchupConfig blockEventTextLogger path

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
      Utxo.utxoBuilder securityParam catchupConfig utxoConfig txBodyCoordinatorLogger path
    StandardWorker spentMVar spentWorker <-
      Spent.spentBuilder securityParam catchupConfig txBodyCoordinatorLogger path
    StandardWorker datumMVar datumWorker <-
      Datum.datumBuilder securityParam catchupConfig txBodyCoordinatorLogger path
    StandardWorker mintTokenMVar mintTokenWorker <-
      MintTokenEvent.mintTokenEventBuilder
        securityParam
        catchupConfig
        mintEventConfig
        txBodyCoordinatorLogger
        path

    let getTxBody :: (C.IsCardanoEra era) => C.BlockNo -> TxIndexInBlock -> C.Tx era -> AnyTxBody
        getTxBody blockNo ix tx = AnyTxBody blockNo ix (C.getTxBody tx)
        toTxBodys :: BlockEvent -> [AnyTxBody]
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

    Core.WorkerIndexer chainTipMVar chainTipWorker <-
      ChainTip.chainTipBuilder mainLogger path

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
