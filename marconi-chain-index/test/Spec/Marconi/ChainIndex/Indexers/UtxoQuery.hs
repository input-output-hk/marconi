{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Spec.Marconi.ChainIndex.Indexers.UtxoQuery (
  tests,
  mkUtxoQuery,
) where

import Marconi.ChainIndex.Indexers.BlockInfo qualified as BlockInfo
import Marconi.ChainIndex.Indexers.Datum qualified as Datum
import Marconi.ChainIndex.Indexers.Spent qualified as Spent
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Indexers.UtxoQuery (UtxoQueryEvent)
import Marconi.ChainIndex.Indexers.UtxoQuery qualified as UtxoQuery
import Marconi.Core qualified as Core

import Cardano.Api qualified as C
import Control.Concurrent qualified as Concurrent
import Control.Lens ((^.), (^..), (^?))
import Control.Lens qualified as Lens
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import System.FilePath ((</>))
import System.IO.Temp qualified as Tmp

import Control.Monad (void)
import Data.Aeson qualified as Aeson
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Hedgehog (Property)
import Hedgehog qualified
import Hedgehog.Gen qualified
import Hedgehog.Range qualified
import Spec.Marconi.ChainIndex.Indexers.BlockInfo (genBlockInfo)
import Spec.Marconi.ChainIndex.Indexers.BlockInfo qualified as Test.BlockInfo
import Spec.Marconi.ChainIndex.Indexers.Datum qualified as Test.Datum
import Spec.Marconi.ChainIndex.Indexers.Spent qualified as Test.Spent
import Spec.Marconi.ChainIndex.Indexers.Utxo qualified as Test.Utxo
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Gen
import Test.Gen.Marconi.Cardano.Core.Types qualified as CGen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.ChainIndex.Indexers.UtxoQuery"
    [ testGroup
        "Indexer"
        [ testPropertyNamed
            "All returned utxos are unspent"
            "propAllUnspent"
            propAllUnspent
        , testPropertyNamed
            "Future spent has a corresponding txIn"
            "propFutureSpentAreTxIn"
            propFutureSpentAreTxIn
        , testPropertyNamed
            "Script data must be resovled when present"
            "propResolvedDatum"
            propResolvedDatum
        ]
    , testPropertyNamed
        "JSON tripping test"
        "propTrippingUtxoResultJSON"
        propTrippingUtxoResultJSON
    ]

-- | Check that alls the returned utxos are either unspent or spent after the upper bound
propAllUnspent :: Property
propAllUnspent = Hedgehog.property $ do
  events <- Hedgehog.forAll Gen.genMockchainWithInfo
  let utxoEvents = Test.Utxo.getTimedUtxosEvents $ Gen.mockchainWithInfoAsMockchain events
  event <- Hedgehog.forAll $ Hedgehog.Gen.element utxoEvents
  let point = event ^. Core.point
  address <-
    Hedgehog.forAll $
      Hedgehog.Gen.element $
        utxoEvents ^.. Lens.folded . Core.event . Lens.folded . Lens.folded . Utxo.address
  res :: [UtxoQuery.UtxoResult] <-
    liftIO $
      withIndexer events $
        Core.query point (UtxoQuery.UtxoQueryInput address Nothing Nothing)
  Hedgehog.assert $
    all (> point) (res ^.. Lens.folded . UtxoQuery.spentInfo . Lens.folded . Core.point)

-- | If an UTxO has a datum, it should be resolved
propResolvedDatum :: Property
propResolvedDatum = Hedgehog.property $ do
  events <- Hedgehog.forAll Gen.genMockchainWithInfo
  let utxoEvents = Test.Utxo.getTimedUtxosEvents $ Gen.mockchainWithInfoAsMockchain events
      datumEvents = Test.Datum.getDatumsEvents $ Gen.mockchainWithInfoAsMockchain events
  event <- Hedgehog.forAll $ Hedgehog.Gen.element utxoEvents
  let point = event ^. Core.point
  address <-
    Hedgehog.forAll $
      Hedgehog.Gen.element $
        utxoEvents ^.. Lens.folded . Core.event . Lens.folded . Lens.folded . Utxo.address
  res :: [UtxoQuery.UtxoResult] <-
    liftIO $
      withIndexer events $
        Core.query point (UtxoQuery.UtxoQueryInput address Nothing Nothing)
  let storedData = Test.Datum.getDatumsEvents $ Gen.mockchainWithInfoAsMockchain events
      notInDatumEvents :: C.Hash C.ScriptData -> Either String String
      notInDatumEvents hash =
        if hash `elem` datumEvents ^.. traverse . Core.event . traverse . traverse . Datum.datumHash
          then Left $ "Hash " <> show hash <> " is unresolved but present in the events"
          else Right "Hash unresolved because it's unknown"
      findByTxIn :: C.TxIn -> Maybe Utxo.Utxo
      findByTxIn txin =
        List.find ((== txin) . Lens.view Utxo.txIn) $
          utxoEvents ^.. traverse . Core.event . traverse . traverse
      datumIsResolved :: UtxoQuery.UtxoResult -> Utxo.Utxo -> Either String String
      datumIsResolved resolved original =
        case (original ^. Utxo.datumHash, resolved ^. UtxoQuery.utxo . Utxo.datumHash) of
          (Nothing, Nothing) -> Right "NoDatum"
          (Just originalDh, Just resolvedDh) | originalDh == resolvedDh -> do
            case resolved ^. UtxoQuery.datum of
              Nothing -> notInDatumEvents resolvedDh
              Just d ->
                let dh = C.hashScriptDataBytes $ C.unsafeHashableScriptData d
                 in if dh == resolvedDh
                      then Right "Datum Resoved"
                      else Left $ "Wrong datum: " <> show dh <> " expected " <> show resolvedDh <> " received"
          (_, _) -> Left "DatuemHashMismatch"
      checkUtxo u = do
        let original = u ^. UtxoQuery.utxo . Utxo.txIn . Lens.to findByTxIn
        maybe (Left "Can't resolve the originalUtxo") (datumIsResolved u) original
  Hedgehog.footnote $
    show $
      events
        ^.. traverse
          . Lens.to Gen.mockBlockWithInfoTxs
          . Lens.folded
          . Lens.to C.getTxBody
  Hedgehog.footnote $ show storedData
  void $ Hedgehog.evalEither $ traverse checkUtxo res
  Hedgehog.success

-- | Check that alls the returned utxos are either unspent or spent after the upper bound
propFutureSpentAreTxIn :: Property
propFutureSpentAreTxIn = Hedgehog.property $ do
  events <- Hedgehog.forAll Gen.genMockchainWithInfo
  let utxoEvents = Test.Utxo.getTimedUtxosEvents $ Gen.mockchainWithInfoAsMockchain events
      geTxId (C.TxIn txId' _) = txId'
      txIds =
        utxoEvents
          ^.. traverse . Core.event . Lens.folded . Lens.folded . Utxo.txIn . Lens.to geTxId
  event <- Hedgehog.forAll $ Hedgehog.Gen.element utxoEvents
  let point = event ^. Core.point
  address <-
    Hedgehog.forAll $
      Hedgehog.Gen.element $
        utxoEvents ^.. Lens.folded . Core.event . Lens.folded . Lens.folded . Utxo.address
  res :: [UtxoQuery.UtxoResult] <-
    liftIO $
      withIndexer events $
        Core.query point (UtxoQuery.UtxoQueryInput address Nothing Nothing)
  let findSpentInUtxos = flip elem txIds
      findUTxo utxoRes = fromMaybe True $ do
        spendTxId <- utxoRes ^? UtxoQuery.spentInfo . traverse . Core.event . Lens._2
        pure $ findSpentInUtxos spendTxId
  Hedgehog.assert $
    all findUTxo res

propTrippingUtxoResultJSON :: Property
propTrippingUtxoResultJSON = Hedgehog.property $ do
  event <- Hedgehog.forAll genUtxoResult
  Hedgehog.tripping event Aeson.encode Aeson.eitherDecode

withIndexer
  :: Gen.MockchainWithInfo C.BabbageEra
  -> ( Core.SQLiteAggregateQuery
        IO
        C.ChainPoint
        UtxoQueryEvent
       -> (ExceptT (Core.QueryError UtxoQuery.UtxoQueryInput) IO) a
     )
  -> IO a
withIndexer events f = do
  (indexers, utxoQuery) <- liftIO mkUtxoQuery
  liftIO $ indexMockchain indexers events
  Right res <- liftIO $ runExceptT $ f utxoQuery
  void $ liftIO $ closeIndexers indexers utxoQuery
  pure res

indexMockchain
  :: UtxoQueryIndexers -> Gen.MockchainWithInfo C.BabbageEra -> IO ()
indexMockchain
  (UtxoQueryIndexers utxoVar spentVar datumVar blockInfoVar)
  chainWithInfo = do
    let chain = Gen.mockchainWithInfoAsMockchain chainWithInfo
    Concurrent.modifyMVar_ utxoVar $
      fmap (either (error . show) id) . Core.indexAllEither (Test.Utxo.getTimedUtxosEvents chain)
    Concurrent.modifyMVar_ spentVar $
      fmap (either (error . show) id) . Core.indexAllEither (Test.Spent.getSpentsEvents chain)
    Concurrent.modifyMVar_ datumVar $
      fmap (either (error . show) id) . Core.indexAllEither (Test.Datum.getDatumsEvents chain)
    Concurrent.modifyMVar_ blockInfoVar $
      fmap (either (error . show) id)
        . Core.indexAllEither (Test.BlockInfo.getBlockInfoEvents chainWithInfo)

closeIndexers
  :: (MonadIO m)
  => UtxoQueryIndexers
  -> Core.SQLiteAggregateQuery m C.ChainPoint UtxoQueryEvent
  -> m ()
closeIndexers
  (UtxoQueryIndexers utxoVar spentVar datumVar blockInfoVar)
  aggregate = do
    liftIO $ Concurrent.withMVar utxoVar Core.close
    liftIO $ Concurrent.withMVar spentVar Core.close
    liftIO $ Concurrent.withMVar datumVar Core.close
    liftIO $ Concurrent.withMVar blockInfoVar Core.close
    Core.close aggregate

data UtxoQueryIndexers = UtxoQueryIndexers
  { _utxoIndexer :: Concurrent.MVar Utxo.UtxoIndexer
  , _spentIndexer :: Concurrent.MVar Spent.SpentIndexer
  , _datumIndexer :: Concurrent.MVar Datum.DatumIndexer
  , _blockInfoIndexer :: Concurrent.MVar BlockInfo.BlockInfoIndexer
  }

utxoQueryIndexersAsAggregate :: (MonadIO m) => UtxoQueryIndexers -> UtxoQuery.UtxoQueryAggregate m
utxoQueryIndexersAsAggregate
  (UtxoQueryIndexers utxoIndexer spentIndexer datumIndexer blockInfoIndexer) =
    UtxoQuery.UtxoQueryAggregate
      { UtxoQuery.blockInfoIndexer = blockInfoIndexer
      , UtxoQuery.datumIndexer = datumIndexer
      , UtxoQuery.spentIndexer = spentIndexer
      , UtxoQuery.utxoIndexer = utxoIndexer
      }

mkUtxoQuery
  :: IO
      ( UtxoQueryIndexers
      , Core.SQLiteAggregateQuery
          IO
          C.ChainPoint
          UtxoQueryEvent
      )
mkUtxoQuery = Tmp.withSystemTempDirectory "testUtxoQuery" $ \dir -> do
  let blockInfoPath = dir </> "blockInfo.db"
  Right blockInfoIndexer <- runExceptT $ BlockInfo.mkBlockInfoIndexer blockInfoPath
  blockInfoVar <- liftIO $ Concurrent.newMVar blockInfoIndexer
  let datumPath = dir </> "datum.db"
  Right datumIndexer <- runExceptT $ Datum.mkDatumIndexer datumPath
  datumVar <- liftIO $ Concurrent.newMVar datumIndexer
  let spentPath = dir </> "spent.db"
  Right spentIndexer <- runExceptT $ Spent.mkSpentIndexer spentPath
  spentVar <- liftIO $ Concurrent.newMVar spentIndexer
  let utxoPath = dir </> "utxo.db"
  Right utxoIndexer <- runExceptT $ Utxo.mkUtxoIndexer utxoPath
  utxoVar <- liftIO $ Concurrent.newMVar utxoIndexer
  let indexers = UtxoQueryIndexers utxoVar spentVar datumVar blockInfoVar
  utxoQuery <- UtxoQuery.mkUtxoSQLiteQuery $ utxoQueryIndexersAsAggregate indexers
  pure (indexers, utxoQuery)

genTimed :: Hedgehog.Gen a -> Hedgehog.Gen (Core.Timed C.ChainPoint a)
genTimed gen = Core.Timed <$> CGen.genChainPoint <*> gen

genUtxoResult :: Hedgehog.Gen UtxoQuery.UtxoResult
genUtxoResult =
  UtxoQuery.UtxoResult
    <$> Test.Utxo.genUtxo
    <*> Hedgehog.Gen.maybe (C.getScriptData <$> CGen.genHashableScriptData)
    <*> genTimed genBlockInfo
    <*> Hedgehog.Gen.maybe (genTimed $ (,) <$> Test.BlockInfo.genBlockInfo <*> CGen.genTxId)
    <*> Hedgehog.Gen.list (Hedgehog.Range.linear 0 10) CGen.genTxIn
