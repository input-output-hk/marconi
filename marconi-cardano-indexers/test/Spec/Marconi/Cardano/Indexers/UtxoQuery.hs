{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Spec.Marconi.Cardano.Indexers.UtxoQuery (
  tests,
) where

import Cardano.Api qualified as C
import Control.Lens ((^.), (^..), (^?))
import Control.Lens qualified as Lens
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as Aeson
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Hedgehog (Property)
import Hedgehog qualified
import Hedgehog.Gen qualified
import Marconi.Cardano.Indexers.Datum qualified as Datum
import Marconi.Cardano.Indexers.Utxo qualified as Utxo
import Marconi.Cardano.Indexers.UtxoQuery qualified as UtxoQuery
import Marconi.Core qualified as Core
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Test.Mockchain
import Test.Gen.Marconi.Cardano.Indexers.Datum qualified as Test.Datum
import Test.Gen.Marconi.Cardano.Indexers.Utxo qualified as Test.Utxo
import Test.Gen.Marconi.Cardano.Indexers.UtxoQuery qualified as Test.UtxoQuery
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.Cardano.Indexers.UtxoQuery"
    [ testGroup
        "Indexer"
        [ testPropertyNamed
            "All returned utxos are unspent or spent after the upper bound"
            "propAllUnspent"
            propAllUnspent
        , testPropertyNamed
            "All returned utxos are unspent when the queried point is genesis with no upper bound"
            "propAllUnspentWhenPointGenesis"
            propAllUnspentWhenPointGenesis
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
  events <- Hedgehog.forAll Test.Mockchain.genMockchainWithInfo
  let utxoEvents = Test.Utxo.getTimedUtxosEvents $ Test.Mockchain.mockchainWithInfoAsMockchain events
  event <- Hedgehog.forAll $ Hedgehog.Gen.element utxoEvents
  let point = event ^. Core.point
  address <-
    Hedgehog.forAll $
      Hedgehog.Gen.element $
        utxoEvents ^.. Lens.folded . Core.event . Lens.folded . Lens.folded . Utxo.address
  res :: [UtxoQuery.UtxoResult] <-
    liftIO $
      Test.UtxoQuery.withIndexer events $
        Core.query point (UtxoQuery.UtxoQueryInput address Nothing Nothing)
  Hedgehog.assert $
    all (> point) (res ^.. Lens.folded . UtxoQuery.spentInfo . Lens.folded . Core.point)

{- | When the @point@ passed to @Core.'query'@ is 'genesis', and there is no provided upper bound,
the query should return all UTxOs that are unspent given the full history.
-}
propAllUnspentWhenPointGenesis :: Property
propAllUnspentWhenPointGenesis = Hedgehog.property $ do
  events <- Hedgehog.forAll Test.Mockchain.genMockchainWithInfo
  let
    utxoEvents =
      Test.Utxo.getTimedUtxosEvents $
        Test.Mockchain.mockchainWithInfoAsMockchain events
    utxos = utxoEvents ^.. Lens.folded . Core.event . Lens.folded . Lens.folded
    hasNoUtxoEvents = null utxos

  Hedgehog.cover 90 "Has at least one UTxO" (not hasNoUtxoEvents)
  unless hasNoUtxoEvents $ do
    -- Take the last UTxO to guarantee the result should
    -- have at least one element.
    let
      event = last utxos
      address = event ^. Utxo.address

    res :: [UtxoQuery.UtxoResult] <-
      liftIO $
        Test.UtxoQuery.withIndexer events $
          Core.query Core.genesis (UtxoQuery.UtxoQueryInput address Nothing Nothing)

    Hedgehog.assert $
      -- This should be empty since none of the returned UTxOs should have been spent.
      null (res ^.. Lens.folded . UtxoQuery.spentInfo . Lens.folded)

-- | If an UTxO has a datum, it should be resolved
propResolvedDatum :: Property
propResolvedDatum = Hedgehog.property $ do
  events <- Hedgehog.forAll Test.Mockchain.genMockchainWithInfo
  let utxoEvents = Test.Utxo.getTimedUtxosEvents $ Test.Mockchain.mockchainWithInfoAsMockchain events
      datumEvents = Test.Datum.getTimedDatumsEvents $ Test.Mockchain.mockchainWithInfoAsMockchain events
  event <- Hedgehog.forAll $ Hedgehog.Gen.element utxoEvents
  let point = event ^. Core.point
  address <-
    Hedgehog.forAll $
      Hedgehog.Gen.element $
        utxoEvents ^.. Lens.folded . Core.event . Lens.folded . Lens.folded . Utxo.address
  res :: [UtxoQuery.UtxoResult] <-
    liftIO $
      Test.UtxoQuery.withIndexer events $
        Core.query point (UtxoQuery.UtxoQueryInput address Nothing Nothing)
  let storedData = Test.Datum.getTimedDatumsEvents $ Test.Mockchain.mockchainWithInfoAsMockchain events
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
          . Lens.to Test.Mockchain.mockBlockWithInfoTxs
          . Lens.folded
          . Lens.to C.getTxBody
  Hedgehog.footnote $ show storedData
  void $ Hedgehog.evalEither $ traverse checkUtxo res
  Hedgehog.success

-- | Check that alls the returned utxos are either unspent or spent after the upper bound
propFutureSpentAreTxIn :: Property
propFutureSpentAreTxIn = Hedgehog.property $ do
  events <- Hedgehog.forAll Test.Mockchain.genMockchainWithInfo
  let utxoEvents = Test.Utxo.getTimedUtxosEvents $ Test.Mockchain.mockchainWithInfoAsMockchain events
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
      Test.UtxoQuery.withIndexer events $
        Core.query point (UtxoQuery.UtxoQueryInput address Nothing Nothing)
  let findSpentInUtxos = flip elem txIds
      findUTxo utxoRes = fromMaybe True $ do
        spendTxId <- utxoRes ^? UtxoQuery.spentInfo . traverse . Core.event . Lens._2
        pure $ findSpentInUtxos spendTxId
  Hedgehog.assert $
    all findUTxo res

propTrippingUtxoResultJSON :: Property
propTrippingUtxoResultJSON = Hedgehog.property $ do
  event <- Hedgehog.forAll Test.UtxoQuery.genUtxoResult
  Hedgehog.tripping event Aeson.encode Aeson.eitherDecode
