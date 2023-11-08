{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Marconi.ChainIndex.Indexers.Spent (
  tests,
  getSpentsEvents,
  genSpent,
) where

import Cardano.Api qualified as C
import Control.Lens ((^.))
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Gen qualified
import Marconi.ChainIndex.Indexers.Spent qualified as Spent
import Marconi.Core qualified as Core
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Gen.Marconi.ChainIndex.Mockchain qualified as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.ChainIndex.Indexers.Spent"
    [ testGroup
        "Indexer"
        [ testPropertyNamed
            "EventAt query works for spents"
            "propRoundTripAtSlotSpent"
            propRoundTripAtSlotSpent
        , testPropertyNamed
            "SpentIndexer can retrieve all the spents it stores"
            "propRoundTripSpent"
            propRoundTripSpent
        , testPropertyNamed
            "EventAt query works as a list indexer"
            "propActLikeListIndexerOnEventAt"
            propActLikeListIndexerOnEventAt
        ]
    , testPropertyNamed
        "JSON event tripping test"
        "propTrippingSpentJSON"
        propTrippingSpentJSON
    ]

-- | We can retrieve the event at a given slot
propRoundTripAtSlotSpent :: Hedgehog.Property
propRoundTripAtSlotSpent = Hedgehog.property $ do
  events <- Hedgehog.forAll $ getSpentsEvents <$> Gen.genMockchain
  event <- Hedgehog.forAll $ Hedgehog.Gen.element events
  emptyIndexer <- Hedgehog.evalExceptT $ Spent.mkSpentIndexer ":memory:"
  indexer <- Hedgehog.evalExceptT $ Core.indexAll events emptyIndexer
  retrievedEvents <-
    Hedgehog.evalExceptT $ Core.query (event ^. Core.point) Core.EventAtQuery indexer
  event ^. Core.event === retrievedEvents

-- | We can retrieve all the events
propRoundTripSpent :: Hedgehog.Property
propRoundTripSpent = Hedgehog.property $ do
  events <- Hedgehog.forAll $ getSpentsEvents <$> Gen.genMockchain
  let filterNonEmpty (Core.Timed _ Nothing) = Nothing
      filterNonEmpty (Core.Timed p (Just utxos)) = Just $ Core.Timed p utxos
      nonEmptyEvents = mapMaybe filterNonEmpty events
  emptyIndexer <- Hedgehog.evalExceptT $ Spent.mkSpentIndexer ":memory:"
  indexer <- Hedgehog.evalExceptT $ Core.indexAll events emptyIndexer
  retrievedEvents <- Hedgehog.evalExceptT $ Core.queryLatest Core.allEvents indexer
  nonEmptyEvents === retrievedEvents

--

-- | On EventAt, the 'SpentIndexer' behaves like a 'ListIndexer'
propActLikeListIndexerOnEventAt :: Hedgehog.Property
propActLikeListIndexerOnEventAt = Hedgehog.property $ do
  events <- Hedgehog.forAll $ getSpentsEvents <$> Gen.genMockchain
  testedEmptyIndexer <- Hedgehog.evalExceptT $ Spent.mkSpentIndexer ":memory:"
  indexer <- Hedgehog.evalExceptT $ Core.indexAll events testedEmptyIndexer
  referenceIndexer <- Core.indexAll events Core.mkListIndexer
  event <- Hedgehog.forAll $ Hedgehog.Gen.element events
  (testedResult :: Maybe (NonEmpty Spent.SpentInfo)) <-
    Hedgehog.evalExceptT $ Core.query (event ^. Core.point) Core.EventAtQuery indexer
  refResult <-
    Hedgehog.evalExceptT $ Core.query (event ^. Core.point) Core.EventAtQuery referenceIndexer
  refResult === testedResult

-- | Standard tripping property for JSON
propTrippingSpentJSON :: Hedgehog.Property
propTrippingSpentJSON = Hedgehog.property $ do
  event <- Hedgehog.forAll genSpent
  Hedgehog.tripping event Aeson.encode Aeson.eitherDecode

-- | Generate a list of events from a mock chain
getSpentsEvents
  :: Gen.Mockchain era
  -> [Core.Timed C.ChainPoint (Maybe (NonEmpty Spent.SpentInfo))]
getSpentsEvents =
  let getTxBody :: C.Tx era -> C.TxBody era
      getTxBody (C.Tx txBody _) = txBody

      getBlockSpentsEvent
        :: Gen.MockBlock era
        -> Core.Timed C.ChainPoint (Maybe (NonEmpty Spent.SpentInfo))
      getBlockSpentsEvent (Gen.MockBlock (C.BlockHeader slotNo blockHeaderHash _) txs) =
        Core.Timed (C.ChainPoint slotNo blockHeaderHash) $
          NonEmpty.nonEmpty $
            concatMap (Spent.getInputs . getTxBody) txs
   in fmap getBlockSpentsEvent

genSpent :: Hedgehog.Gen Spent.SpentInfo
genSpent = Spent.SpentInfo <$> CGen.genTxIn <*> CGen.genTxId
