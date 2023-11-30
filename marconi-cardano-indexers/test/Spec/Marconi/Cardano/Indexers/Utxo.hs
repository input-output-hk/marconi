{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Cardano.Indexers.Utxo (
  tests,
) where

import Cardano.Api qualified as C
import Control.Concurrent qualified as Concurrent
import Control.Lens ((^.), (^..))
import Control.Lens qualified as Lens
import Control.Monad (join, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Gen qualified
import Hedgehog.Gen qualified as Hedgehog
import Hedgehog.Range qualified
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance (WithDistance))
import Marconi.Cardano.Core.Indexer.Worker (
  StandardWorker (StandardWorker),
  StandardWorkerConfig (StandardWorkerConfig),
 )
import Marconi.Cardano.Core.Logger (nullTracer)
import Marconi.Cardano.Core.Types (TxIndexInBlock (TxIndexInBlock))
import Marconi.Cardano.Indexers.Utxo qualified as Utxo
import Marconi.Core qualified as Core
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Gen
import Test.Gen.Marconi.Cardano.Indexers.Utxo qualified as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.Cardano.Indexers.Utxo"
    [ testGroup
        "Indexer"
        [ testPropertyNamed
            "EventAt query works for utxos"
            "propRoundTripAtSlotUtxo"
            propRoundTripAtSlotUtxo
        , testPropertyNamed
            "UtxoIndexer can retrieve all the utxos it stores"
            "propRoundTripUtxo"
            propRoundTripUtxo
        , testPropertyNamed
            "EventAt query works as a list indexer"
            "propActLikeListIndexerOnEventAt"
            propActLikeListIndexerOnEventAt
        ]
    , testGroup
        "Runner"
        [ testPropertyNamed
            "Retrieve an event at a tracked address"
            "propRunnerTracksSelectedAddress"
            propRunnerTracksSelectedAddress
        , testPropertyNamed
            "Don't find anything at untrack address"
            "propRunnerDoesntTrackUnselectedAddress"
            propRunnerDoesntTrackUnselectedAddress
        ]
    , testPropertyNamed
        "JSON event tripping test"
        "propTrippingUtxoJSON"
        propTrippingUtxoJSON
    ]

-- | We can retrieve the event at a given slot
propRoundTripAtSlotUtxo :: Hedgehog.Property
propRoundTripAtSlotUtxo = Hedgehog.property $ do
  events <- Hedgehog.forAll $ Gen.getTimedUtxosEvents <$> Gen.genMockchain
  event <- Hedgehog.forAll $ Hedgehog.Gen.element events
  emptyIndexer <- Hedgehog.evalExceptT $ Utxo.mkUtxoIndexer ":memory:"
  indexer <- Hedgehog.evalExceptT $ Core.indexAll events emptyIndexer
  retrievedEvents <-
    Hedgehog.evalExceptT $ Core.query (event ^. Core.point) Core.EventAtQuery indexer
  event ^. Core.event === retrievedEvents

-- | We can retrieve all the events
propRoundTripUtxo :: Hedgehog.Property
propRoundTripUtxo = Hedgehog.property $ do
  events <- Hedgehog.forAll $ Gen.getTimedUtxosEvents <$> Gen.genMockchain
  let filterNonEmpty (Core.Timed _ Nothing) = Nothing
      filterNonEmpty (Core.Timed p (Just utxos)) = Just $ Core.Timed p utxos
      nonEmptyEvents = mapMaybe filterNonEmpty events
  emptyIndexer <- Hedgehog.evalExceptT $ Utxo.mkUtxoIndexer ":memory:"
  indexer <- Hedgehog.evalExceptT $ Core.indexAll events emptyIndexer
  retrievedEvents <- Hedgehog.evalExceptT $ Core.queryLatest Core.allEvents indexer
  nonEmptyEvents === retrievedEvents

-- | On EventAt, the 'UtxoIndexer' behaves like a 'ListIndexer'
propActLikeListIndexerOnEventAt :: Hedgehog.Property
propActLikeListIndexerOnEventAt = Hedgehog.property $ do
  events <- Hedgehog.forAll $ Gen.getTimedUtxosEvents <$> Gen.genMockchain
  testedEmptyIndexer <- Hedgehog.evalExceptT $ Utxo.mkUtxoIndexer ":memory:"
  indexer <- Hedgehog.evalExceptT $ Core.indexAll events testedEmptyIndexer
  referenceIndexer <- Core.indexAll events Core.mkListIndexer
  event <- Hedgehog.forAll $ Hedgehog.Gen.element events
  (testedResult :: Maybe (NonEmpty Utxo.Utxo)) <-
    Hedgehog.evalExceptT $ Core.query (event ^. Core.point) Core.EventAtQuery indexer
  refResult <-
    Hedgehog.evalExceptT $ Core.query (event ^. Core.point) Core.EventAtQuery referenceIndexer
  refResult === testedResult

{- | Check that an 'utxoWorker' tracks the given address
TODO Change to look like the 'propRunnerTracksSelectedAssetId' of the MintBurnEvent test (much
shorter test).
-}
propRunnerTracksSelectedAddress :: Hedgehog.Property
propRunnerTracksSelectedAddress = Hedgehog.property $ do
  events <- Hedgehog.forAll Gen.genMockchain
  let utxoEvents = Gen.getTimedUtxosEvents events
      timedEvents = fmap (\evt -> Core.Timed (Gen.extractChainPoint evt) evt) events
      chainAddresses = utxoEvents ^.. traverse . Core.event . traverse . traverse . Utxo.address
      attachDistance dist = Just . WithDistance dist
      eventsWithDistance
        :: [Core.Timed C.ChainPoint (Maybe (WithDistance (Gen.MockBlock C.BabbageEra)))]
      eventsWithDistance = zipWith (fmap . attachDistance) [0 ..] $ reverse timedEvents
      resultAtAddress addr = NonEmpty.nonEmpty . NonEmpty.filter ((== addr) . Lens.view Utxo.address)
      eventAtAddress
        :: C.AddressAny
        -> [Core.Timed C.ChainPoint (Maybe (NonEmpty Utxo.Utxo))]
        -> [Core.Timed C.ChainPoint (NonEmpty Utxo.Utxo)]
      eventAtAddress addr =
        mapMaybe (traverse $ resultAtAddress addr)
          . mapMaybe sequence
  -- we take a subset of the chain addresses to track them
  followedAddresses <-
    Hedgehog.forAll $
      Hedgehog.filter (not . null) $
        Hedgehog.subsequence chainAddresses
  -- if addresses are tracked, we choose one of them, otherwise we pick any address of the chain
  addr <-
    Hedgehog.forAll $
      Hedgehog.element $
        if null followedAddresses then chainAddresses else followedAddresses
  StandardWorker ix w <-
    Hedgehog.evalExceptT $
      Utxo.utxoWorker
        ( StandardWorkerConfig
            "test"
            1
            (Core.mkCatchupConfig 4 2)
            (pure . Gen.getBlockUtxosEvent)
            nullTracer
        )
        (Utxo.UtxoIndexerConfig followedAddresses True)
        ":memory:"
  -- we create a coordinator to perform indexing through the worker
  coordinator <- liftIO $ Core.mkCoordinator [w]
  void $ Hedgehog.evalExceptT $ Core.indexAllDescending eventsWithDistance coordinator
  -- and we read through the indexer reference in the mvar
  indexer <- liftIO $ Concurrent.readMVar ix
  res <-
    Hedgehog.evalExceptT $
      Core.queryLatest (Core.EventsMatchingQuery $ resultAtAddress addr) indexer
  eventAtAddress addr utxoEvents === res

{- | Check that an 'utxoWorker' doesn't track other addresses
TODO Change to look like the 'propRunnerTracksSelectedAssetId' of the MintBurnEvent test.
-}
propRunnerDoesntTrackUnselectedAddress :: Hedgehog.Property
propRunnerDoesntTrackUnselectedAddress = Hedgehog.property $ do
  events <- Hedgehog.forAll Gen.genMockchain
  let utxoEvents = Gen.getTimedUtxosEvents events
      timedEvents = fmap (\evt -> Core.Timed (Gen.extractChainPoint evt) evt) events
      chainAddresses = utxoEvents ^.. traverse . Core.event . traverse . traverse . Utxo.address
      attachDistance dist = Just . WithDistance dist
      eventsWithDistance
        :: [Core.Timed C.ChainPoint (Maybe (WithDistance (Gen.MockBlock C.BabbageEra)))]
      eventsWithDistance = zipWith (fmap . attachDistance) [0 ..] $ reverse timedEvents
      resultAtAddress addr = NonEmpty.nonEmpty . NonEmpty.filter ((== addr) . Lens.view Utxo.address)
  -- we take a subsett of the chain addresses to track them,
  -- at least one address must be untracked
  let notAllAddresses xs = all ($ xs) [not . null, (/= chainAddresses)]
  followedAddresses <-
    Hedgehog.forAll $
      Hedgehog.filter notAllAddresses $
        Hedgehog.subsequence chainAddresses
  -- We choose one of the untracked
  let untrackedAddresses = chainAddresses \\ followedAddresses
  addr <-
    Hedgehog.forAll $ Hedgehog.element untrackedAddresses
  StandardWorker ix w <-
    Hedgehog.evalExceptT $
      Utxo.utxoWorker
        ( StandardWorkerConfig
            "test"
            1
            (Core.mkCatchupConfig 4 2)
            (pure . Gen.getBlockUtxosEvent)
            nullTracer
        )
        (Utxo.UtxoIndexerConfig followedAddresses True)
        ":memory:"
  -- we create a coordinator to perform indexing through the worker
  coordinator <- liftIO $ Core.mkCoordinator [w]
  void $ Hedgehog.evalExceptT $ Core.indexAllDescending eventsWithDistance coordinator
  -- and we read through the indexer reference in the mvar
  indexer <- liftIO $ Concurrent.readMVar ix
  res <-
    Hedgehog.evalExceptT $
      Core.queryLatest (Core.EventsMatchingQuery $ resultAtAddress addr) indexer
  [] === res

-- | Standard tripping property for JSON
propTrippingUtxoJSON :: Hedgehog.Property
propTrippingUtxoJSON = Hedgehog.property $ do
  event <- Hedgehog.forAll Gen.genUtxo
  Hedgehog.tripping event Aeson.encode Aeson.eitherDecode
