{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Spec.Marconi.Cardano.Indexers.MintTokenEvent (
  propTests,
  unitTests,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Gen qualified as CEGen
import Control.Concurrent (readMVar, threadDelay)
import Control.Concurrent qualified as Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Exception (throwIO)
import Control.Lens (over, toListOf, view, (^.))
import Control.Lens.Fold qualified as Lens
import Control.Lens.Traversal qualified as Lens
import Control.Monad (forM, forM_, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Tracer (nullTracer)
import Data.Foldable (minimumBy)
import Data.List (maximumBy, (\\))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Word (Word64)
import Hedgehog (Gen, PropertyT, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as H.Gen
import Hedgehog.Range qualified as H.Range
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance (WithDistance))
import Marconi.Cardano.Core.Indexer.Worker (
  StandardWorker (StandardWorker),
  StandardWorkerConfig (StandardWorkerConfig),
 )
import Marconi.Cardano.Core.Indexer.Worker qualified as Core
import Marconi.Cardano.Core.Logger (defaultStdOutLogger, mkMarconiTrace)
import Marconi.Cardano.Core.Runner qualified as Runner
import Marconi.Cardano.Core.Types (SecurityParam, TxIndexInBlock (TxIndexInBlock))
import Marconi.Cardano.Core.Types qualified as Core
import Marconi.Cardano.Indexers (mintBuilder)
import Marconi.Cardano.Indexers.BlockInfo qualified as BlockInfo
import Marconi.Cardano.Indexers.MintTokenEvent (
  EventType (BurnEventType, MintEventType),
  MintAsset (MintAsset),
  MintAssetRedeemer (MintAssetRedeemer),
  MintTokenBlockEvents (MintTokenBlockEvents),
  MintTokenEvent (MintTokenEvent),
  MintTokenEventConfig (MintTokenEventConfig),
  MintTokenEventLocation (MintTokenEventLocation),
  QueryByAssetId (QueryByAssetId),
  StandardMintTokenEventIndexer,
  mintAssetAssetId,
  mintAssetAssetName,
  mintAssetPolicyId,
  mintAssetQuantity,
  mintTokenEventAsset,
  mintTokenEvents,
 )
import Marconi.Cardano.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.Cardano.Indexers.MintTokenEventQuery qualified as MintTokenEventQuery
import Marconi.Core qualified as Core
import Spec.Marconi.Cardano.Indexers.BlockInfo qualified as Test.BlockInfo
import System.FilePath ((</>))
import System.IO.Temp qualified as Tmp
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Gen.Marconi.Cardano.Core.Helpers qualified as Core.Helpers
import Test.Gen.Marconi.Cardano.Core.Mockchain (
  MockBlock (MockBlock),
  MockBlockWithInfo (mockBlockInfoChainTip),
  Mockchain,
  MockchainWithInfo,
  genMockchainWithInfo,
  mockchainWithInfoAsMockchain,
 )
import Test.Gen.Marconi.Cardano.Core.Types qualified as Gen
import Test.Gen.Marconi.Cardano.Indexers.MintTokenEvent qualified as Gen
import Test.Helpers qualified as Helpers
import Test.Integration qualified as Integration
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

{- | Genuine property tests, in which more than one test can be run and
 - options on the number of tests might be configured by the caller.
-}
propTests :: TestTree
propTests =
  testGroup
    "Spec.Marconi.Cardano.Indexers.MintTokenEvent"
    [ testGroup
        "ListIndexer properties"
        [ testPropertyNamed
            "Querying mint and burn events is the same as querying all events"
            "propQueryingMintEventsAndBurnEventsIsTheSameAsQueryingAllEvents"
            propQueryingMintEventsAndBurnEventsIsTheSameAsQueryingAllEvents
        , testPropertyNamed
            "Queried mint events should never appear in queried burn events (and vis-versa)"
            "propMintEventsShouldNeverAppearInBurnEvensAndVisVersa"
            propMintEventsShouldNeverAppearInBurnEvensAndVisVersa
        , testPropertyNamed
            "Queried mint events should always have positive minted quantity"
            "propMintEventsShouldAlwaysHavePositiveMintedQuantity"
            propMintEventsShouldAlwaysHavePositiveMintedQuantity
        , testPropertyNamed
            "Queried burn events should always have negative minted quantity"
            "propBurnEventsShouldAlwaysHaveNegativeMintedQuantity"
            propBurnEventsShouldAlwaysHaveNegativeMintedQuantity
        , testPropertyNamed
            "Querying by all possible AssetIds should be the same as querying everything"
            "propQueryingAllPossibleAssetIdsShouldBeSameAsQueryingEverything"
            propQueryingAllPossibleAssetIdsShouldBeSameAsQueryingEverything
        , testPropertyNamed
            "Querying by all possible PolicyIds should be the same as querying everything"
            "propQueryingAllPossiblePolicyIdsShouldBeSameAsQueryingEverything"
            propQueryingAllPossiblePolicyIdsShouldBeSameAsQueryingEverything
        , testPropertyNamed
            "Querying by AssetIds with only PolicyId should be the same as querying by PolicyId"
            "propQueryByAssetIdWithPolicyIdIsSameAsQueryingByPolicyId"
            propQueryByAssetIdWithPolicyIdIsSameAsQueryingByPolicyId
        , testPropertyNamed
            "QueryByAssetId with upperSlotNo bound returns events only to that upper bound"
            "propQueryWithUpperSlotNoReturnsEventsUpToThatSlot"
            propQueryWithUpperSlotNoReturnsEventsUpToThatSlot
        , testPropertyNamed
            "QueryByAssetId with lowerTxId returns events only in range"
            "propQueryWithLowerTxIdAndUpperSlotNoReturnsEventsInRange"
            propQueryWithLowerTxIdAndUpperSlotNoReturnsEventsInRange
        ]
    , testGroup
        "Model test SQLIndexer queries with reference ListIndexer"
        [ testPropertyNamed
            "EventsMatchingQuery query works as a list indexer"
            "propActLikeListIndexerOnEventsMatchingQuery"
            propActLikeListIndexerOnEventsMatchingQuery
        , testPropertyNamed
            "QueryByAssetId query works as a list indexer"
            "propActLikeListIndexerOnQueryByAssetId"
            propActLikeListIndexerOnQueryByAssetId
        , testPropertyNamed
            "QueryByAssetId query works as a list indexer when given lowerTxId and upperSlotNo"
            "propActLikeListIndexerOnQueryByAssetIdWithUpperLowerBounds"
            propActLikeListIndexerOnQueryByAssetIdWithUpperLowerBounds
        ]
    , testGroup
        "Runner testing"
        [ testPropertyNamed
            "Retrieve an event at a tracked AssetId"
            "propRunnerTracksSelectedAssetId"
            propRunnerTracksSelectedAssetId
        , testPropertyNamed
            "Don't find anything at untrack AssetId"
            "propRunnerDoesntTrackUnselectedAssetId"
            propRunnerDoesntTrackUnselectedAssetId
        ]
    , testGroup
        "Stability"
        [ testPropertyNamed
            "withStability always stable when SecurityParam is 0"
            "propWithStabilityAllStable"
            propWithStabilityAllStable
        , testPropertyNamed
            "withStability all unstable when SecurityParam is greater than upper slot of chain"
            "propWithStabilityAllUnstable"
            propWithStabilityAllUnstable
        , testPropertyNamed
            "Despite the SecurityParam of 0, all results are stable due to upperSlotNo"
            "propWithStabilitySetUsnStable"
            propWithStabilitySetUsnStable
        , testPropertyNamed
            "Despite the high SecurityParam, all results are unstable due to upperSlotNo"
            "propWithStabilitySetUsnUnstable"
            propWithStabilitySetUsnUnstable
        ]
    ]

{- | Unit tests, defined with the Hedgehog API.
 - Tests defined @Hedgehog.'propertyOnce'@ or otherwise with
   a fixed number of test runs that should not be changed.
-}
unitTests :: TestTree
unitTests =
  testGroup
    "Spec.Marconi.Cardano.Indexers.MintTokenEvent"
    [ testGroup
        "End-to-end indexer tests with cardano-node-emulator"
        [ testPropertyNamed
            "Indexing a testnet and then submitting a transaction with a mint event to it has the indexer receive that mint event"
            "endToEndMintTokenEvent"
            endToEndMintTokenEvent
        ]
    ]

-- | On 'EventsMatchingQuery', the 'MintTokenEventIndexer' behaves like a 'ListIndexer'.
propActLikeListIndexerOnEventsMatchingQuery :: H.Property
propActLikeListIndexerOnEventsMatchingQuery = H.property $ do
  events <- H.forAll genTimedEvents
  sqlIndexer <- H.evalExceptT $ MintTokenEvent.mkMintTokenIndexer ":memory:" >>= Core.indexAll events
  listIndexer <- Core.indexAll events Core.mkListIndexer
  (actualResult :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest MintTokenEvent.AllEvents sqlIndexer
  expectedResult <-
    H.evalExceptT $ Core.queryLatest MintTokenEvent.AllEvents listIndexer
  actualResult === expectedResult

-- | On 'QueryByAssetId', the 'MintTokenEventIndexer' behaves like a 'ListIndexer'.
propActLikeListIndexerOnQueryByAssetId :: H.Property
propActLikeListIndexerOnQueryByAssetId = H.property $ do
  events <- H.forAll genTimedEvents
  sqlIndexer <- H.evalExceptT $ MintTokenEvent.mkMintTokenIndexer ":memory:" >>= Core.indexAll events
  listIndexer <- Core.indexAll events Core.mkListIndexer

  (allTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest MintTokenEvent.AllEvents sqlIndexer
  let assetIds = getAssetIdsFromTimedEvents allTimedEvents

  eventType <- H.forAll genEventType
  forM_ assetIds $ \case
    C.AdaAssetId -> do
      let query = QueryByAssetId "" (Just "") eventType Nothing Nothing
      (sqlQueryResult :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
        H.evalExceptT $ Core.queryLatest query sqlIndexer
      (listQueryResult :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
        H.evalExceptT $ Core.queryLatest query listIndexer

      sqlQueryResult === listQueryResult
    C.AssetId policyId assetName -> do
      let query = QueryByAssetId policyId (Just assetName) eventType Nothing Nothing
      (sqlQueryResult :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
        H.evalExceptT $ Core.queryLatest query sqlIndexer
      (listQueryResult :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
        H.evalExceptT $ Core.queryLatest query listIndexer

      sqlQueryResult === listQueryResult

{- | On 'QueryByAssetId', the 'MintTokenEventIndexer' behaves like 'ListIndexer'
 if lowerTxId, upperSlotNo are specified.
-}
propActLikeListIndexerOnQueryByAssetIdWithUpperLowerBounds :: H.Property
propActLikeListIndexerOnQueryByAssetIdWithUpperLowerBounds = H.property $ do
  -- Generate events with all the same policyId, to simplify the query
  -- and since only the temporal information is being tested.
  policy <- H.forAll CGen.genPolicyId
  events <- setPolicyId policy <$> H.forAll genTimedEvents

  listIndexer <- Core.indexAll events Core.mkListIndexer
  (allEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest Core.allEvents listIndexer

  sqlIndexer <- H.evalExceptT $ MintTokenEvent.mkMintTokenIndexer ":memory:" >>= Core.indexAll events

  H.cover 10 "At least two blocks with mint/burn events" $
    length allEvents > 1

  when (not $ null allEvents) $ do
    let (_, maxPoint) =
          maybe (C.ChainPointAtGenesis, C.ChainPointAtGenesis) id $
            minMaxChainPoints allEvents

    -- The query requires the lowerTxId to exist among the events.
    sampleEvent <- H.forAll $ H.Gen.element allEvents

    let txIdGetter =
          Core.event
            . mintTokenEvents
            . Lens.folded
            . MintTokenEvent.mintTokenEventLocation
            . MintTokenEvent.mintTokenEventTxId

    let sampleEventTxId = Lens.firstOf txIdGetter sampleEvent
    let sampleEventPoint = sampleEvent ^. Core.point

    let lowerBound = maybe 0 C.unSlotNo $ C.chainPointToSlotNo sampleEventPoint
    let upperBound = maybe 0 C.unSlotNo $ C.chainPointToSlotNo maxPoint

    sampleUpperSlotNo <- C.SlotNo <$> H.forAll (H.Gen.word64 $ H.Range.linear lowerBound upperBound)

    let query = QueryByAssetId policy Nothing Nothing (Just sampleUpperSlotNo) sampleEventTxId

    (sqlQueryResult :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
      H.evalExceptT $ Core.queryLatest query sqlIndexer
    (listQueryResult :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
      H.evalExceptT $ Core.queryLatest query listIndexer

    List.sort (getFlattenedTimedEvents sqlQueryResult)
      === List.sort (getFlattenedTimedEvents listQueryResult)

propQueryingMintEventsAndBurnEventsIsTheSameAsQueryingAllEvents :: H.Property
propQueryingMintEventsAndBurnEventsIsTheSameAsQueryingAllEvents = H.property $ do
  events <- H.forAll genTimedEvents
  indexer <- Core.indexAll events Core.mkListIndexer

  let getMintTokenBlockEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents] -> [MintTokenEvent]
      getMintTokenBlockEvents = fmap (view Core.event) . getFlattenedTimedEvents

  (allTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest Core.allEvents indexer
  let allEvents = getMintTokenBlockEvents allTimedEvents

  (mintTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest MintTokenEvent.AllMintEvents indexer
  let mintEvents = getMintTokenBlockEvents mintTimedEvents

  (burnTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest MintTokenEvent.AllBurnEvents indexer
  let burnEvents = getMintTokenBlockEvents burnTimedEvents

  List.sort allEvents === List.sort (mintEvents ++ burnEvents)

propMintEventsShouldNeverAppearInBurnEvensAndVisVersa :: H.Property
propMintEventsShouldNeverAppearInBurnEvensAndVisVersa = H.property $ do
  events <- H.forAll genTimedEvents
  indexer <- Core.indexAll events Core.mkListIndexer

  let getMintTokenBlockEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents] -> [MintTokenEvent]
      getMintTokenBlockEvents = fmap (view Core.event) . getFlattenedTimedEvents

  (mintTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest MintTokenEvent.AllMintEvents indexer
  let mintEvents = getMintTokenBlockEvents mintTimedEvents

  (burnTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest MintTokenEvent.AllBurnEvents indexer
  let burnEvents = getMintTokenBlockEvents burnTimedEvents

  forM_ mintEvents $ \e -> H.assert $ List.notElem e burnEvents
  forM_ burnEvents $ \e -> H.assert $ List.notElem e mintEvents

propMintEventsShouldAlwaysHavePositiveMintedQuantity :: H.Property
propMintEventsShouldAlwaysHavePositiveMintedQuantity = H.property $ do
  events <- H.forAll genTimedEvents
  indexer <- Core.indexAll events Core.mkListIndexer

  let getMintTokenBlockEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents] -> [MintTokenEvent]
      getMintTokenBlockEvents = fmap (view Core.event) . getFlattenedTimedEvents

  (mintTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest MintTokenEvent.AllMintEvents indexer
  let mintEvents = getMintTokenBlockEvents mintTimedEvents

  forM_ mintEvents $ \e -> H.assert $ (e ^. mintTokenEventAsset . mintAssetQuantity) > 0

propBurnEventsShouldAlwaysHaveNegativeMintedQuantity :: H.Property
propBurnEventsShouldAlwaysHaveNegativeMintedQuantity = H.property $ do
  events <- H.forAll genTimedEvents
  indexer <- Core.indexAll events Core.mkListIndexer

  let getMintTokenBlockEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents] -> [MintTokenEvent]
      getMintTokenBlockEvents = fmap (view Core.event) . getFlattenedTimedEvents

  (burnTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest MintTokenEvent.AllBurnEvents indexer
  let burnEvents = getMintTokenBlockEvents burnTimedEvents

  forM_ burnEvents $ \e -> H.assert $ (e ^. mintTokenEventAsset . mintAssetQuantity) < 0

propQueryingAllPossibleAssetIdsShouldBeSameAsQueryingEverything :: H.Property
propQueryingAllPossibleAssetIdsShouldBeSameAsQueryingEverything = H.property $ do
  events <- H.forAll genTimedEvents
  indexer <- Core.indexAll events Core.mkListIndexer

  (allTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest Core.allEvents indexer
  let allFlattenedTimedEvents = getFlattenedTimedEvents allTimedEvents

  let assetIds = getAssetIdsFromTimedEvents allTimedEvents

  (combinedTimedEvents :: [Core.Timed C.ChainPoint MintTokenEvent]) <-
    fmap concat <$> forM assetIds $ \case
      C.AdaAssetId -> do
        let query = QueryByAssetId "" (Just "") Nothing Nothing Nothing
        timedEvents <- H.evalExceptT $ Core.queryLatest query indexer
        pure $ getFlattenedTimedEvents timedEvents
      C.AssetId policyId assetName -> do
        let query = QueryByAssetId policyId (Just assetName) Nothing Nothing Nothing
        timedEvents <- H.evalExceptT $ Core.queryLatest query indexer
        pure $ getFlattenedTimedEvents timedEvents

  List.sort combinedTimedEvents === List.sort allFlattenedTimedEvents

propQueryingAllPossiblePolicyIdsShouldBeSameAsQueryingEverything :: H.Property
propQueryingAllPossiblePolicyIdsShouldBeSameAsQueryingEverything = H.property $ do
  events <- H.forAll genTimedEvents
  indexer <- Core.indexAll events Core.mkListIndexer

  (allTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest Core.allEvents indexer
  let allFlattenedTimedEvents = getFlattenedTimedEvents allTimedEvents

  let policyIds = getPolicyIdsFromTimedEvents allTimedEvents

  (combinedTimedEvents :: [Core.Timed C.ChainPoint MintTokenEvent]) <-
    fmap concat <$> forM policyIds $ \policyId -> do
      let query = QueryByAssetId policyId Nothing Nothing Nothing Nothing
      timedEvents <- H.evalExceptT $ Core.queryLatest query indexer
      pure $ getFlattenedTimedEvents timedEvents

  List.sort combinedTimedEvents === List.sort allFlattenedTimedEvents

propQueryByAssetIdWithPolicyIdIsSameAsQueryingByPolicyId :: H.Property
propQueryByAssetIdWithPolicyIdIsSameAsQueryingByPolicyId = H.property $ do
  events <- H.forAll genTimedEvents
  indexer <- Core.indexAll events Core.mkListIndexer

  (allTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest Core.allEvents indexer
  let assetIdsGroupByPolicyId =
        groupByKey getPolicyIdFromAssetId $ getAssetIdsFromTimedEvents allTimedEvents

  eventType <- H.forAll genEventType
  forM_ assetIdsGroupByPolicyId $ \(policyId, assetIds) -> do
    let queryByPolicyId = QueryByAssetId policyId Nothing eventType Nothing Nothing
    timedEventsByPolicyId <- H.evalExceptT $ Core.queryLatest queryByPolicyId indexer

    (timedEventsByAssetIds :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
      fmap concat <$> forM (NonEmpty.toList assetIds) $ \case
        C.AdaAssetId -> do
          let queryByAssetId = QueryByAssetId "" (Just "") eventType Nothing Nothing
          H.evalExceptT $ Core.queryLatest queryByAssetId indexer
        (C.AssetId pid assetName) -> do
          let queryByAssetId = QueryByAssetId pid (Just assetName) eventType Nothing Nothing
          H.evalExceptT $ Core.queryLatest queryByAssetId indexer

    List.sort (getFlattenedTimedEvents timedEventsByPolicyId)
      === List.sort (getFlattenedTimedEvents timedEventsByAssetIds)

{- | Helper for upper/lower query bound properties. Shifts the given chain point
 back one slot, to a min of 0. Genesis and SlotNo 0 untouched. Hash is reused.
-}
shiftChainPointBackOne :: C.ChainPoint -> C.ChainPoint
shiftChainPointBackOne C.ChainPointAtGenesis = C.ChainPointAtGenesis
shiftChainPointBackOne cp@(C.ChainPoint sn h)
  | sn == 0 = cp
  | otherwise = C.ChainPoint (sn - 1) h

{- | Helper for upper/lower query bound properties. Retrieve the (first, last) ChainPoints
 from a list of timed events, if the list is non-empty.
-}
minMaxChainPoints :: [Core.Timed C.ChainPoint event] -> Maybe (C.ChainPoint, C.ChainPoint)
minMaxChainPoints events =
  (,)
    <$> Lens.minimumOf (Lens.folded . Core.point) events
    <*> Lens.maximumOf (Lens.folded . Core.point) events

{- | Helper for upper/lower query bound properties. Since those properties test only
 the temporal components, you can set the PolicyId of all events to be the same arbitrarily.
 That allows making one query to test properties of upper/lower slot bounds.
-}
setPolicyId
  :: C.PolicyId
  -> [Core.Timed C.ChainPoint (Maybe MintTokenBlockEvents)]
  -> [Core.Timed C.ChainPoint (Maybe MintTokenBlockEvents)]
setPolicyId policy = over traversePolicyIds (const policy)
  where
    traversePolicyIds =
      Lens.traverse
        . Core.event
        . Lens.traverse
        . mintTokenEvents
        . Lens.traverse
        . mintTokenEventAsset
        . mintAssetPolicyId

{- | Check that if an upper @C.'SlotNo'@ is provided, only the events up to that bound
 are returned in the query. These tests ignore all other parameters of the query, setting
 the 'PolicyId' arbitrarily to a common value to allow testing the bounds in a single query on
 all indexed events.
-}
propQueryWithUpperSlotNoReturnsEventsUpToThatSlot :: H.Property
propQueryWithUpperSlotNoReturnsEventsUpToThatSlot = H.property $ do
  -- Setup: Create an indexer on events with all the same policy ids
  policy <- H.forAll CGen.genPolicyId
  events <- setPolicyId policy <$> H.forAll genTimedEvents

  indexer <- Core.indexAll events Core.mkListIndexer
  (allEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest Core.allEvents indexer

  -- Condition to ensure enough meaningful cases. Cases with no mint/burn events
  -- cannot be tested properly here, since there is no meaningful upper bound.
  -- Cases where the bounds mean the query returns 0 events are tested.
  H.cover 20 "At least two blocks with mint/burn events" $
    length allEvents > 1

  when (not $ null allEvents) $ do
    let (minPoint, maxPoint) =
          maybe (C.ChainPointAtGenesis, C.ChainPointAtGenesis) id $
            minMaxChainPoints allEvents

    sampleEventPoint <- (^. Core.point) <$> H.forAll (H.Gen.element allEvents)

    -- Query should return all elements if upper bound is maxPoint, with no lower bound.
    let queryAll = QueryByAssetId policy Nothing Nothing (C.chainPointToSlotNo maxPoint) Nothing
    (queryAllEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
      H.evalExceptT $ Core.queryLatest queryAll indexer

    List.sort (getFlattenedTimedEvents queryAllEvents)
      === List.sort (getFlattenedTimedEvents allEvents)

    -- Query should return no elements if provided an upper bound that is earlier than
    -- the first event indexed. Run only when minPoint > 0 to avoid degenerate case.
    let lowerThanMinSlotNo = C.chainPointToSlotNo $ shiftChainPointBackOne minPoint

    when (lowerThanMinSlotNo > Just (C.SlotNo 0)) $ do
      let queryNone = QueryByAssetId policy Nothing Nothing lowerThanMinSlotNo Nothing
      (queryNoneEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
        H.evalExceptT $ Core.queryLatest queryNone indexer

      queryNoneEvents === []

    -- Query should return all events up to an including the max ChainPoint bound,
    -- when that bound is known to exist in the data. In other words, you get what you
    -- expect from a simple filter on the events by ChainPoint.
    let queryTillPoint = QueryByAssetId policy Nothing Nothing (C.chainPointToSlotNo sampleEventPoint) Nothing
    (queryTillPointEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
      H.evalExceptT $ Core.queryLatest queryTillPoint indexer
    let filteredTillPointEvents = List.filter (\e -> e ^. Core.point <= sampleEventPoint) allEvents

    List.sort (getFlattenedTimedEvents queryTillPointEvents)
      === List.sort (getFlattenedTimedEvents filteredTillPointEvents)

{- | Check that if both a lower @C.TxId@ and upper @C.'SlotNo'@ are provided, only the events
 - in the range are are returned in the query. These tests ignore all other parameters of the
 - query, setting the 'PolicyId' arbitrarily to a common value to allow testing the bounds in
 - a single query on all indexed events. The upperSlotNo bound is set to the max of existing
 - events here, but see propertyQueryWithUpperSlotNoReturnsEventsUpToThatSlot.
-}
propQueryWithLowerTxIdAndUpperSlotNoReturnsEventsInRange :: H.Property
propQueryWithLowerTxIdAndUpperSlotNoReturnsEventsInRange = H.property $ do
  -- Setup: Create an indexer on events with all the same policy ids
  policy <- H.forAll CGen.genPolicyId
  events <- setPolicyId policy <$> H.forAll genTimedEvents

  indexer <- Core.indexAll events Core.mkListIndexer
  (allEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest Core.allEvents indexer

  -- Condition to ensure enough meaningful cases. Cases with no mint/burn events
  -- cannot be tested properly here, since there is no meaningful upper bound.
  -- Cases where the bounds mean the query returns 0 events are tested.
  H.cover 20 "At least two blocks with mint/burn events" $
    length allEvents > 1

  when (not $ null allEvents) $ do
    let (_, maxPoint) =
          maybe (C.ChainPointAtGenesis, C.ChainPointAtGenesis) id $
            minMaxChainPoints allEvents

    -- The query requires the lowerTxId to exist among the events.
    sampleEvent <- H.forAll $ H.Gen.element allEvents

    let txIdGetter =
          Core.event
            . mintTokenEvents
            . Lens.folded
            . MintTokenEvent.mintTokenEventLocation
            . MintTokenEvent.mintTokenEventTxId

    let sampleEventTxId = Lens.firstOf txIdGetter sampleEvent
    let sampleEventPoint = sampleEvent ^. Core.point

    -- Query should return elements within the generated (sampleEventPoint, maxPoint) range,
    -- as compared with a simple filter statement.
    let queryInRange = QueryByAssetId policy Nothing Nothing (C.chainPointToSlotNo maxPoint) sampleEventTxId
    (queryInRangeEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
      H.evalExceptT $ Core.queryLatest queryInRange indexer
    let
      inRange e = e ^. Core.point >= sampleEventPoint && e ^. Core.point <= maxPoint
      filteredInRangeEvents = List.filter inRange allEvents

    List.sort (getFlattenedTimedEvents queryInRangeEvents)
      === List.sort (getFlattenedTimedEvents filteredInRangeEvents)

-- | Check that an 'mintTokenEventWorker' tracks the given 'C.AssetId'.
propRunnerTracksSelectedAssetId :: H.Property
propRunnerTracksSelectedAssetId = H.property $ do
  events <- H.forAll genTimedEvents
  let allAssetIds = getAssetIdsFromTimedEvents $ mapMaybe sequence events

  (MintTokenEventConfig trackedAssetIds, _) <- H.forAll $ genTrackedAssetIds allAssetIds

  listIndexer <- Core.indexAll events Core.mkListIndexer
  listIndexerEvents <- queryListIndexerEventsMatchingTargetAssetIds trackedAssetIds listIndexer

  sqlIndexer <- indexWithRunner trackedAssetIds events
  sqlIndexerEvents <- H.evalExceptT $ Core.queryLatest MintTokenEvent.AllEvents sqlIndexer

  sqlIndexerEvents === listIndexerEvents

-- | Check that an 'mintTokenEventWorker' doesn't track other 'C.AssetId's.
propRunnerDoesntTrackUnselectedAssetId :: H.Property
propRunnerDoesntTrackUnselectedAssetId = H.property $ do
  events <- H.forAll genTimedEvents
  let allAssetIds = getAssetIdsFromTimedEvents $ mapMaybe sequence events

  (MintTokenEventConfig trackedAssetIds, untrackedPolicyIds) <-
    H.forAll $ genTrackedAssetIds allAssetIds

  sqlIndexer <- indexWithRunner trackedAssetIds events

  -- If untracked assets is empty, the test does nothing.
  H.cover 50 "Non-empty untracked assets" $ not (null untrackedPolicyIds)

  forM_ untrackedPolicyIds $ \policyid -> do
    let query = QueryByAssetId policyid Nothing Nothing Nothing Nothing
    (sqlIndexerEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
      H.evalM $ H.evalExceptT $ Core.queryLatest query sqlIndexer
    sqlIndexerEvents === []

{- | Runs the 'MintTokenEvent' worker while providing 'AssetId's to track and events to index, and
return the resulting indexer.
-}
indexWithRunner
  :: Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName))
  -> [Core.Timed C.ChainPoint (Maybe MintTokenBlockEvents)]
  -> PropertyT IO (StandardMintTokenEventIndexer IO)
indexWithRunner trackedAssetIds events = do
  StandardWorker indexerVar worker <-
    H.evalExceptT $
      MintTokenEvent.mkMintTokenEventWorker
        (StandardWorkerConfig "MintTokenEventWorker" 1 (Core.mkCatchupConfig 4 2) pure nullTracer)
        (MintTokenEventConfig trackedAssetIds)
        ":memory:"

  -- We create a coordinator to perform indexing through the worker
  coordinator <- liftIO $ Core.mkCoordinator [worker]
  let eventsWithDistance = fmap (fmap (Just . WithDistance 0)) $ reverse events
  void $ H.evalExceptT $ Core.indexAllDescending eventsWithDistance coordinator
  liftIO $ Concurrent.readMVar indexerVar

-- | Query the events of the 'ListIndexer' and only keep the events that contain tracked AssetId.
queryListIndexerEventsMatchingTargetAssetIds
  :: Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName))
  -> Core.ListIndexer MintTokenBlockEvents
  -> PropertyT IO [Core.Timed C.ChainPoint MintTokenBlockEvents]
queryListIndexerEventsMatchingTargetAssetIds assetIds indexer = do
  -- If assetIds is Nothing, query does no filtering (looks for all ids).
  let query = case assetIds of
        Just x -> MintTokenEvent.ByTargetAssetIds (MintTokenEvent.ByTargetAssetIdsArgs x)
        Nothing -> MintTokenEvent.AllEvents
  H.evalExceptT $ Core.queryLatest query indexer

{- | End-to-end test that a single mint event sent over a local network
 - via cardano-node-emulator is received by the indexer.
-}
endToEndMintTokenEvent :: H.Property
endToEndMintTokenEvent = Helpers.unitTestWithTmpDir "." $ \tempPath -> do
  -- Setup
  (trace, _) <- liftIO $ defaultStdOutLogger "endToEndMintTokenEvent"
  let marconiTrace = mkMarconiTrace trace

  -- Local node config and connect info, with slots of length 100ms
  (nscConfig, localNodeConnectInfo) <- liftIO $ Integration.mkLocalNodeInfo tempPath 100

  let
    -- Do no filtering
    mintTokenConfig = MintTokenEvent.MintTokenEventConfig Nothing
    catchupConfig = Integration.mkEndToEndCatchupConfig
    config =
      Integration.mkEndToEndRunIndexerConfig
        marconiTrace
        nscConfig
        Integration.anyTxBodyWithDistancePreprocessor

  -- Create the worker/coordinator
  StandardWorker mindexer worker <-
    H.evalIO $
      either throwIO pure
        =<< runExceptT
          ( mintBuilder
              (config ^. Runner.runIndexerConfigSecurityParam)
              catchupConfig
              mintTokenConfig
              trace
              tempPath
          )
  coordinator <- H.evalIO $ Core.mkCoordinator [worker]

  -- Generate a random MintValue
  txMintValue <- H.forAll Gen.genTxMintValue

  H.evalIO $ Integration.startTestnet nscConfig

  res <- H.evalIO
    $ Async.race
      (Runner.runIndexer config coordinator)
    $ do
      threadDelay 5_000_000

      ledgerPP <- Core.Helpers.getLedgerProtocolParams @C.BabbageEra localNodeConnectInfo

      -- Transaction-builder inputs
      address <- Core.Helpers.nothingFail "Empty knownShelleyAddress" Integration.knownShelleyAddress
      witnessSigningKey <-
        Core.Helpers.nothingFail "Empty knownWitnessSigningKey" Integration.knownWitnessSigningKey
      (txIns, lovelace) <- Core.Helpers.getAddressTxInsValue @C.BabbageEra localNodeConnectInfo address
      let validityRange = Integration.unboundedValidityRange

      let txbody =
            Integration.mkUnbalancedTxBodyContent
              validityRange
              ledgerPP
              txIns
              txIns
              [ Core.Helpers.mkTxOut address $ C.lovelaceToValue lovelace <> Gen.getValueFromTxMintValue txMintValue
              ]
              txMintValue

      -- Submit the transaction
      Integration.validateAndSubmitTx
        localNodeConnectInfo
        ledgerPP
        (Integration.nscNetworkId nscConfig)
        address
        witnessSigningKey
        txbody
        lovelace

      threadDelay 5_000_000

      indexer <- readMVar mindexer

      runExceptT (Core.queryLatest MintTokenEvent.AllMintEvents indexer)
        >>= either throwIO pure

  queryEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents] <- H.leftFail res

  -- Test
  let queryPolicyAssets = List.sort $ getPolicyAssetsFromTimedEvents queryEvents
      inputPolicyAssets = List.sort $ getPolicyAssetsFromTxMintValue txMintValue

  queryPolicyAssets === inputPolicyAssets

{- Utilities -}

getPolicyIdFromAssetId :: C.AssetId -> C.PolicyId
getPolicyIdFromAssetId C.AdaAssetId = ""
getPolicyIdFromAssetId (C.AssetId pid _) = pid

getAssetNameFromAssetId :: C.AssetId -> C.AssetName
getAssetNameFromAssetId C.AdaAssetId = ""
getAssetNameFromAssetId (C.AssetId _ name) = name

getPolicyIdsFromTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents] -> [C.PolicyId]
getPolicyIdsFromTimedEvents =
  List.nub
    . concatMap
      ( toListOf
          ( Core.event
              . mintTokenEvents
              . traverse
              . mintTokenEventAsset
              . mintAssetPolicyId
          )
      )

getAssetIdsFromTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents] -> [C.AssetId]
getAssetIdsFromTimedEvents =
  List.nub
    . concatMap
      ( toListOf
          ( Core.event
              . mintTokenEvents
              . traverse
              . mintTokenEventAsset
              . mintAssetAssetId
          )
      )

groupByKey :: (Ord k) => (a -> k) -> [a] -> [(k, NonEmpty a)]
groupByKey f ls =
  fmap (\xs -> (f $ NonEmpty.head xs, xs)) $
    NonEmpty.groupBy (\x1 x2 -> f x1 == f x2) $
      List.sortOn f ls

getFlattenedTimedEvents
  :: [Core.Timed C.ChainPoint MintTokenBlockEvents]
  -> [Core.Timed C.ChainPoint MintTokenEvent]
getFlattenedTimedEvents = concatMap (NonEmpty.toList . getFlattenedTimedEvent)

-- | Flatten the mint token timed events into each specific mint token event.
getFlattenedTimedEvent
  :: Core.Timed C.ChainPoint MintTokenBlockEvents
  -> NonEmpty (Core.Timed C.ChainPoint MintTokenEvent)
getFlattenedTimedEvent timedEvent =
  fmap
    ( \e ->
        Core.Timed (timedEvent ^. Core.point) e
    )
    $ timedEvent ^. Core.event . mintTokenEvents

getPolicyAssetsFromTimedEvents
  :: [Core.Timed C.ChainPoint MintTokenBlockEvents]
  -> [(C.PolicyId, C.AssetName, C.Quantity)]
getPolicyAssetsFromTimedEvents = map (op . (^. Core.event . mintTokenEventAsset)) . getFlattenedTimedEvents
  where
    op e = (e ^. mintAssetPolicyId, e ^. mintAssetAssetName, e ^. mintAssetQuantity)

-- | Unpack a TxMintValue into its id, name and quantity. Note there can be multiple assets.
getPolicyAssetsFromTxMintValue
  :: C.TxMintValue C.BuildTx C.BabbageEra -> [(C.PolicyId, C.AssetName, C.Quantity)]
getPolicyAssetsFromTxMintValue txMintValue = case txMintValue of
  (C.TxMintValue C.MultiAssetInBabbageEra mintedValues (C.BuildTxWith _policyIdToWitnessMap)) ->
    mapMaybe
      ( \(assetId, quantity) -> case assetId of
          C.AssetId policyId assetName -> Just (policyId, assetName, quantity)
          C.AdaAssetId -> Nothing
      )
      $ C.valueToList mintedValues
  _ -> []

{- Generators -}

genTimedEvents :: Gen [Core.Timed C.ChainPoint (Maybe MintTokenBlockEvents)]
genTimedEvents = do
  cps <-
    H.Gen.frequency
      [ (1, Gen.genChainPoints 0 5)
      , (10, Gen.genChainPoints 5 20)
      ]
  forM cps $ \cp -> Core.Timed cp <$> genMintTokenBlockEvents

genMintTokenBlockEvents :: Gen (Maybe MintTokenBlockEvents)
genMintTokenBlockEvents = do
  events <-
    H.Gen.frequency
      [ (1, H.Gen.list (H.Range.linear 0 2) genMintTokenEvent)
      , (10, H.Gen.list (H.Range.linear 2 10) genMintTokenEvent)
      ]
  pure $ fmap MintTokenBlockEvents $ NonEmpty.nonEmpty events

genMintTokenEvent :: Gen MintTokenEvent
genMintTokenEvent = do
  mintLocation <-
    MintTokenEventLocation
      <$> Gen.genBlockNo
      <*> CEGen.genTxId
      <*> (fmap TxIndexInBlock $ H.Gen.word64 (H.Range.linear 0 100))
  scriptData <- Gen.genSimpleHashableScriptData
  let genMintAssetRedeemer =
        MintAssetRedeemer
          <$> Gen.genSimpleScriptData
          <*> pure (C.hashScriptDataBytes scriptData)
  mintAsset <-
    MintAsset
      <$> CGen.genPolicyId
      <*> CGen.genAssetName
      <*> CGen.genSignedNonZeroQuantity
      <*> H.Gen.maybe genMintAssetRedeemer
  pure $ MintTokenEvent mintLocation mintAsset

genEventType :: Gen (Maybe EventType)
genEventType = do
  H.Gen.maybe $ H.Gen.element [MintEventType, BurnEventType]

{- | Generator for 'MintTokenEventConfig' along with a list of @C.'PolicyId'@ s not selected for
tracking. It is enough to mark untracked assets by policy id, since those must be unique to an asset.
https://developers.cardano.org/docs/native-tokens/minting-nfts/
-}
genTrackedAssetIds :: [C.AssetId] -> Gen (MintTokenEventConfig, [C.PolicyId])
genTrackedAssetIds assetIds = do
  -- Here we let the empty list mark the 'Nothing' case in the config,
  -- for convenience. Therefore, the untrackedPolicyIds must be [] in that case.
  -- nubBy policy id since the input assetIds might have assets with different names
  -- but the same policy id.
  let allAssetIds = List.nubBy (\a1 a2 -> getPolicyIdFromAssetId a1 == getPolicyIdFromAssetId a2) assetIds
  trackedAssetIds <- H.Gen.filter (not . (C.AdaAssetId `elem`)) (H.Gen.subsequence allAssetIds)
  let untrackedPolicyIds = case trackedAssetIds of
        [] -> []
        xs -> map getPolicyIdFromAssetId $ allAssetIds \\ xs

  -- Generate a list of bools to decide randomly whether the name is used or not.
  keepNameIndicator <- H.Gen.list (H.Range.singleton (length trackedAssetIds)) H.Gen.bool

  let toConfigElem assetid isIncluded =
        if isIncluded
          then (getPolicyIdFromAssetId assetid, Just $ getAssetNameFromAssetId assetid)
          else (getPolicyIdFromAssetId assetid, Nothing)

  let config = NonEmpty.nonEmpty $ zipWith toConfigElem trackedAssetIds keepNameIndicator

  pure (MintTokenEventConfig config, untrackedPolicyIds)

data MintTokenIndexers = MintTokenIndexers
  { _mintTokenIndexer
      :: Concurrent.MVar (Core.StandardIndexer IO Core.SQLiteIndexer MintTokenEvent.MintTokenBlockEvents)
  , _blockInfoIndexer
      :: Concurrent.MVar (Core.StandardIndexer IO Core.SQLiteIndexer BlockInfo.BlockInfo)
  }

data UpperSlotNo = Min | Max

-- | Given a @SecurityParam@ of @0@ and no @upperSlotNo@, all events should be @Stable@
propWithStabilityAllStable :: H.Property
propWithStabilityAllStable =
  stabilityTestBase (H.assert . all isStable) Nothing 0

-- | Given a @SecurityParam@ of @1000000@ and no @upperSlotNo@, all events should be @Volatile@
propWithStabilityAllUnstable :: H.Property
propWithStabilityAllUnstable =
  stabilityTestBase (H.assert . not . any isStable) Nothing 1000000

{- | Given a maximal @upperSlotNo@, all events should be @Stable@, despite a @SecurityParam@ of
    @1000000@
-}
propWithStabilitySetUsnStable :: H.Property
propWithStabilitySetUsnStable =
  stabilityTestBase (H.assert . all isStable) (Just Max) 1000000

{- | Given a minimal @upperSlotNo@, all events should be @Volatile@, despite a @SecurityParam@ of
    @0@
-}
propWithStabilitySetUsnUnstable :: H.Property
propWithStabilitySetUsnUnstable =
  stabilityTestBase (H.assert . not . any isStable) (Just Min) 0

-- | A function for building stability tests with the MintTokenEvent.MintTokenEventIndexerCombine
stabilityTestBase
  :: ([Core.Stability (Core.Timed C.ChainPoint MintTokenBlockEvents)] -> PropertyT IO ())
  -> Maybe UpperSlotNo
  -> SecurityParam
  -> H.Property
stabilityTestBase assertion upperSlotNo securityParam = H.property $ do
  policy <- H.forAll CGen.genPolicyId
  events <- H.forAll genMockchainWithInfo
  mintEvents <- H.forAll $ genMintTokenEvents $ mockchainWithInfoAsMockchain events
  let blockEvents = Test.BlockInfo.getBlockInfoEvents events
  let event = maximumBy (comparing $ view Core.point) mintEvents
      point = event ^. Core.point
      usn = do
        upper <- upperSlotNo
        let comp = case upper of
              Min -> minimumBy
              Max -> maximumBy
        let upperPoint = comp (comparing $ view Core.point) blockEvents ^. Core.point
        case upperPoint of
          (C.ChainPoint sn _) -> Just sn
          _ -> Nothing
  ((Right res)) <-
    liftIO $
      runExceptT $
        withIndexer securityParam events $
          Core.query
            point
            ( Core.WithStability
                (MintTokenEvent.QueryByAssetId policy Nothing Nothing usn Nothing)
            )
  assertion res

-- | Generate a list of events from a mock chain
genMintTokenEvents
  :: Mockchain era
  -> Gen [Core.Timed C.ChainPoint (Maybe MintTokenEvent.MintTokenBlockEvents)]
genMintTokenEvents =
  let getTxBody :: C.Tx era -> C.TxBody era
      getTxBody (C.Tx txBody _) = txBody
      getBlockSpentsEvent
        :: MockBlock era
        -> Gen (Core.Timed C.ChainPoint (Maybe MintTokenEvent.MintTokenBlockEvents))
      getBlockSpentsEvent (MockBlock (C.BlockHeader slotNo blockHeaderHash blockNo) txs) = do
        events <- concat <$> traverse (genInputs blockNo . getTxBody) txs
        pure $
          Core.Timed
            (C.ChainPoint slotNo blockHeaderHash)
            ( fmap MintTokenBlockEvents $
                NonEmpty.nonEmpty events
            )
   in traverse getBlockSpentsEvent

genInputs :: C.BlockNo -> C.TxBody era -> Gen [MintTokenEvent]
genInputs
  blockNo
  ( C.TxBody
      C.TxBodyContent
        { C.txIns
        , C.txInsCollateral
        , C.txScriptValidity
        }
    ) =
    let inputs = case C.txScriptValidityToScriptValidity txScriptValidity of
          C.ScriptValid -> fst <$> txIns
          C.ScriptInvalid -> case txInsCollateral of
            C.TxInsCollateralNone -> []
            C.TxInsCollateral _ txins -> txins
        makeLocation :: C.TxIn -> Word64 -> MintTokenEventLocation
        makeLocation (C.TxIn txId _) txIdx = MintTokenEventLocation blockNo txId (Core.TxIndexInBlock txIdx)
        locations = zipWith makeLocation inputs [0 ..]
        genMintAssetRedeemer = do
          scriptData <- Gen.genSimpleHashableScriptData
          MintAssetRedeemer
            <$> Gen.genSimpleScriptData
            <*> pure (C.hashScriptDataBytes scriptData)
        generator =
          MintAsset
            <$> CGen.genPolicyId
            <*> CGen.genAssetName
            <*> CGen.genSignedNonZeroQuantity
            <*> H.Gen.maybe genMintAssetRedeemer
     in do
          assets <- traverse (const generator) locations
          pure $ zipWith MintTokenEvent locations assets

mkMintCombine
  :: SecurityParam
  -> IO
      ( MintTokenIndexers
      , MintTokenEventQuery.MintTokenEventIndexerQuery MintTokenEvent.MintTokenBlockEvents
      )
mkMintCombine securityParam = Tmp.withSystemTempDirectory "testUtxoQuery" $ \dir -> do
  let blockInfoPath = dir </> "blockInfo.db"
  let catchupConfig = Core.mkCatchupConfig 4 2

  Right blockInfoIndexer <- runExceptT $ BlockInfo.mkBlockInfoIndexer blockInfoPath

  blockInfoVar <-
    liftIO $
      Concurrent.newMVar $
        Core.mkStandardIndexerWithFilter
          ( Core.StandardWorkerConfig
              "mintTokenWorker"
              securityParam
              catchupConfig
              (pure . Just)
              nullTracer
          )
          Just
          blockInfoIndexer

  let mintPath = dir </> "mint.db"

  Right mintTokenIndexer <- runExceptT $ MintTokenEvent.mkMintTokenIndexer mintPath

  mintTokenVar <-
    liftIO $
      Concurrent.newMVar $
        Core.mkStandardIndexerWithFilter
          ( Core.StandardWorkerConfig
              "mintTokenWorker"
              securityParam
              catchupConfig
              (pure . Just)
              nullTracer
          )
          Just
          mintTokenIndexer

  let indexers = MintTokenIndexers mintTokenVar blockInfoVar
  let combined = MintTokenEventQuery.MintTokenEventIndexerQuery securityParam mintTokenVar blockInfoVar
  pure (indexers, combined)

withIndexer
  :: SecurityParam
  -> MockchainWithInfo C.BabbageEra
  -> ( MintTokenEventQuery.MintTokenEventIndexerQuery MintTokenEvent.MintTokenBlockEvents
       -> ExceptT
            ( Core.QueryError
                (Core.WithStability (MintTokenEvent.QueryByAssetId MintTokenEvent.MintTokenBlockEvents))
            )
            IO
            a
     )
  -> ExceptT
      ( Core.QueryError
          (Core.WithStability (MintTokenEvent.QueryByAssetId MintTokenEvent.MintTokenBlockEvents))
      )
      IO
      a
withIndexer securityParam events f = do
  (indexers, mintTokenCombine) <- liftIO (mkMintCombine securityParam)
  liftIO $ indexMockchain securityParam indexers events
  res <- f mintTokenCombine
  void $ liftIO $ runExceptT $ closeIndexers indexers
  pure res

indexMockchain
  :: SecurityParam -> MintTokenIndexers -> MockchainWithInfo C.BabbageEra -> IO ()
indexMockchain
  (Core.SecurityParam sp)
  (MintTokenIndexers mintTokenVar blockInfoVar)
  chainWithInfo = do
    let chain = mockchainWithInfoAsMockchain chainWithInfo
        tip = maximum $ fmap mockBlockInfoChainTip chainWithInfo
        chainTipPoint = case tip of
          C.ChainTip (C.SlotNo unSlotNo) bh _ ->
            case toInteger unSlotNo - toInteger sp of
              stable@((> 0) -> True) -> C.ChainPoint (C.SlotNo (fromIntegral stable)) bh
              _ -> C.ChainPointAtGenesis
          _ -> C.ChainPointAtGenesis

        withGen
          :: (Core.Point event ~ C.ChainPoint)
          => Gen [Core.Timed (Core.Point event) (Maybe event)]
          -> Core.StandardIndexer IO Core.SQLiteIndexer event
          -> IO (Core.StandardIndexer IO Core.SQLiteIndexer event)
        withGen eventGenerator idx = do
          events <- H.Gen.sample eventGenerator
          Right res <- runExceptT $ Core.indexAll (fmap (fmap (WithDistance 0 . Just)) <$> events) idx
          Right res' <-
            if isGenesis chainTipPoint
              then pure (Right res)
              else runExceptT $ Core.setLastStablePoint chainTipPoint res
          pure res'

    Concurrent.modifyMVar_ mintTokenVar (withGen (genMintTokenEvents chain))
    Concurrent.modifyMVar_
      blockInfoVar
      ( withGen
          (pure $ Test.BlockInfo.getBlockInfoEvents chainWithInfo)
      )

isStable :: Core.Stability a -> Bool
isStable (Core.Stable _) = True
isStable _ = False

isGenesis :: C.ChainPoint -> Bool
isGenesis C.ChainPointAtGenesis = True
isGenesis _ = False

closeIndexers
  :: (MonadIO m)
  => MintTokenIndexers
  -> m ()
closeIndexers
  (MintTokenIndexers mintTokenVar blockInfoVar) =
    do
      liftIO $ Concurrent.withMVar mintTokenVar Core.close
      liftIO $ Concurrent.withMVar blockInfoVar Core.close
