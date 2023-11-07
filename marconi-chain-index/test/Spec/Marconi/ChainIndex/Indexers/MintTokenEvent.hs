{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.ChainIndex.Indexers.MintTokenEvent (
  tests,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Gen qualified as CEGen
import Control.Concurrent qualified as Concurrent
import Control.Lens (over, toListOf, view, (^.))
import Control.Lens.Fold qualified as Lens
import Control.Lens.Traversal qualified as Lens
import Control.Monad (forM, forM_, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Control.Tracer (nullTracer)
import Data.List ((\\))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Gen.Marconi.ChainIndex.Types qualified as CGenApi
import Gen.Marconi.ChainIndex.Types qualified as Gen
import Hedgehog (Gen, PropertyT, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as H.Gen
import Hedgehog.Range qualified as H.Range
import Marconi.ChainIndex.Extract.WithDistance (WithDistance (WithDistance))
import Marconi.ChainIndex.Indexers.BlockInfo qualified as BlockInfo
import Marconi.ChainIndex.Indexers.MintTokenEvent (
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
  mintAssetPolicyId,
  mintAssetQuantity,
  mintTokenEventAsset,
  mintTokenEvents,
 )
import Marconi.ChainIndex.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.ChainIndex.Indexers.Worker (
  StandardWorker (StandardWorker),
  StandardWorkerConfig (StandardWorkerConfig),
 )
import Marconi.ChainIndex.Indexers.Worker qualified as Core
import Marconi.ChainIndex.Types (SecurityParam, TxIndexInBlock (TxIndexInBlock))
import Marconi.Core qualified as Core
import Spec.Marconi.ChainIndex.Indexers.BlockInfo qualified as Test.BlockInfo
import System.Directory (doesFileExist, removeFile)
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.ChainIndex.Indexers.MintTokenEvent"
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
            "withStability always stable when lsp is `securityParam` > any event"
            "withStabilityAllStable"
            withStabilityAllStable
        , testPropertyNamed
            "withStability all unstable"
            "withStabilityAllUnstable"
            withStabilityAllUnstable
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

genTimedEvents :: Gen [Core.Timed C.ChainPoint (Maybe MintTokenBlockEvents)]
genTimedEvents = do
  cps <-
    H.Gen.frequency
      [ (1, Gen.genChainPoints 0 5)
      , (10, Gen.genChainPoints 5 20)
      ]
  forM cps $ \cp -> Core.Timed cp <$> genMintTokenBlockEvents

genTimedEventsByChainPoints :: [C.ChainPoint] -> Gen [Core.Timed C.ChainPoint MintTokenBlockEvents]
genTimedEventsByChainPoints cps = do
  a <- forM cps $ \cp -> Core.Timed cp <$> genMintTokenBlockEvents
  pure (mapMaybe sequence a)

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

mkMintTokenEventIndexerCombineQuery
  :: (C.ChainPoint -> C.ChainPoint)
  -> SecurityParam
  -> [Core.Timed (Core.Point MintTokenBlockEvents) (Maybe MintTokenBlockEvents)]
  -> [Core.Timed (Core.Point MintTokenBlockEvents) BlockInfo.BlockInfo]
  -> IO
      ( MintTokenIndexers
      , MintTokenEvent.MintTokenEventIndexerCombine MintTokenEvent.MintTokenBlockEvents
      )
mkMintTokenEventIndexerCombineQuery lspModifier securityParam events blockInfoEvents =
  do
    let catchupConfig = Core.mkCatchupConfig 4 2
    let blockInfoPath = ":memory:"
    blockInfoIndexerE <- runExceptT $ BlockInfo.mkBlockInfoIndexer blockInfoPath
    let blockInfoIndexer = case blockInfoIndexerE of
          Left e -> error $ show e
          Right x -> x

    hydratedBlockInfoIndexerE <-
      runExceptT $
        Core.indexAll (fmap Just <$> blockInfoEvents) blockInfoIndexer

    let hydratedBlockInfoIndexer = case hydratedBlockInfoIndexerE of
          Left e -> error $ show e
          Right x -> x

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
            hydratedBlockInfoIndexer
    let mintPath = ":memory:"
    mintTokenIndexerE <- runExceptT $ MintTokenEvent.mkMintTokenIndexer mintPath
    let mintTokenIndexer = case mintTokenIndexerE of
          Left e -> error $ show e
          Right x -> x

    hydratedMintTokenIndexerE <- runExceptT $ Core.indexAll events mintTokenIndexer
    let hydratedMintTokenIndexer = case hydratedMintTokenIndexerE of
          Left e -> error $ show e
          Right x -> x

    lastPoint <- Core.lastSyncPoint hydratedMintTokenIndexer
    Right hydratedMintTokenIndexerWithPoint <-
      runExceptT $
        Core.setLastStablePoint (lspModifier lastPoint) hydratedMintTokenIndexer

    mintTokenVar <-
      liftIO $
        Concurrent.newMVar $
          Core.mkStandardIndexerWithFilter
            (Core.StandardWorkerConfig "mintTokenWorker" securityParam catchupConfig (pure . Just) nullTracer)
            Just
            hydratedMintTokenIndexerWithPoint
    let indexers = MintTokenIndexers mintTokenVar blockInfoVar

    pure (indexers, MintTokenEvent.MintTokenEventIndexerCombine securityParam mintTokenVar blockInfoVar)

genTimed :: Hedgehog.Gen a -> Hedgehog.Gen (Core.Timed C.ChainPoint a)
genTimed gen = Core.Timed <$> CGenApi.genChainPoint <*> gen

withStabilityAllStable :: H.Property
withStabilityAllStable =
  stabilityTestBase guaranteeStability (H.assert . all isStable) 2160
  where
    guaranteeStability :: C.ChainPoint -> C.ChainPoint
    guaranteeStability (C.ChainPoint (C.SlotNo _) bh) = C.ChainPoint (C.SlotNo 1000) bh
    guaranteeStability x = x

withStabilityAllUnstable :: H.Property
withStabilityAllUnstable =
  stabilityTestBase guaranteeVolatility (H.assert . not . any isStable) 2160
  where
    guaranteeVolatility (C.ChainPoint (C.SlotNo _) bh) = C.ChainPoint (C.SlotNo 9) bh
    guaranteeVolatility x = x

stabilityTestBase
  :: (C.ChainPoint -> C.ChainPoint)
  -> ([Core.Stability (Core.Timed C.ChainPoint MintTokenBlockEvents)] -> PropertyT IO ())
  -> SecurityParam
  -> H.Property
stabilityTestBase lspModifier assertion securityParam = H.property $ do
  -- Generate events with all the same policyId, to simplify the query
  -- and since only the temporal information is being tested.
  policy <- H.forAll CGen.genPolicyId
  blockEvents <-
    fmap
      ( filter
          (\b -> not $ isGenesis (b ^. Core.point))
      )
      <$> H.forAll
      $ traverse (const (genTimed Test.BlockInfo.genBlockInfo)) [(0 :: Int) .. 20]

  events <-
    setPolicyId policy . fmap (fmap Just)
      <$> H.forAll (genTimedEventsByChainPoints (fmap (view Core.point) blockEvents))
  (idxs@(MintTokenIndexers mintTokenIndexer _), combined) <-
    liftIO (mkMintTokenEventIndexerCombineQuery lspModifier securityParam events blockEvents)
  idx <- liftIO $ Concurrent.readMVar mintTokenIndexer
  lastPoint <- liftIO $ Core.lastSyncPoint idx
  result <-
    H.evalExceptT $
      Core.query
        lastPoint
        ( Core.WithStability
            (MintTokenEvent.QueryByAssetId policy Nothing Nothing Nothing Nothing)
        )
        combined
  closeIndexers idxs
  liftIO delFile
  assertion result
  where
    delFile = do
      exists <- doesFileExist ":memory:"
      when exists $ removeFile ":memory:" >> putStrLn "Deleted file: :memory:"

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
