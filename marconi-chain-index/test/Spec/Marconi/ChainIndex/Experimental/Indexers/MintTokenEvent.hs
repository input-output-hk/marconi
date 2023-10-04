{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.ChainIndex.Experimental.Indexers.MintTokenEvent (
  tests,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Gen qualified as CEGen
import Control.Concurrent qualified as Concurrent
import Control.Lens (over, toListOf, view, (^.))
import Control.Lens.Fold qualified as Lens
import Control.Lens.Traversal qualified as Lens
import Control.Monad (forM, forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Tracer (nullTracer)
import Data.List ((\\))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Gen.Marconi.ChainIndex.Types qualified as Gen
import Hedgehog (Gen, PropertyT, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as H.Gen
import Hedgehog.Range qualified as H.Range
import Marconi.ChainIndex.Experimental.Extract.WithDistance (WithDistance (WithDistance))
import Marconi.ChainIndex.Experimental.Indexers.MintTokenEvent (
  EventType (BurnEventType, MintEventType),
  MintAsset (MintAsset),
  MintAssetRedeemer (MintAssetRedeemer),
  MintTokenBlockEvents (MintTokenBlockEvents),
  MintTokenEvent (MintTokenEvent),
  MintTokenEventConfig (MintTokenEventConfig),
  MintTokenEventLocation (MintTokenEventLocation),
  MintTokenEventsMatchingQuery (MintTokenEventsMatchingQuery),
  QueryByAssetId (QueryByAssetId),
  StandardMintTokenEventIndexer,
  allBurnEvents,
  allMintEvents,
  filterByTargetAssetIds,
  mintAssetAssetId,
  mintAssetPolicyId,
  mintAssetQuantity,
  mintTokenEventAsset,
  mintTokenEvents,
 )
import Marconi.ChainIndex.Experimental.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.ChainIndex.Experimental.Indexers.Worker (
  StandardWorker (StandardWorker),
  StandardWorkerConfig (StandardWorkerConfig),
 )
import Marconi.ChainIndex.Types (TxIndexInBlock (TxIndexInBlock))
import Marconi.Core qualified as Core
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.ChainIndex.Experimental.Indexers.MintTokenEvent"
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
            "Query by AssetIds with upperSlotNo bound returns events only to that upper bound"
            "propQueryWithUpperSlotNoReturnsEventsUpToThatSlot"
            propQueryWithUpperSlotNoReturnsEventsUpToThatSlot
        , testPropertyNamed
            "Query by AssetIds with lowerTxId returns events only in range"
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
    ]

-- | On EventsMatchingQuery, the 'MintTokenEventIndexer' behaves like a 'ListIndexer'.
propActLikeListIndexerOnEventsMatchingQuery :: H.Property
propActLikeListIndexerOnEventsMatchingQuery = H.property $ do
  events <- H.forAll genTimedEvents
  sqlIndexer <- H.evalExceptT $ MintTokenEvent.mkMintTokenIndexer ":memory:" >>= Core.indexAll events
  listIndexer <- Core.indexAll events Core.mkListIndexer
  (actualResult :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest MintTokenEvent.allEvents sqlIndexer
  expectedResult <-
    H.evalExceptT $ Core.queryLatest MintTokenEvent.allEvents listIndexer
  actualResult === expectedResult

-- | On EventsMatchingQuery, the 'UtxoIndexer' behaves like a 'ListIndexer'.
propActLikeListIndexerOnQueryByAssetId :: H.Property
propActLikeListIndexerOnQueryByAssetId = H.property $ do
  events <- H.forAll genTimedEvents
  sqlIndexer <- H.evalExceptT $ MintTokenEvent.mkMintTokenIndexer ":memory:" >>= Core.indexAll events
  listIndexer <- Core.indexAll events Core.mkListIndexer

  (allTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest MintTokenEvent.allEvents sqlIndexer
  let assetIds = getAssetIdsFromTimedEvents allTimedEvents

  eventType <- H.forAll genEventType
  forM_ assetIds $ \case
    C.AdaAssetId -> do
      -- TODO: possibly fill the holes when query impl updated
      let query = QueryByAssetId "" (Just "") eventType Nothing Nothing
      (sqlQueryResult :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
        H.evalExceptT $ Core.queryLatest query sqlIndexer
      (listQueryResult :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
        H.evalExceptT $ Core.queryLatest query listIndexer

      sqlQueryResult === listQueryResult
    C.AssetId policyId assetName -> do
      -- TODO: possibly fill the holes when query impl updated
      let query = QueryByAssetId policyId (Just assetName) eventType Nothing Nothing
      (sqlQueryResult :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
        H.evalExceptT $ Core.queryLatest query sqlIndexer
      (listQueryResult :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
        H.evalExceptT $ Core.queryLatest query listIndexer

      sqlQueryResult === listQueryResult

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
    H.evalExceptT $ Core.queryLatest allMintEvents indexer
  let mintEvents = getMintTokenBlockEvents mintTimedEvents

  (burnTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest allBurnEvents indexer
  let burnEvents = getMintTokenBlockEvents burnTimedEvents

  List.sort allEvents === List.sort (mintEvents ++ burnEvents)

propMintEventsShouldNeverAppearInBurnEvensAndVisVersa :: H.Property
propMintEventsShouldNeverAppearInBurnEvensAndVisVersa = H.property $ do
  events <- H.forAll genTimedEvents
  indexer <- Core.indexAll events Core.mkListIndexer

  let getMintTokenBlockEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents] -> [MintTokenEvent]
      getMintTokenBlockEvents = fmap (view Core.event) . getFlattenedTimedEvents

  (mintTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest allMintEvents indexer
  let mintEvents = getMintTokenBlockEvents mintTimedEvents

  (burnTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest allBurnEvents indexer
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
    H.evalExceptT $ Core.queryLatest allMintEvents indexer
  let mintEvents = getMintTokenBlockEvents mintTimedEvents

  forM_ mintEvents $ \e -> H.assert $ (e ^. mintTokenEventAsset . mintAssetQuantity) > 0

propBurnEventsShouldAlwaysHaveNegativeMintedQuantity :: H.Property
propBurnEventsShouldAlwaysHaveNegativeMintedQuantity = H.property $ do
  events <- H.forAll genTimedEvents
  indexer <- Core.indexAll events Core.mkListIndexer

  let getMintTokenBlockEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents] -> [MintTokenEvent]
      getMintTokenBlockEvents = fmap (view Core.event) . getFlattenedTimedEvents

  (burnTimedEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
    H.evalExceptT $ Core.queryLatest allBurnEvents indexer
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
        -- TODO: possibly fill the holes when query impl updated
        let query = QueryByAssetId "" (Just "") Nothing Nothing Nothing
        timedEvents <- H.evalExceptT $ Core.queryLatest query indexer
        pure $ getFlattenedTimedEvents timedEvents
      C.AssetId policyId assetName -> do
        -- TODO: possibly fill the holes when query impl updated
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
      -- TODO: possibly fill the holes when query impl updated
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
          -- TODO: possibly fill the holes when query impl updated
          let queryByAssetId = QueryByAssetId "" (Just "") eventType Nothing Nothing
          H.evalExceptT $ Core.queryLatest queryByAssetId indexer
        (C.AssetId pid assetName) -> do
          -- TODO: possibly fill the holes when query impl updated
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

  -- Reference point for all queries
  syncPoint <- Core.lastSyncPoint indexer

  -- Condition to ensure enough meaningful cases. Cases with no mint/burn events
  -- cannot be tested properly here, since there is no meaningful upper bound.
  -- Cases where the bounds mean the query returns 0 events are tested.
  H.cover 20 "At least two blocks with mint/burn events" $
    length allEvents > 1

  when (not $ null allEvents) $ do
    let (minPoint, maxPoint) =
          maybe (C.ChainPointAtGenesis, C.ChainPointAtGenesis) id $
            minMaxChainPoints allEvents

    -- TODO: generate a random point between min and max, instead of sampling one from existing.
    sampleEventPoint <- (^. Core.point) <$> H.forAll (H.Gen.element allEvents)

    -- Query should return all elements if upper bound is maxPoint, with no lower bound.
    let queryAll = QueryByAssetId policy Nothing Nothing (C.chainPointToSlotNo maxPoint) Nothing
    (queryAllEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
      H.evalExceptT $ Core.query syncPoint queryAll indexer

    List.sort (getFlattenedTimedEvents queryAllEvents)
      === List.sort (getFlattenedTimedEvents allEvents)

    -- Query should return no elements if provided an upper bound that is earlier than
    -- the first event indexed. Run only when minPoint > 0 to avoid degenerate case.
    let lowerThanMinSlotNo = C.chainPointToSlotNo $ shiftChainPointBackOne minPoint

    when (lowerThanMinSlotNo > Just (C.SlotNo 0)) $ do
      let queryNone = QueryByAssetId policy Nothing Nothing lowerThanMinSlotNo Nothing
      (queryNoneEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
        H.evalExceptT $ Core.query syncPoint queryNone indexer

      queryNoneEvents === []

    -- Query should return all events up to an including the max ChainPoint bound,
    -- when that bound is known to exist in the data. In other words, you get what you
    -- expect from a simple filter on the events by ChainPoint.
    let queryTillPoint = QueryByAssetId policy Nothing Nothing (C.chainPointToSlotNo sampleEventPoint) Nothing
    (queryTillPointEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
      H.evalExceptT $ Core.query syncPoint queryTillPoint indexer
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

  -- Reference point for all queries
  syncPoint <- Core.lastSyncPoint indexer

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
      H.evalExceptT $ Core.query syncPoint queryInRange indexer
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

  -- We take a subset of the AssetIds to track them. If the subset is empty, we track all the
  -- assetIds.
  assetIdSubset <-
    H.forAll $
      H.Gen.filter (not . null) $
        H.Gen.subsequence allAssetIds
  let trackedAssetIds = if null assetIdSubset then allAssetIds else assetIdSubset

  listIndexer <- Core.indexAll events Core.mkListIndexer
  listIndexerEvents <- queryListIndexerEventsMatchingTargetAssetIds trackedAssetIds listIndexer

  sqlIndexer <- indexWithRunner trackedAssetIds events
  sqlIndexerEvents <- H.evalExceptT $ Core.queryLatest MintTokenEvent.allEvents sqlIndexer

  sqlIndexerEvents === listIndexerEvents

-- | Check that an 'mintTokenEventWorker' doesn't track other 'C.AssetId's.
propRunnerDoesntTrackUnselectedAssetId :: H.Property
propRunnerDoesntTrackUnselectedAssetId = H.property $ do
  events <- H.forAll genTimedEvents
  let allAssetIds = getAssetIdsFromTimedEvents $ mapMaybe sequence events

  -- We take a subset of the AssetIds to track them. If the subset is empty, we track all the
  -- assetIds.
  let notAllAddressesPredicate xs = all ($ xs) [not . null, (/= allAssetIds)]
  trackedAssetIds <- H.forAll $ H.Gen.filter notAllAddressesPredicate $ H.Gen.subsequence allAssetIds
  let untrackedAssetIds = allAssetIds \\ trackedAssetIds

  sqlIndexer <- indexWithRunner trackedAssetIds events

  forM_ untrackedAssetIds $ \untrackedAssetId -> do
    let query =
          case untrackedAssetId of
            C.AdaAssetId -> QueryByAssetId "" (Just "") Nothing Nothing Nothing
            C.AssetId policyId assetName -> QueryByAssetId policyId (Just assetName) Nothing Nothing Nothing
    (sqlIndexerEvents :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
      H.evalM $ H.evalExceptT $ Core.queryLatest query sqlIndexer
    sqlIndexerEvents === []

{- | Runs the 'MintTokenEvent' worker while providing 'AssetId's to track and events to index, and
return the resulting indexer.
-}
indexWithRunner
  :: [C.AssetId]
  -> [Core.Timed C.ChainPoint (Maybe MintTokenBlockEvents)]
  -> PropertyT IO (StandardMintTokenEventIndexer IO)
indexWithRunner trackedAssetIds events = do
  StandardWorker indexerVar worker <-
    H.evalExceptT $
      MintTokenEvent.mkMintTokenEventWorker
        (StandardWorkerConfig "MintTokenEventWorker" 1 (Core.CatchupConfig 4 2) pure nullTracer)
        (MintTokenEventConfig trackedAssetIds)
        ":memory:"

  -- We create a coordinator to perform indexing through the worker
  coordinator <- liftIO $ Core.mkCoordinator [worker]
  let eventsWithDistance = fmap (fmap (Just . WithDistance 0)) $ reverse events
  void $ H.evalExceptT $ Core.indexAllDescending eventsWithDistance coordinator
  liftIO $ Concurrent.readMVar indexerVar

-- | Query the events of the 'ListIndexer' and only keep the events that contain tracked AssetId.
queryListIndexerEventsMatchingTargetAssetIds
  :: [C.AssetId]
  -> Core.ListIndexer MintTokenBlockEvents
  -> PropertyT IO [Core.Timed C.ChainPoint MintTokenBlockEvents]
queryListIndexerEventsMatchingTargetAssetIds assetIds indexer = do
  let query = MintTokenEventsMatchingQuery (filterByTargetAssetIds assetIds)
  H.evalExceptT $ Core.queryLatest query indexer

getPolicyIdFromAssetId :: C.AssetId -> C.PolicyId
getPolicyIdFromAssetId C.AdaAssetId = ""
getPolicyIdFromAssetId (C.AssetId pid _) = pid

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
