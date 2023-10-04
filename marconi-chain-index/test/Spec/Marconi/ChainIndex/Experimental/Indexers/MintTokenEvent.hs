{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO: return this after ad-hoc testing in repl
-- module Spec.Marconi.ChainIndex.Experimental.Indexers.MintTokenEvent (
--  tests,
-- ) where
module Spec.Marconi.ChainIndex.Experimental.Indexers.MintTokenEvent where

import Cardano.Api qualified as C
import Control.Concurrent qualified as Concurrent
import Control.Lens (toListOf, view, (^.))
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
  mintTokenEventLocation,
  mintTokenEventTxId,
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
    , testGroup
        "Generator testing"
        [ testPropertyNamed
            "genTimedEvents creates mint/burn events whose TxIds are all the same (PLT-7842)"
            "propGenTimedEventsHaveConstantTxId"
            propGenTimedEventsHaveConstantTxId
        , testPropertyNamed
            "genTxId from cardano-api is degenerate"
            "propTxIdGenIsDegenerate"
            propTxIdGenIsDegenerate
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
      let query = QueryByAssetId "" (Just "") eventType
      (sqlQueryResult :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
        H.evalExceptT $ Core.queryLatest query sqlIndexer
      (listQueryResult :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
        H.evalExceptT $ Core.queryLatest query listIndexer

      sqlQueryResult === listQueryResult
    C.AssetId policyId assetName -> do
      let query = QueryByAssetId policyId (Just assetName) eventType
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
        let query = QueryByAssetId "" (Just "") Nothing
        timedEvents <- H.evalExceptT $ Core.queryLatest query indexer
        pure $ getFlattenedTimedEvents timedEvents
      C.AssetId policyId assetName -> do
        let query = QueryByAssetId policyId (Just assetName) Nothing
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
      let query = QueryByAssetId policyId Nothing Nothing
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
    let queryByPolicyId = QueryByAssetId policyId Nothing eventType
    timedEventsByPolicyId <- H.evalExceptT $ Core.queryLatest queryByPolicyId indexer

    (timedEventsByAssetIds :: [Core.Timed C.ChainPoint MintTokenBlockEvents]) <-
      fmap concat <$> forM (NonEmpty.toList assetIds) $ \case
        C.AdaAssetId -> do
          let queryByAssetId = QueryByAssetId "" (Just "") eventType
          H.evalExceptT $ Core.queryLatest queryByAssetId indexer
        (C.AssetId pid assetName) -> do
          let queryByAssetId = QueryByAssetId pid (Just assetName) eventType
          H.evalExceptT $ Core.queryLatest queryByAssetId indexer

    List.sort (getFlattenedTimedEvents timedEventsByPolicyId)
      === List.sort (getFlattenedTimedEvents timedEventsByAssetIds)

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
            C.AdaAssetId -> QueryByAssetId "" (Just "") Nothing
            C.AssetId policyId assetName -> QueryByAssetId policyId (Just assetName) Nothing
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

-- | Property demonstrating the issue in PLT-7842.
propGenTimedEventsHaveConstantTxId :: H.Property
propGenTimedEventsHaveConstantTxId = H.property $ do
  events <- H.forAll genTimedEvents
  let extractMintEvents = mapMaybe (\te -> te ^. Core.event >>= \es -> pure $ Core.Timed (te ^. Core.point) es) events
  let txGetter = Core.event . mintTokenEventLocation . mintTokenEventTxId
  let txIds = map (^. txGetter) $ getFlattenedTimedEvents extractMintEvents
  let nUnique = length $ List.nub txIds
  H.cover 30 "More than one mint/burn event" $ length extractMintEvents > 1
  when (nUnique > 0) $ nUnique === 1

{- | Root cause of PLT-7842. No need for a property test though.
 https://github.com/input-output-hk/cardano-api/blob/dd8e898e1ce331235952799d49f51ac023693344/cardano-api/gen/Test/Gen/Cardano/Api/Typed.hs#L487
-}
propTxIdGenIsDegenerate :: H.Property
propTxIdGenIsDegenerate = H.property $ do
  txid <- H.forAll CGen.genTxId
  show txid === "\"01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53\""

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
      <*> CGen.genTxId
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
