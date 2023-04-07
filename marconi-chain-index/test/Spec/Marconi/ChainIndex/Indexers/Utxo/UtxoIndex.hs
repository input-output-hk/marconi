{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Marconi.ChainIndex.Indexers.Utxo.UtxoIndex (tests) where

import Control.Lens (each, filtered, folded, toListOf, (%~), (&), (^.))
import Control.Monad (forM, forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.List.NonEmpty (nonEmpty)
import Data.Map (Map)
import Data.Map qualified
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

import Cardano.Api qualified as C
import Gen.Marconi.ChainIndex.Indexers.Utxo (genShelleyEraUtxoEvents, genTx, genUtxoEvents)
import Gen.Marconi.ChainIndex.Indexers.Utxo qualified as UtxoGen
import Gen.Marconi.ChainIndex.Mockchain (MockBlock (mockBlockChainPoint, mockBlockTxs))
import Gen.Marconi.ChainIndex.Types (genChainPoint, genChainPoints)
import Helpers (addressAnyToShelley)
import Marconi.ChainIndex.Indexers.Utxo (StorableEvent (ueInputs, ueUtxos), StorableQuery (LastSyncPoint),
                                         StorableResult (LastSyncPointResult))
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.TestLib.StorableProperties qualified as StorableProperties
import Marconi.ChainIndex.Types (TargetAddresses)
import Marconi.Core.Storable qualified as Storable

import Hedgehog (Gen, Property, cover, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests = testGroup "Spec.Marconi.ChainIndex.Indexers.Utxo"
      [ testPropertyNamed
        "All queried UTXOs by address should be unspent."
        "allqueryUtxosShouldBeUnspent"
        allqueryUtxosShouldBeUnspent

      , testPropertyNamed
        "Collateral that was produced in a transaction should be returned."
        "propTxOutWhenPhase2ValidationFails"
        propTxOutWhenPhase2ValidationFails

      , testPropertyNamed
        "Collateral TxIn should be indexed only, When Phase-2 validation fails."
        "propTxInWhenPhase2ValidationFails"
        propTxInWhenPhase2ValidationFails

      , testPropertyNamed
        "When there are target addresses, we should store only events at those addresses."
        "propComputeEventsAtAddress"
        propComputeEventsAtAddress

    , testPropertyNamed
        "Roundtrip events to sqlRows transformation property."
        "propRoundTripEventsToRowConversion"
        propRoundTripEventsToRowConversion

    , testPropertyNamed
        "getUtxoEvents with target addresses corresponding to all addresses in generated txs should return the same 'UtxoEvent' as if no target addresses were provided"
        "propUsingAllAddressesOfTxsAsTargetAddressesShouldReturnUtxosAsIfNoFilterWasApplied "
        propUsingAllAddressesOfTxsAsTargetAddressesShouldReturnUtxosAsIfNoFilterWasApplied

    , testPropertyNamed
        "Roundtrip save and retrieve from in-memory store property"
        "propSaveToAndRetrieveFromUtxoInMemoryStore"
        propSaveToAndRetrieveFromUtxoInMemoryStore

    , testPropertyNamed
        "Roundtrip save and retrieve events by address from storage test."
        "propSaveAndRetrieveUtxoEvents"
        propSaveAndRetrieveUtxoEvents

    , testPropertyNamed
        "Save and retrieve events by address and slot number from storage test."
        "propUtxoQueryByAddressAndSlot"
        propUtxoQueryByAddressAndSlot

    , testPropertyNamed
        "The points the indexer can be resumed from should return at least the genesis point"
        "propResumingShouldReturnAtLeastTheGenesisPoint"
        propResumingShouldReturnAtLeastTheGenesisPoint

    , testPropertyNamed
        "The points that indexer can be resumed from should return at least non-genesis point when some data was indexed on disk"
        "propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk"
        propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk

    , testPropertyNamed
        "The points that indexer can be resumed from should return an descending ordered list of points"
        "propResumablePointsShouldBeSortedInDescOrder"
        propResumablePointsShouldBeSortedInDescOrder

    , testPropertyNamed
        "ToJSON/FromJSON roundtrip for UtxoRow"
        "propJsonRoundtripUtxoRow"
        propJsonRoundtripUtxoRow

    , testPropertyNamed
        "Block events computed by getUtxoEvents should be the same as those generated by the event-generator."
        "propGetUtxoEventFromBlock"
        propGetUtxoEventFromBlock

    , testPropertyNamed
        "Querying all slots is the same as querying to the last slot"
        "propUtxoQueryAtLatestPointShouldBeSameAsQueryingAll"
        propUtxoQueryAtLatestPointShouldBeSameAsQueryingAll

    , testGroup "LastSync query"
        [ testPropertyNamed
             "Empty indexer latest sync point is ChainPointAtGenesis"
             "testLastSyncOnFreshIndexer"
             testLastSyncOnFreshIndexer

        , testPropertyNamed
             "Querying the latest chain point should return the slot of the last indexed block"
             "propLastChainPointOnRunningIndexer"
             propLastChainPointOnRunningIndexer

        , testPropertyNamed
             "On a rollback, the latest latest sync point is the one we rollback to if the indexer was ahead of it"
             "propLastChainPointOnRewindedIndexer"
             propLastChainPointOnRewindedIndexer
        ]
    ]

-- | The purpose of test is to make sure All Queried Utxo's are unSpent.
--  The Utxo store consists of:
--  * in-memory store:  UtxoEvents before they're flushed to SQlite
--  * SQL-database store:  UtxoRows that are stored in SQLite
--  In this test, We want to make sure:
--    (1) all utxo query results from SQL-database store are unspent
--    (2) all utxos query results from in-memory store are unspent
--    (3) the edge case where although we satisfy (1) and (2),
--        one or many of the query results from SQLite store may have `Spent` in the in-memory store.
--    (4) furthermore, we want to prove that there is always at least one utxoRow returned from sotre.
--  Point (4) is a consequence of the `genShelleyEraUtxoEvents` specifications:  __there is only Spent for previous generated UtxoEvent__
-- Assumption:  SQLite vacuum is disabled, so that we can accouhnt for generated Spents
-- Note:        We expect this test to fail in this branch.
allqueryUtxosShouldBeUnspent :: Property
allqueryUtxosShouldBeUnspent = property $ do
  events <- forAll genShelleyEraUtxoEvents -- we need to use shelley era addresses only to allow for point (4)
  let numOfEvents = length events
  -- We choose the `depth` such that we can prove the boundery condtions, see point (3).
  -- this will ensure we have adequate coverage where events are in both, in-memory store and SQLite store
  depth <- forAll $ Gen.int (Range.constantFrom (numOfEvents - 1) 1 (numOfEvents + 1))
  Hedgehog.classify "Query both in-memory and storage " $ depth < numOfEvents
  Hedgehog.classify "Query in-memory only" $ depth > numOfEvents

  -- It is crtical that we perform NO vacuum.
  -- With depth at such small numbers, the likelyhood of SQLite vaccume is almost certain in
  indexer <- liftIO (Utxo.open ":memory:" (Utxo.Depth depth) False >>= Storable.insertMany events )
  let
    addressQueries :: [StorableQuery Utxo.UtxoHandle] -- we want to query for all addresses
    addressQueries
      = List.nub
      . fmap (flip Utxo.UtxoByAddress Nothing . Utxo._address)
      . concatMap (Set.toList . Utxo.ueUtxos)
      $ events
  results <- liftIO . traverse (Storable.query Storable.QEverything indexer) $ addressQueries
  let getResult = \case
          Utxo.UtxoResult rs         -> rs
          Utxo.LastSyncPointResult _ -> []
      retrievedUtxoRows :: [Utxo.UtxoRow] = concatMap getResult results
      txinsFromRetrievedUtsoRows :: [C.TxIn]  -- get all the TxIn from quried UtxoRows
        = retrievedUtxoRows & each %~ (\r ->
                                       C.TxIn (r ^. Utxo.urUtxo . Utxo.txId)(r ^. Utxo.urUtxo . Utxo.txIx))
      txInsFromGeneratedEvents :: [C.TxIn]    -- get all the TxIn from quried UtxoRows
        = concatMap (\(Utxo.UtxoEvent _ ins _ ) -> Set.toList ins ) events

  -- A property of the generator is that there is at least one unspent transaction
  -- this property also ensures that the next test will not succeed for the trivila case
  -- when retrievedUtxoRows is an empty list
  Hedgehog.assert (not . null $ retrievedUtxoRows)

-- There should be no `Spent` in the retrieved UtxoRows
  Hedgehog.footnote "Regression test must return at least one Utxo. Utxo's may not have any Spent in the Orig. event"
  Hedgehog.assert
    $ all (== True)
    [u `notElem` txInsFromGeneratedEvents| u <- txinsFromRetrievedUtsoRows]

-- | The property verifies that we
--    * process/store all TxIns for valid transactions
--    * use the collateral TxIns only to balance transactions when phase-2 validation failas
--    * use the collateral TxOutsTxIns
propTxInWhenPhase2ValidationFails :: Property
propTxInWhenPhase2ValidationFails = property $ do
  tx@(C.Tx (C.TxBody C.TxBodyContent {..})_) <- forAll genTx
  cp <- forAll genChainPoint
  let event :: StorableEvent Utxo.UtxoHandle = Utxo.getUtxoEvents Nothing [tx] cp
      computedTxins :: [C.TxIn]
        = fmap (\(Utxo.Spent txid txix _ _) -> C.TxIn txid txix)
        . Utxo.getSpentFrom
        $ event
      expectedTxins :: [C.TxIn] = fmap fst txIns

  case txScriptValidity of
    -- this is the same as script is valid, see https://github.com/input-output-hk/cardano-node/pull/4569
    C.TxScriptValidityNone    ->
      Hedgehog.assert $ all (== True) [u `elem` expectedTxins| u <- computedTxins]
    (C.TxScriptValidity _ C.ScriptValid ) ->
      Hedgehog.assert $ all (== True) [u `elem` expectedTxins| u <- computedTxins]
    (C.TxScriptValidity _ C.ScriptInvalid ) -> do
      case txInsCollateral of
        C.TxInsCollateralNone -> Hedgehog.assert $ null computedTxins
        C.TxInsCollateral _ txinsC_ -> do
          Hedgehog.footnoteShow txReturnCollateral
          let (Utxo.BalanceUtxo _ ins) = Utxo.balanceUtxoFromTx Nothing tx
          -- This property shows collateral TxIns will be processed and balanced
          -- Note: not all collateral txins may be utilized in when phase-2 validation fails
          ins === Set.fromList txinsC_
          -- Note: Post transaction balancing, C.TxIns of Spent,
          -- are a subset of of the Collateral TxIns for the same reason as previous note
          -- empty list of computedTxis is a valid subset of collateral txins
          Hedgehog.assert $ all (== True) [u `elem` ins | u <- computedTxins]
      -- -- we should only return txOut collateral

-- | The property verifies that we when there is
--    * no failure in phase-2 validation, collateral is not used
--    * failure in phase-2 validation, collateral used
propTxOutWhenPhase2ValidationFails :: Property
propTxOutWhenPhase2ValidationFails = property $ do
  (C.Tx (C.TxBody txBodyContent@C.TxBodyContent {..}) _) <- forAll genTx
  let computedTxOuts = Utxo.getTxOutFromTxBodyContent txBodyContent
  case txReturnCollateral of
    C.TxReturnCollateralNone -> Hedgehog.success -- nothing to do here
    C.TxReturnCollateral _ txout -> do
      case txScriptValidity of
        (C.TxScriptValidity _ C.ScriptValid ) ->
          Hedgehog.assert $ txout `notElem` computedTxOuts -- collateral is discarded/returned
        (C.TxScriptValidity _ C.ScriptInvalid ) ->
          Hedgehog.footnoteShow computedTxOuts >>
          [txout] === computedTxOuts -- collateral is the only UTXO
        C.TxScriptValidityNone    ->
          Hedgehog.footnoteShow computedTxOuts >>
          [txout] === computedTxOuts -- collateral is the only UTXO

-- | Round trip UtxoEvents to UtxoRow conversion
-- The purpose of this test is to show that there is a isomorphism between `UtxoRow` and UtxoEvent.
propRoundTripEventsToRowConversion :: Property
propRoundTripEventsToRowConversion  = property $ do
  events <- forAll UtxoGen.genUtxoEvents
  let
    txInsMap :: Map C.SlotNo (Set C.TxIn)
    txInsMap = Data.Map.fromList $ concatMap g  events
    f :: C.SlotNo -> IO (Set C.TxIn)
    f sn = pure $ fromMaybe Set.empty (Data.Map.lookup sn txInsMap)
    g :: StorableEvent Utxo.UtxoHandle -> [(C.SlotNo, Set C.TxIn)]
    g (Utxo.UtxoEvent _ ins (C.ChainPoint sn _)) = [(sn, ins)]
    g _                                          = []

    postGenesisEvents = filter (\e -> C.ChainPointAtGenesis /= Utxo.ueChainPoint e) events
    rows :: [Utxo.UtxoRow]
    rows = concatMap Utxo.eventToRows postGenesisEvents
  computedEvent <- liftIO . Utxo.rowsToEvents f $ rows
  Set.fromList computedEvent === Set.fromList postGenesisEvents

-- | Insert Utxo events in storage, and retreive the events
--   Note:
--   we are intetested in testing the in-memory storage only for this test.

propSaveToAndRetrieveFromUtxoInMemoryStore :: Property
propSaveToAndRetrieveFromUtxoInMemoryStore = property $ do
  events <- forAll UtxoGen.genUtxoEvents
  let depth = Utxo.Depth (length events + 1)
  (storedEvents :: [StorableEvent Utxo.UtxoHandle]) <-
    (liftIO $ Utxo.open ":memory:" depth False) -- don't vacuum sqlite
     >>= liftIO . Storable.insertMany events
     >>= liftIO . Storable.getEvents
  Set.fromList storedEvents === Set.fromList events

-- | Insert Utxo events in storage, and retrieve the events
--   The property we're checking here is:
--    - retrieved at least one unspent utxo
--    - only retrieve unspent utxo's
--    - test `spent` filtering at the boundary of in-memory & disk storage
propSaveAndRetrieveUtxoEvents :: Property
propSaveAndRetrieveUtxoEvents = property $ do
  -- events <- forAll genUtxoEvents'' -- TODO
  events <- forAll genShelleyEraUtxoEvents
  let numOfEvents = length events
  depth <- forAll $ Gen.int (Range.constantFrom (numOfEvents - 1) 1 (numOfEvents + 1))
  indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth) False
             >>= liftIO . Storable.insertMany events
  let
    qs :: [StorableQuery Utxo.UtxoHandle]
    qs = List.nub . fmap (flip Utxo.UtxoByAddress Nothing . Utxo._address) . concatMap (Set.toList . Utxo.ueUtxos) $ events
  results <- liftIO . traverse (Storable.query Storable.QEverything indexer) $ qs
  let getResult = \case
          Utxo.UtxoResult rs         -> rs
          Utxo.LastSyncPointResult _ -> []
      rowsFromStorage :: [Utxo.UtxoRow] = concatMap getResult results
      fromStorageTxIns :: [C.TxIn]
        = rowsFromStorage & each %~ (\r ->
                                       C.TxIn (r ^. Utxo.urUtxo . Utxo.txId)(r ^. Utxo.urUtxo . Utxo.txIx))
      fromEventsTxIns :: [C.TxIn]
        = concatMap (\(Utxo.UtxoEvent _ ins _ ) -> Set.toList ins ) events

  Hedgehog.classify "Query both in-memory and storage " $ depth < numOfEvents
  Hedgehog.classify "Query in-memory only" $ depth > numOfEvents

  -- A property of the generator is that there is at least one unspent transaction
  Hedgehog.assert (not . null $ rowsFromStorage)
-- The result set should only contain `unspent` utxos
  Hedgehog.assert
    $ all (== True)
    [u `notElem` fromEventsTxIns| u <- fromStorageTxIns]


-- Insert Utxo events in storage, and retreive the events by address and slot
-- Note: The property we are checking is:
--   - Insert many events at various chainPoints
--   - Fetch for all the evenent addresses
--   - There should not be any results with ChainPoint > highChainPoint
propUtxoQueryByAddressAndSlot :: Property
propUtxoQueryByAddressAndSlot = property $ do
  highSlotNo <- forAll $ Gen.integral $ Range.constantFrom 7 5 20
  chainPoints :: [C.ChainPoint]  <- forAll $ genChainPoints 2 highSlotNo
  events::[StorableEvent Utxo.UtxoHandle] <- forAll $ forM chainPoints genEventWithShelleyAddressAtChainPoint <&> concat
  let numOfEvents = length events
  depth <- forAll $ Gen.int (Range.constantFrom (numOfEvents - 1) 1 (numOfEvents + 1))
  indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth) False -- don't vacuum sqlite
             >>= liftIO . Storable.insertMany events
  let _slot = getSlot $ chainPoints !! (length chainPoints `div` 2)
      qAddresses
        = List.nub  -- remove duplicate addresses
        . fmap (flip Utxo.UtxoByAddress _slot . Utxo._address)
        . concatMap (Set.toList . Utxo.ueUtxos)
        $ events
  results <- liftIO . traverse (Storable.query Storable.QEverything indexer) $ qAddresses
  let filterResult = \case
          Utxo.UtxoResult rs -> rs
          _other             -> []
      fetchedRows = concatMap filterResult results
      slotNoFromStorage = List.sort . fmap Utxo._urSlotNo $ fetchedRows

  Hedgehog.classify "Query both in-memory and storage " $ depth <= numOfEvents
  Hedgehog.classify "Query in-memory only" $ depth > numOfEvents

  Just (last slotNoFromStorage) === _slot

propUtxoQueryAtLatestPointShouldBeSameAsQueryingAll :: Property
propUtxoQueryAtLatestPointShouldBeSameAsQueryingAll = property $ do
  highSlotNo <- forAll $ Gen.integral $ Range.constantFrom 7 5 20
  chainPoints :: [C.ChainPoint]  <- forAll $ genChainPoints 2 highSlotNo
  events::[StorableEvent Utxo.UtxoHandle] <- forAll $ forM chainPoints genEventWithShelleyAddressAtChainPoint <&> concat
  let numOfEvents = length events
  depth <- forAll $ Gen.int (Range.constantFrom (numOfEvents - 1) 1 (numOfEvents + 1))
  indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth) False -- don't vacuum sqlite
             >>= liftIO . Storable.insertMany events
  let lastSlot = getSlot $ last chainPoints
      qAddressesAtSlot
        = List.nub  -- remove duplicate addresses
        . fmap (flip Utxo.UtxoByAddress lastSlot . Utxo._address)
        . concatMap (Set.toList . Utxo.ueUtxos)
        $ events
      qAddressesAll
        = List.nub  -- remove duplicate addresses
        . fmap (flip Utxo.UtxoByAddress Nothing . Utxo._address)
        . concatMap (Set.toList . Utxo.ueUtxos)
        $ events
  resultAtSlot <-
      liftIO . traverse (Storable.query Storable.QEverything indexer) $ qAddressesAtSlot
  resultAll <-
      liftIO . traverse (Storable.query Storable.QEverything indexer) $ qAddressesAll
  resultAtSlot === resultAll

-- TargetAddresses are the addresses in UTXO that we filter for.
-- Puporse of this test is to filter out utxos that have a different address than those in the TargetAddress list.
propComputeEventsAtAddress :: Property
propComputeEventsAtAddress = property $ do
    event <- head <$> forAll UtxoGen.genUtxoEvents
    let (addresses :: [C.AddressAny]) =
          map Utxo._address $ Set.toList $ Utxo.ueUtxos event
        sameAddressEvents :: [StorableEvent Utxo.UtxoHandle]
        sameAddressEvents =  Utxo.eventsAtAddress (head addresses) Nothing [event]
        targetAddress =  head addresses
        (computedAddresses :: [C.AddressAny])
          = toListOf (folded . Utxo.address)
          . concatMap (Set.toList . Utxo.ueUtxos)
          $ sameAddressEvents
        (actualAddresses :: [C.AddressAny])
          = toListOf (folded
                      . Utxo.address
                      . filtered (== targetAddress) )
            $ Utxo.ueUtxos event
    computedAddresses === actualAddresses

-- | Calling 'Utxo.getUtxoEventos' with target addresses that are extracted from all tx outputs from
-- the initial generated txs should return the same 'UtxoEvent's as if there was no provided target
-- addresses.
propUsingAllAddressesOfTxsAsTargetAddressesShouldReturnUtxosAsIfNoFilterWasApplied ::  Property
propUsingAllAddressesOfTxsAsTargetAddressesShouldReturnUtxosAsIfNoFilterWasApplied = property $ do
    utxoEventsWithTxs <- forAll UtxoGen.genUtxoEventsWithTxs
    forM_ utxoEventsWithTxs $ \(expectedUtxoEvent, block) -> do
        let txs = mockBlockTxs block
            expectedAddresses = mkTargetAddressFromTxs txs
        cover 50 "At least one address is used as a target address"
            $ isJust expectedAddresses
        cover 1 "No target addresses are provided"
            $ isNothing expectedAddresses
        let actualUtxoEvents =
                Utxo.getUtxoEvents expectedAddresses txs (Utxo.ueChainPoint expectedUtxoEvent)
        let filteredExpectedUtxoEvent =
                expectedUtxoEvent
                    { Utxo.ueUtxos =
                        Set.filter (\utxo -> isJust $ addressAnyToShelley $ Utxo._address utxo)
                                   (Utxo.ueUtxos expectedUtxoEvent)
                    }

        -- If the 'expectedUtxoEvent' only contain Byron addresses, then 'filteredExpectedUtxoEvent'
        -- will have an empty set of utxos. In that scenario, the `getUtxoEvents` should not filter
        -- anything, so we just return 'pure ()'.
        if not (null $ Utxo.ueUtxos expectedUtxoEvent) && null (Utxo.ueUtxos filteredExpectedUtxoEvent)
           then pure ()
           else filteredExpectedUtxoEvent === actualUtxoEvents
 where
    mkTargetAddressFromTxs
      :: [C.Tx C.BabbageEra]
      -> Maybe TargetAddresses
    mkTargetAddressFromTxs txs =
        foldMap (\(C.Tx (C.TxBody C.TxBodyContent { C.txOuts}) _) -> mkTargetAddressFromTxOuts txOuts)  txs

    mkTargetAddressFromTxOuts
      :: [C.TxOut C.CtxTx C.BabbageEra]
      -> Maybe TargetAddresses
    mkTargetAddressFromTxOuts txOuts =
        nonEmpty $ mapMaybe (\(C.TxOut addr _ _ _) -> addressAnyToShelley $ Utxo.toAddr addr) txOuts

-- | The property verifies that the 'Storable.resumeFromStorage' call returns at least the
-- 'C.ChainPointAtGenesis' point.
propResumingShouldReturnAtLeastTheGenesisPoint :: Property
propResumingShouldReturnAtLeastTheGenesisPoint = property $ do
    events <- forAll UtxoGen.genUtxoEvents
    depth <- forAll $ Gen.int (Range.linear 1 $ length events)
    indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth) False
            >>= Storable.insertMany events
    StorableProperties.propResumingShouldReturnAtLeastTheGenesisPoint indexer

-- | The property verifies that the 'Storable.resumeFromStorage' call returns at least a point which
-- is not 'C.ChainPointAtGenesis' when some events are inserted on disk.
propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk :: Property
propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk = property $ do
    events <- forAll UtxoGen.genUtxoEvents
    cover 90 "All UtxoEvents have at least one utxo and one spent txout"
        $ isJust
        $ List.find (\ue -> not (Set.null (ueUtxos ue)) && not (Set.null (ueInputs ue))) events
    cover 90 "At least one UtxoEvent with at least one utxo"
        $ isJust
        $ List.find (\ue -> not $ Set.null $ ueUtxos ue) events
    cover 90 "At least one UtxoEvent with at least one spent tx out"
        $ isJust
        $ List.find (\ue -> not $ Set.null $ ueInputs ue) events

    -- We make sure that at least one event is stored on disk
    depth <- forAll $ Gen.int (Range.linear 1 $ length events - 1)

    -- We insert the events in the indexer, but for the test assertions, we discard the events in
    indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth) False -- don't vacuum SQLite
    void $ liftIO $ Storable.insertMany events indexer

    actualResumablePoints <- liftIO $ Storable.resume indexer
    -- TODO Given current implementation, we only expect to be able to resume from events which
    -- contain at least one UTXO. In the future, this should be changed to take into account
    Hedgehog.assert $ length actualResumablePoints >= 2

-- | The property verifies that the 'Storable.resumeFromStorage' call returns a sorted list of chain
-- points in descending order.
propResumablePointsShouldBeSortedInDescOrder :: Property
propResumablePointsShouldBeSortedInDescOrder = property $ do
    events <- forAll UtxoGen.genUtxoEvents
    depth <- forAll $ Gen.int (Range.linear 1 $ length events)

    Hedgehog.classify "Events are in memory only" $ depth >= length events
    Hedgehog.classify "Events on disk and memory" $ depth < length events

    indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth) False -- don't vaccum sqlite
           >>= Storable.insertMany events

    StorableProperties.propResumablePointsShouldBeSortedInDescOrder indexer

propJsonRoundtripUtxoRow :: Property
propJsonRoundtripUtxoRow = property $ do
    utxoEvents <- forAll genUtxoEvents
    let utxoRows = concatMap Utxo.eventToRows utxoEvents
    forM_ utxoRows $ \utxoRow -> Hedgehog.tripping utxoRow Aeson.encode Aeson.decode

-- getUtxoEvens should compute the same event as the event generator
-- This test should prove the getUtxoEvent is correctly computing Unspent Transactions
propGetUtxoEventFromBlock :: Property
propGetUtxoEventFromBlock = property $ do
  utxoEventsWithTxs <- forAll UtxoGen.genUtxoEventsWithTxs
  forM_ utxoEventsWithTxs $ \(expectedUtxoEvent, block) -> do
    let (C.BlockHeader sno hBh _) = mockBlockChainPoint  block
        (txs, cp) :: ([C.Tx C.BabbageEra], C.ChainPoint) = (mockBlockTxs block, C.ChainPoint sno hBh)
        computedEvent = Utxo.getUtxoEvents Nothing txs cp
    length (Utxo.ueUtxos computedEvent)  === length (Utxo.ueUtxos expectedUtxoEvent)
    length (Utxo.ueInputs computedEvent) === length (Utxo.ueInputs expectedUtxoEvent)
    computedEvent === expectedUtxoEvent

genEventWithShelleyAddressAtChainPoint :: C.ChainPoint -> Hedgehog.Gen[Utxo.StorableEvent Utxo.UtxoHandle]
genEventWithShelleyAddressAtChainPoint cp =
 genShelleyEraUtxoEvents <&> fmap (\e -> e {Utxo.ueChainPoint = cp})

testLastSyncOnFreshIndexer :: Property
testLastSyncOnFreshIndexer = property $ do
    indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth 50) False
    result <- liftIO $ Storable.query Storable.QEverything indexer LastSyncPoint
    result === LastSyncPointResult C.ChainPointAtGenesis

propLastChainPointOnRunningIndexer :: Property
propLastChainPointOnRunningIndexer = property $ do
    events <- forAll UtxoGen.genUtxoEvents
    depth <- forAll $ Gen.int (Range.linear 1 $ length events)
    indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth) False
    indexer' <- liftIO $ Storable.insertMany events indexer
    result <- liftIO $ Storable.query Storable.QEverything indexer' LastSyncPoint
    result === LastSyncPointResult (Utxo.ueChainPoint $ last $ init events)

propLastChainPointOnRewindedIndexer :: Property
propLastChainPointOnRewindedIndexer = property $ do
    events <- forAll UtxoGen.genUtxoEvents
    depth <- forAll $ Gen.int (Range.linear 1 $ length events)
    ix <- forAll $ Gen.int (Range.linear 0 (length events - 1))
    let rollbackPoint = Utxo.ueChainPoint (events !! ix)
    let lastestPointPostRollback = if ix == 0
            then C.ChainPointAtGenesis
            else Utxo.ueChainPoint (events !! (ix - 1))
    Hedgehog.annotateShow lastestPointPostRollback
    indexer <- liftIO $ Utxo.open ":memory:" (Utxo.Depth depth) False
    indexer' <- liftIO $ Storable.insertMany events indexer
    Just indexer'' <- liftIO $ Storable.rewind rollbackPoint indexer'
    result <- liftIO $ Storable.query Storable.QEverything indexer'' LastSyncPoint
    result === LastSyncPointResult lastestPointPostRollback

getSlot :: C.ChainPoint -> Maybe C.SlotNo
getSlot C.ChainPointAtGenesis = Nothing
getSlot (C.ChainPoint s _)    = Just s
