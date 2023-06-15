{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}

module Spec.Marconi.ChainIndex.Indexers.Utxo.UtxoIndex (tests) where

import Cardano.Api qualified as C
import Cardano.Slotting.Slot (
  WithOrigin (At, Origin),
 )
import Control.Lens (filtered, folded, toListOf, view, (^.), (^..))
import Control.Monad (forM, forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.List.NonEmpty (nonEmpty)
import Data.Map (Map)
import Data.Map qualified
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Set qualified as Set
import Gen.Marconi.ChainIndex.Indexers.Utxo (genShelleyEraUtxoEvents, genTx, genUtxoEvents)
import Gen.Marconi.ChainIndex.Indexers.Utxo qualified as UtxoGen
import Gen.Marconi.ChainIndex.Mockchain (MockBlock (mockBlockChainPoint, mockBlockTxs))
import Gen.Marconi.ChainIndex.Types qualified as Gen
import Hedgehog (Gen, Property, cover, forAll, property, (/==), (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helpers (addressAnyToShelley)
import Marconi.ChainIndex.Error (raiseException)
import Marconi.ChainIndex.Indexers.Utxo (
  BlockInfo (BlockInfo),
  StorableEvent (ueBlockInfo, ueInputs, ueUtxos),
  StorableQuery (LastSyncedBlockInfoQuery),
  StorableResult (LastSyncedBlockInfoResult),
 )
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types (TargetAddresses, UtxoIndexerConfig (UtxoIndexerConfig), ucEnableUtxoTxOutRef, ucTargetAddresses)
import Marconi.Core.Storable qualified as Storable
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.ChainIndex.Indexers.Utxo"
    [ testPropertyNamed
        "All queried UTXOs by address should be unspent."
        "allQueryUtxosShouldBeUnspent"
        allQueryUtxosShouldBeUnspent
    , testPropertyNamed
        "all queried UTXOs spent in the future have a spent TxId"
        "allQueryUtxosSpentInTheFutureHaveASpentTxId"
        allQueryUtxosSpentInTheFutureHaveASpentTxId
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
        "Supress saving Utxo inLineScript and inLineScriptHash"
        "supressSavingInlineScriptAndInlineScriptHash"
        propSupressSavingInlineScriptAndInlineScriptHash
    , testPropertyNamed
        "Roundtrip save and retrieve from storage store property"
        "propSaveToAndRetrieveFromUtxoInMemoryStore"
        propSaveToAndRetrieveFromUtxoInMemoryStore
    , testPropertyNamed
        "Roundtrip save and retrieve events by address from storage test."
        "propSaveAndRetrieveUtxoEvents"
        propSaveAndRetrieveUtxoEvents
    , testPropertyNamed
        "Save and retrieve events by address and slot number from storage test."
        "propUtxoQueryByAddressAndSlotInterval"
        propUtxoQueryByAddressAndSlotInterval
    , testPropertyNamed
        "Querying for addresses for all slots is the same as querying for address for the last slot"
        "propUtxoQueryAtLatestPointShouldBeSameAsQueryingAll"
        propUtxoQueryAtLatestPointShouldBeSameAsQueryingAll
    , testPropertyNamed
        "The points that indexer can be resumed from should return at least non-genesis point when some data was indexed on disk"
        "propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk"
        propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk
    , testPropertyNamed
        "ToJSON/FromJSON roundtrip for UtxoRow"
        "propJsonRoundtripUtxoRow"
        propJsonRoundtripUtxoRow
    , testPropertyNamed
        "Block events computed by getUtxoEvents should be the same as those generated by the event-generator."
        "propGetUtxoEventFromBlock"
        propGetUtxoEventFromBlock
    , testGroup
        "LastSync query"
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

{- |
  The purpose of test is to make sure All Queried Utxo's are unSpent.
  The Utxo store consists of:
    * in-memory store:  UtxoEvents before they're flushed to SQlite
    * SQL-database store:  UtxoRows that are stored in SQLite

  In this test, We want to make sure:
    * all utxo query results from SQL-database store are unspent
    * all utxos query results from in-memory store are unspent
    * the edge case where although we satisfy (1) and (2),
        one or many of the query results from SQLite store may have `Spent` in the in-memory store.
    * furthermore, we want to prove that there is always at least one utxoRow returned from sotre.
  The last point is a consequence of the `genShelleyEraUtxoEvents` specifications:  __there is only Spent for previous generated UtxoEvent__

 Assumption:  SQLite vacuum is disabled, so that we can account for generated Spents
 Note:        We expect this test to fail in this branch.
-}
allQueryUtxosShouldBeUnspent :: Property
allQueryUtxosShouldBeUnspent = property $ do
  events <- forAll genShelleyEraUtxoEvents -- we need to use shelley era addresses only to allow for point (4)
  let numOfEvents = length events
  -- We choose the `depth` such that we can prove the boundary condtions, see point (3).
  -- this will ensure we have adequate coverage where events are in both, in-memory store and SQLite store
  depth <- forAll $ Gen.int (Range.constantFrom (numOfEvents - 1) 1 (numOfEvents + 1))
  Hedgehog.classify "Query both in-memory and storage " $ depth <= numOfEvents
  Hedgehog.classify "Query in-memory only" $ depth > numOfEvents

  -- It is crtical that we perform NO vacuum.
  -- With depth at such small numbers, the likelihood of SQLite vaccume is almost certain in
  indexer <- liftIO $ raiseException $ Utxo.open ":memory:" (Utxo.Depth depth) False >>= Storable.insertMany events
  let upperBound = maximum $ fmap (Utxo._blockInfoSlotNo . ueBlockInfo) events
      addressQueries :: [StorableQuery Utxo.UtxoHandle] -- we want to query for all addresses
        =
        List.nub
          . fmap
            ( Utxo.QueryUtxoByAddressWrapper
                . flip Utxo.QueryUtxoByAddress (Utxo.LessThanOrEqual upperBound) --  maxBound will overflow. See TODO below
                . view Utxo.address
            )
          . concatMap (Set.toList . Utxo.ueUtxos)
          $ events
  results <- liftIO . raiseException . traverse (Storable.query indexer) $ addressQueries
  let getResult = \case
        Utxo.UtxoResult rs -> rs
        Utxo.LastSyncedBlockInfoResult _ -> []
      retrievedUtxoRows :: [Utxo.UtxoRow] = concatMap getResult results
      txinsFromRetrievedUtsoRows :: [C.TxIn] -- get all the TxIn from quried UtxoRows
        =
        view (Utxo.urUtxo . Utxo.txIn) <$> retrievedUtxoRows
      txInsFromGeneratedEvents :: [C.TxIn] -- get all the TxIn from quried UtxoRows
        =
        concatMap (\(Utxo.UtxoEvent _ ins _) -> Map.keys ins) events

  -- A property of the generator is that there is at least one unspent transaction
  -- this property also ensures that the next test will not succeed for the trivila case
  -- when retrievedUtxoRows is an empty list
  Hedgehog.assert (not . null $ retrievedUtxoRows)

  -- There should be no `Spent` in the retrieved UtxoRows
  Hedgehog.footnote "Regression test must return at least one Utxo. Utxo's may not have any Spent in the Orig. event"
  Hedgehog.assert $ all (`notElem` txInsFromGeneratedEvents) txinsFromRetrievedUtsoRows

allQueryUtxosSpentInTheFutureHaveASpentTxId :: Property
allQueryUtxosSpentInTheFutureHaveASpentTxId = property $ do
  events <- forAll genShelleyEraUtxoEvents -- we need to use shelley era addresses only to allow for point (4)
  let numOfEvents = length events
  -- We choose the `depth` such that we can prove the boundery condtions, see point (3).
  -- this will ensure we have adequate coverage where events are in both, in-memory store and SQLite store
  depth <- forAll $ Gen.int (Range.constantFrom (numOfEvents - 1) 1 (numOfEvents + 1))
  Hedgehog.classify "Query both in-memory and storage " $ depth <= numOfEvents
  Hedgehog.classify "Query in-memory only" $ depth > numOfEvents
  let slots = fmap (Utxo._blockInfoSlotNo . ueBlockInfo) events

  upperBound <- forAll $ Gen.element slots
  let allAddresses =
        List.nub
          . fmap (view Utxo.address)
          . concatMap (Set.toList . Utxo.ueUtxos)
          $ events
      addressQueries :: C.SlotNo -> [StorableQuery Utxo.UtxoHandle] -- we want to query for all addresses
      addressQueries sn =
        Utxo.QueryUtxoByAddressWrapper
          . flip Utxo.QueryUtxoByAddress (Utxo.LessThanOrEqual sn)
          <$> allAddresses
  indexer <- liftIO $ raiseException $ Utxo.open ":memory:" (Utxo.Depth depth) False >>= Storable.insertMany events
  results <- liftIO . raiseException . traverse (Storable.query indexer) $ addressQueries upperBound
  let getResult = \case
        Utxo.UtxoResult rs -> rs
        Utxo.LastSyncedBlockInfoResult _ -> []
      retrievedUtxoRows :: [Utxo.UtxoRow] = concatMap getResult results
      txinsFromRetrievedUtsoRows :: [C.TxIn] -- get all the TxIn from quried UtxoRows
        =
        view (Utxo.urUtxo . Utxo.txIn) <$> retrievedUtxoRows

      getAlreadySpent :: StorableEvent Utxo.UtxoHandle -> [C.TxIn]
      getAlreadySpent (Utxo.UtxoEvent _ ins bi) =
        if upperBound >= Utxo._blockInfoSlotNo bi
          then Map.keys ins
          else []

      getFutureSpent :: StorableEvent Utxo.UtxoHandle -> Set.Set C.TxIn
      getFutureSpent (Utxo.UtxoEvent _ ins bi) =
        if upperBound < Utxo._blockInfoSlotNo bi
          then Map.keysSet ins
          else mempty

      txInsFromGeneratedEvents :: [C.TxIn] -- get all the TxIn from quried UtxoRows
        =
        foldMap getAlreadySpent events

      futureSpentHasSpentTxId :: Utxo.UtxoRow -> Bool
      futureSpentHasSpentTxId u =
        let futureSpents = foldMap getFutureSpent events
            txin = u ^. Utxo.urUtxo . Utxo.txIn
         in txin `Set.notMember` futureSpents || isJust (u ^. Utxo.urSpentInfo)

  -- A property of the generator is that there is at least one unspent transaction
  -- this property also ensures that the next test will not succeed for the trivila case
  -- when retrievedUtxoRows is an empty list
  Hedgehog.footnote
    "Regression test must return at least one Utxo. Utxo's may not have any Spent in the Orig. event"
  Hedgehog.assert (not . null $ retrievedUtxoRows)

  -- There should be no `Spent` in the retrieved UtxoRows
  traverse_ (Hedgehog.assert . flip notElem txInsFromGeneratedEvents) txinsFromRetrievedUtsoRows

  -- Should have spent
  traverse_ (Hedgehog.assert . futureSpentHasSpentTxId) retrievedUtxoRows

{- |
  The property verifies that we
    * process/store all TxIns for valid transactions
    * use the collateral TxIns only to balance transactions when phase-2 validation failas
    * use the collateral TxOutsTxIns
-}
propTxInWhenPhase2ValidationFails :: Property
propTxInWhenPhase2ValidationFails = property $ do
  tx@(C.Tx (C.TxBody C.TxBodyContent{..}) _) <- forAll genTx
  slotNo@(C.SlotNo sn) <- forAll Gen.genSlotNo
  bhh <- forAll Gen.genHashBlockHeader
  let blockInfo = BlockInfo slotNo bhh (C.BlockNo sn) 0 1
      utxoIndexerConfig = UtxoIndexerConfig{ucTargetAddresses = Nothing, ucEnableUtxoTxOutRef = True}
      event :: StorableEvent Utxo.UtxoHandle = Utxo.getUtxoEvents utxoIndexerConfig [tx] blockInfo
      computedTxins :: [C.TxIn] =
        fmap (view Utxo.sTxIn)
          . Utxo.getSpentFrom
          $ event
      expectedTxins :: [C.TxIn] = fmap fst txIns

  case txScriptValidity of
    -- this is the same as script is valid, see https://github.com/input-output-hk/cardano-node/pull/4569
    C.TxScriptValidityNone ->
      Hedgehog.assert $ and [u `elem` expectedTxins | u <- computedTxins]
    (C.TxScriptValidity _ C.ScriptValid) ->
      Hedgehog.assert $ and [u `elem` expectedTxins | u <- computedTxins]
    (C.TxScriptValidity _ C.ScriptInvalid) -> do
      case txInsCollateral of
        C.TxInsCollateralNone -> Hedgehog.assert $ null computedTxins
        C.TxInsCollateral _ txinsC_ -> do
          Hedgehog.footnoteShow txReturnCollateral
          let (Utxo.TxOutBalance _ ins) = Utxo.balanceUtxoFromTx UtxoIndexerConfig{ucTargetAddresses = Nothing, ucEnableUtxoTxOutRef = True} (tx, 0)
          -- This property shows collateral TxIns will be processed and balanced
          -- Note: not all collateral txins may be utilized in when phase-2 validation fails
          Map.keysSet ins === Set.fromList txinsC_
          -- Note: Post transaction balancing, C.TxIns of Spent,
          -- are a subset of of the Collateral TxIns for the same reason as previous note
          -- empty list of computedTxis is a valid subset of collateral txins
          Hedgehog.assert $ and [u `Map.member` ins | u <- computedTxins]

-- -- we should only return txOut collateral

{- |
  The property verifies that we when there is
    * no failure in phase-2 validation, collateral is not used
    * failure in phase-2 validation, collateral used
-}
propTxOutWhenPhase2ValidationFails :: Property
propTxOutWhenPhase2ValidationFails = property $ do
  (C.Tx (C.TxBody txBodyContent@C.TxBodyContent{..}) _) <- forAll genTx
  let computedTxOuts = Utxo.getTxOutFromTxBodyContent txBodyContent
  case txReturnCollateral of
    C.TxReturnCollateralNone -> Hedgehog.success -- nothing to do here
    C.TxReturnCollateral _ txout -> do
      case txScriptValidity of
        (C.TxScriptValidity _ C.ScriptValid) ->
          Hedgehog.assert $ txout `notElem` computedTxOuts -- collateral is discarded/returned
        (C.TxScriptValidity _ C.ScriptInvalid) ->
          Hedgehog.footnoteShow computedTxOuts
            >> [txout]
              === computedTxOuts -- collateral is the only UTXO
        C.TxScriptValidityNone ->
          Hedgehog.footnoteShow computedTxOuts
            >> [txout]
              === computedTxOuts -- collateral is the only UTXO

{- |
  Round trip UtxoEvents to UtxoRow conversion
  The purpose of this test is to show that there is a isomorphism between `UtxoRow` and UtxoEvent.
-}
propRoundTripEventsToRowConversion :: Property
propRoundTripEventsToRowConversion = property $ do
  events <- forAll UtxoGen.genUtxoEvents
  let txInsMap :: Map C.SlotNo (Map C.TxIn C.TxId)
      txInsMap = Data.Map.fromList $ concatMap g events
      f :: C.SlotNo -> IO (Map C.TxIn C.TxId)
      f sn = pure $ fromMaybe Map.empty (Data.Map.lookup sn txInsMap)
      g :: StorableEvent Utxo.UtxoHandle -> [(C.SlotNo, Map C.TxIn C.TxId)]
      g (Utxo.UtxoEvent _ ins bi) = [(Utxo._blockInfoSlotNo bi, ins)]

      rows :: [Utxo.UtxoRow]
      rows = concatMap Utxo.eventToRows events
  computedEvent <- liftIO . Utxo.rowsToEvents f $ rows
  Set.fromList computedEvent === Set.fromList events

{- |
    Insert Utxo events in storage, and retreive the events
    Note, we are intetested in testing the in-memory storage only for this test.
-}
propSaveToAndRetrieveFromUtxoInMemoryStore :: Property
propSaveToAndRetrieveFromUtxoInMemoryStore = property $ do
  events <- forAll UtxoGen.genUtxoEvents
  let depth = Utxo.Depth (length events + 1)
  (storedEvents :: [StorableEvent Utxo.UtxoHandle]) <-
    liftIO $
      raiseException $
        Utxo.open ":memory:" depth False -- don't vacuum sqlite
          >>= Storable.insertMany events
          >>= Storable.getEvents
  Set.fromList storedEvents === Set.fromList events

{- |
  Insert Utxo events in storage,and retrieve the events
  The property we're checking here is:
    * Retrieved at least one unspent utxo, the generators garantees that there is at least one Utxo
    * Utxo query for all addresses should yield the same result for:
        > created after the genesis
        > unSpent before last observed slot no.

    * Results from UtxoAddress within a given SlotNo open interval,should not have any slotNo outside of that interval
    * only retrieve unspent Utxo's
-}
propSaveAndRetrieveUtxoEvents :: Property
propSaveAndRetrieveUtxoEvents = property $ do
  events <- forAll genShelleyEraUtxoEvents
  let numOfEvents = length events
  depth <- forAll $ Gen.int (Range.constantFrom (numOfEvents - 1) 1 (numOfEvents + 1))
  indexer <-
    liftIO $
      raiseException $
        Utxo.open ":memory:" (Utxo.Depth depth) False
          >>= Storable.insertMany events
  let qs = mkUtxoQueries events (Utxo.LessThanOrEqual $ C.SlotNo 20000) --  TODO maxBound use this when PLT-5937 is implmented. See TODO below.
  results <-
    liftIO
      . raiseException
      . traverse (Storable.query indexer)
      $ qs
  let getResult = \case
        Utxo.UtxoResult rs -> rs
        Utxo.LastSyncedBlockInfoResult _ -> []
      rowsFromStorage :: [Utxo.UtxoRow] = concatMap getResult results
      fromStorageTxIns :: [C.TxIn] =
        view (Utxo.urUtxo . Utxo.txIn) <$> rowsFromStorage
      fromEventsTxIns :: [C.TxIn] =
        concatMap (\(Utxo.UtxoEvent _ ins _) -> Map.keys ins) events

  -- A property of the generator is that there is at least one unspent transaction
  Hedgehog.assert (not . null $ rowsFromStorage)
  -- The result set should only contain `unspent` utxos
  Hedgehog.assert $ and [u `notElem` fromEventsTxIns | u <- fromStorageTxIns]

---------------------------------------------------------------------
-- TODO --
-- Uncommenting the TODO section below will fail the test, see ticket PLT-5937
-- Add this to observe the Integer Overflow in the logs:
-- liftIO $ SQL.setTrace (Utxo.hdlConnection (h ^. Storable.handle)) $ Just (Text.IO.appendFile "utxoSQLiteTrace.loger")
-- In summary, Word64 maxBound will cause the Integer overflow as SQL.Integer is signed 64 bit
---------------------------------------------------------------------

{- |
  The property we test here is that:
  Query for utxos for all addresses for the following ChainPoint intervals will yield the same result:
    * From ChainPointAtGenesis onward
    * From that last stored Chainpoint
-}
propUtxoQueryAtLatestPointShouldBeSameAsQueryingAll :: Property
propUtxoQueryAtLatestPointShouldBeSameAsQueryingAll = property $ do
  highSlotNo <- forAll $ Gen.integral $ Range.constantFrom 7 5 20
  upperBoundSlotNo <- forAll $ Gen.word64 (Range.linear 2 highSlotNo)
  blockInfos <- forAll $ forM [1 .. upperBoundSlotNo] $ \slotNo -> do
    BlockInfo (C.SlotNo slotNo)
      <$> Gen.genHashBlockHeader
      <*> pure (C.BlockNo slotNo)
      <*> pure 0
      <*> pure 1
  events :: [StorableEvent Utxo.UtxoHandle] <- forAll $ forM blockInfos genEventWithShelleyAddressAtChainPoint <&> concat
  h <- liftIO $ raiseException $ Utxo.open ":memory:" (Utxo.Depth 1) False -- don't vacuum sqlite
  indexer <- liftIO $ raiseException $ Storable.insertMany events h
  let upperIntervalQuery = mkUtxoQueries events (Utxo.LessThanOrEqual $ C.SlotNo upperBoundSlotNo)
      maxIntervalQuery =
        mkUtxoQueries
          events
          ( Utxo.InRange (C.SlotNo minBound) $
              C.SlotNo 2000
          )
  -- TODO --
  -- Use this to trigger Integer overvflow And SQL trace to observe the overflow
  --  $ C.SlotNo maxBound)

  upperIntervalQueryResult <-
    liftIO
      . raiseException
      . traverse (Storable.query indexer)
      $ upperIntervalQuery
  maxIntervalQueryResult <-
    liftIO
      . raiseException
      . traverse (Storable.query indexer)
      $ maxIntervalQuery

  upperIntervalQueryResult === maxIntervalQueryResult

{- |
  Insert Utxo events in storage, and retreive the events by address and slotNo Interval

  Note: The property we are checking is:

   * Insert many events at various chainPoints
   * retrieving for all addresses starting from genesis, is the same as retrieving for all address prior to last observed SlotNo
   * retrieving all addresses in a given open interval should not have any slotNo outside of the query interval
-}
propUtxoQueryByAddressAndSlotInterval :: Property
propUtxoQueryByAddressAndSlotInterval = property $ do
  highSlotNo <- forAll $ Gen.integral $ Range.constantFrom 10 8 15
  Hedgehog.footnote $ "highSlotNo: " <> show highSlotNo
  upperBoundSlotNo <- forAll $ Gen.word64 (Range.linear 5 highSlotNo)
  Hedgehog.footnote $ "upperBoundSlotNo: " <> show upperBoundSlotNo
  blockInfos <- forAll $ forM [1 .. upperBoundSlotNo] $ \slotNo -> do
    BlockInfo (C.SlotNo slotNo)
      <$> Gen.genHashBlockHeader
      <*> pure (C.BlockNo slotNo)
      <*> pure 0
      <*> pure 1
  forM_
    blockInfos
    ( \blockInfo -> do
        events :: [StorableEvent Utxo.UtxoHandle] <-
          forAll $ genEventWithShelleyAddressAtChainPoint blockInfo
        indexer <-
          liftIO $
            raiseException $
              Utxo.open ":memory:" (Utxo.Depth 1) False
                >>= Storable.insertMany events

        upperBoundInterval <- Hedgehog.evalEither (Utxo.interval Nothing (C.SlotNo upperBoundSlotNo))
        let upperBoundIntervalQuery :: [StorableQuery Utxo.UtxoHandle]
            upperBoundIntervalQuery = mkUtxoQueries events upperBoundInterval

        -- pick a random slotNo less than highPoint
        let C.SlotNo maxLowerBoundSlotNo = Utxo._blockInfoSlotNo blockInfo
        lowerBoundSlotNo <-
          fmap C.SlotNo $ forAll $ Gen.word64 $ Range.linear 0 maxLowerBoundSlotNo

        -- set queryInterval for the open interval [lowerBoundSlotno, upperBoundSlotNo]
        slotnoIntervalOpen <-
          Hedgehog.evalEither (Utxo.interval (Just lowerBoundSlotNo) (C.SlotNo upperBoundSlotNo))

        -- we're tesing for all addresses in all slotNo
        maxInterval <-
          Hedgehog.evalEither (Utxo.interval (Just $ C.SlotNo minBound) (C.SlotNo 2000))
        let maxIntervalQuery :: [StorableQuery Utxo.UtxoHandle]
            maxIntervalQuery = mkUtxoQueries events maxInterval

            openIntervalQuery :: [StorableQuery Utxo.UtxoHandle]
            openIntervalQuery = mkUtxoQueries events slotnoIntervalOpen

            filterResult :: StorableResult Utxo.UtxoHandle -> [Utxo.UtxoRow]
            filterResult = \case
              Utxo.UtxoResult rs -> rs
              _other -> []

        maxIntervalResult <-
          liftIO $ raiseException $ traverse (Storable.query indexer) maxIntervalQuery

        upperBoundIntervalResult <- liftIO . raiseException . traverse (Storable.query indexer) $ upperBoundIntervalQuery

        openIntervalResult <-
          liftIO $ raiseException $ traverse (Storable.query indexer) openIntervalQuery

        let rows :: [Utxo.UtxoRow] = concatMap filterResult openIntervalResult
            cps :: [C.SlotNo] = Set.toList . Set.fromList $ rows ^.. folded . Utxo.urCreationSlotNo
            (retrievedLowSlotNo, retrievedHighSlotNo) = (head cps, last cps)

        -- Show we did not retrieve any slotNo before the queryInterval [low,*]
        Hedgehog.assert (retrievedLowSlotNo >= lowerBoundSlotNo)
        Hedgehog.assert (retrievedHighSlotNo <= C.SlotNo upperBoundSlotNo)

        maxIntervalResult === upperBoundIntervalResult
        maxIntervalResult === openIntervalResult
    )

{- |
  TargetAddresses are the addresses in UTXO that we filter for.
  Puporse of this test is to filter out Utxos that have a different address than those in the TargetAddress list.
-}
propComputeEventsAtAddress :: Property
propComputeEventsAtAddress = property $ do
  event <- head <$> forAll UtxoGen.genUtxoEvents
  let addresses :: [C.AddressAny]
      addresses = map (view Utxo.address) $ Set.toList $ Utxo.ueUtxos event
      addressQuery = Utxo.QueryUtxoByAddress (head addresses) $ Utxo.LessThanOrEqual (C.SlotNo maxBound)
      sameAddressEvents :: Utxo.UtxoByAddressBufferEvents
      sameAddressEvents = Utxo.eventsAtAddress addressQuery [event]
      targetAddress = head addresses
      computedAddresses =
        toListOf (folded . Utxo.address)
          . concatMap (Set.toList . Utxo.ueUtxos)
          . view Utxo.bufferUtxos
          $ sameAddressEvents
      actualAddresses =
        toListOf (folded . Utxo.address . filtered (== targetAddress)) $
          Utxo.ueUtxos event
  computedAddresses === actualAddresses

{- |
We are testing that we can supress saving Utxo inLineScript and inLineScriptHash
using the UtxoIndexerConfig data type.
-}
propSupressSavingInlineScriptAndInlineScriptHash :: Property
propSupressSavingInlineScriptAndInlineScriptHash = property $ do
  utxoEventsWithTxs <- forAll UtxoGen.genUtxoEventsWithTxs
  forM_ utxoEventsWithTxs $ \(expectedUtxoEvent, block) -> do
    let txs = mockBlockTxs block
        expectedAddresses = mkTargetAddressFromTxs txs

        saveRefUtxoIndexerConfig =
          UtxoIndexerConfig
            { ucTargetAddresses = expectedAddresses
            , ucEnableUtxoTxOutRef = True
            }
        noSaveRefUtxoIndexerConfig =
          saveRefUtxoIndexerConfig{ucEnableUtxoTxOutRef = False}

        withSaveScriptRef, withNoSaveScriptRef :: Set.Set Utxo.Utxo
        withSaveScriptRef =
          Utxo.ueUtxos $
            Utxo.getUtxoEvents
              saveRefUtxoIndexerConfig
              txs
              (Utxo.ueBlockInfo expectedUtxoEvent)
        withNoSaveScriptRef =
          Utxo.ueUtxos $
            Utxo.getUtxoEvents
              noSaveRefUtxoIndexerConfig
              txs
              (Utxo.ueBlockInfo expectedUtxoEvent)

    -- there should be some inlineScripts
    Set.filter (\u -> isJust . Utxo._inlineScript $ u) withSaveScriptRef /== Set.empty
    Set.filter (\u -> isJust . Utxo._inlineScriptHash $ u) withSaveScriptRef /== Set.empty

    -- there should be no inlineScript or inlineScriptHash
    Set.filter (\u -> isJust . Utxo._inlineScript $ u) withNoSaveScriptRef === Set.empty
    Set.filter (\u -> isJust . Utxo._inlineScriptHash $ u) withNoSaveScriptRef === Set.empty

{- |
  Calling 'Utxo.getUtxoEventos' with target addresses that are extracted from all tx outputs from
  the initial generated txs should return the same 'UtxoEvent's as if there was no provided target
  addresses.
-}
propUsingAllAddressesOfTxsAsTargetAddressesShouldReturnUtxosAsIfNoFilterWasApplied :: Property
propUsingAllAddressesOfTxsAsTargetAddressesShouldReturnUtxosAsIfNoFilterWasApplied = property $ do
  utxoEventsWithTxs <- forAll UtxoGen.genUtxoEventsWithTxs
  forM_ utxoEventsWithTxs $ \(expectedUtxoEvent, block) -> do
    let txs = mockBlockTxs block
        expectedAddresses = mkTargetAddressFromTxs txs
    cover 50 "At least one address is used as a target address" $
      isJust expectedAddresses
    cover 1 "No target addresses are provided" $
      isNothing expectedAddresses
    let utxoIndexerConfig =
          UtxoIndexerConfig
            { ucTargetAddresses = expectedAddresses
            , ucEnableUtxoTxOutRef = True
            }
        actualUtxoEvents =
          Utxo.getUtxoEvents
            utxoIndexerConfig
            txs
            (Utxo.ueBlockInfo expectedUtxoEvent)
        filteredExpectedUtxoEvent =
          expectedUtxoEvent
            { Utxo.ueUtxos =
                Set.filter
                  (\utxo -> isJust $ addressAnyToShelley $ utxo ^. Utxo.address)
                  (Utxo.ueUtxos expectedUtxoEvent)
            }

    if not (null $ Utxo.ueUtxos expectedUtxoEvent) && null (Utxo.ueUtxos filteredExpectedUtxoEvent)
      then pure ()
      else filteredExpectedUtxoEvent === actualUtxoEvents

mkTargetAddressFromTxs
  :: [C.Tx C.BabbageEra]
  -> Maybe TargetAddresses
mkTargetAddressFromTxs =
  foldMap (\(C.Tx (C.TxBody C.TxBodyContent{C.txOuts}) _) -> mkTargetAddressFromTxOuts txOuts)

mkTargetAddressFromTxOuts
  :: [C.TxOut C.CtxTx C.BabbageEra]
  -> Maybe TargetAddresses
mkTargetAddressFromTxOuts txOuts =
  nonEmpty $ mapMaybe (\(C.TxOut addr _ _ _) -> addressAnyToShelley $ Utxo.toAddr addr) txOuts

{- |
  The property verifies that the 'Storable.resumeFromStorage' call returns at least a point which
  is not 'C.ChainPointAtGenesis' when some events are inserted on disk.
-}
propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk :: Property
propResumingShouldReturnAtLeastOneNonGenesisPointIfStoredOnDisk = property $ do
  events <- forAll UtxoGen.genUtxoEvents
  cover 90 "All UtxoEvents have at least one utxo and one spent txout" $
    isJust $
      List.find (\ue -> not (Set.null (ueUtxos ue)) && not (Map.null (ueInputs ue))) events
  cover 90 "At least one UtxoEvent with at least one utxo" $
    isJust $
      List.find (\ue -> not $ Set.null $ ueUtxos ue) events
  cover 90 "At least one UtxoEvent with at least one spent tx out" $
    isJust $
      List.find (\ue -> not $ Map.null $ ueInputs ue) events

  depth <- forAll $ Gen.int (Range.linear 1 $ length events - 1)

  -- We insert the events in the indexer, but for the test assertions, we discard the events in
  indexer <- liftIO $ raiseException $ Utxo.open ":memory:" (Utxo.Depth depth) False
  void $ liftIO $ raiseException $ Storable.insertMany events indexer

  latestResumablePoint <- liftIO $ raiseException $ Storable.resume indexer
  -- TODO Given current implementation, we only expect to be able to resume from events which
  -- contain at least one UTXO. In the future, this should be changed to take into account
  Hedgehog.assert $ latestResumablePoint /= C.ChainPointAtGenesis

propJsonRoundtripUtxoRow :: Property
propJsonRoundtripUtxoRow = property $ do
  utxoEvents <- forAll genUtxoEvents
  let utxoRows = concatMap Utxo.eventToRows utxoEvents
  forM_ utxoRows $ \utxoRow -> Hedgehog.tripping utxoRow Aeson.encode Aeson.decode

{- |
  getUtxoEvens should compute the same event as the event generator
  This test should prove the getUtxoEvent is correctly computing Unspent Transactions
-}
propGetUtxoEventFromBlock :: Property
propGetUtxoEventFromBlock = property $ do
  utxoEventsWithTxs <- forAll UtxoGen.genUtxoEventsWithTxs
  forM_ utxoEventsWithTxs $ \(expectedUtxoEvent, block) -> do
    let (C.BlockHeader sno bhh bn) = mockBlockChainPoint block
        txs = mockBlockTxs block
        bi = BlockInfo sno bhh bn 0 1
        utxoIndexerConfig = UtxoIndexerConfig{ucTargetAddresses = Nothing, ucEnableUtxoTxOutRef = True}
        computedEvent = Utxo.getUtxoEvents utxoIndexerConfig txs bi
    length (Utxo.ueUtxos computedEvent) === length (Utxo.ueUtxos expectedUtxoEvent)
    length (Utxo.ueInputs computedEvent) === length (Utxo.ueInputs expectedUtxoEvent)
    computedEvent === expectedUtxoEvent

genEventWithShelleyAddressAtChainPoint :: BlockInfo -> Hedgehog.Gen [Utxo.StorableEvent Utxo.UtxoHandle]
genEventWithShelleyAddressAtChainPoint bi =
  genShelleyEraUtxoEvents <&> fmap (\e -> e{Utxo.ueBlockInfo = bi})

testLastSyncOnFreshIndexer :: Property
testLastSyncOnFreshIndexer = property $ do
  indexer <- liftIO $ raiseException $ Utxo.open ":memory:" (Utxo.Depth 50) False
  result <- liftIO $ raiseException $ Storable.query indexer LastSyncedBlockInfoQuery
  result === LastSyncedBlockInfoResult Origin

propLastChainPointOnRunningIndexer :: Property
propLastChainPointOnRunningIndexer = property $ do
  events <- forAll UtxoGen.genUtxoEvents
  depth <- forAll $ Gen.int (Range.linear 1 $ length events)
  indexer <- liftIO $ raiseException $ Utxo.open ":memory:" (Utxo.Depth depth) False
  indexer' <- liftIO $ raiseException $ Storable.insertMany events indexer
  result <- liftIO $ raiseException $ Storable.query indexer' LastSyncedBlockInfoQuery
  let beforeLastEvent = last $ init events
  let beforeLastBlockInfo = ueBlockInfo beforeLastEvent
  result === LastSyncedBlockInfoResult (At beforeLastBlockInfo)

propLastChainPointOnRewindedIndexer :: Property
propLastChainPointOnRewindedIndexer = property $ do
  events <- forAll UtxoGen.genUtxoEvents
  depth <- forAll $ Gen.int (Range.linear 1 $ length events)
  ix <- forAll $ Gen.int (Range.linear 0 (length events - 1))
  let eventToRollbackTo = events !! ix
      rollbackPoint =
        C.ChainPoint
          (Utxo._blockInfoSlotNo $ Utxo.ueBlockInfo eventToRollbackTo)
          (Utxo._blockInfoBlockHeaderHash $ Utxo.ueBlockInfo eventToRollbackTo)
      latestEventPostRollback = events !! (ix - 1)
      lastestBlockInfoPostRollback =
        if ix == 0
          then Origin
          else At $ Utxo.ueBlockInfo latestEventPostRollback
  indexer <- liftIO $ raiseException $ Utxo.open ":memory:" (Utxo.Depth depth) False
  indexer' <- liftIO $ raiseException $ Storable.insertMany events indexer
  indexer'' <- liftIO $ raiseException $ Storable.rewind rollbackPoint indexer'
  result <- liftIO $ raiseException $ Storable.query indexer'' LastSyncedBlockInfoQuery
  result === LastSyncedBlockInfoResult lastestBlockInfoPostRollback

-- | make unique queries
mkUtxoQueries
  :: [StorableEvent Utxo.UtxoHandle]
  -> Utxo.Interval C.SlotNo
  -> [StorableQuery Utxo.UtxoHandle]
mkUtxoQueries events slotInterval =
  let qAddresses :: [C.AddressAny]
      qAddresses =
        Set.toList
          . Set.fromList
          . concatMap (\e -> Utxo.ueUtxos e ^.. folded . Utxo.address)
          $ events
   in fmap (Utxo.QueryUtxoByAddressWrapper . flip Utxo.QueryUtxoByAddress slotInterval) qAddresses
