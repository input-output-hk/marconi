{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Marconi.ChainIndex.Experimental.Indexers.Utxo.UtxoIndex where

import Cardano.Api qualified as C
import Control.Lens (folded, traversed, (%~), (&), (^.), (^..))
import Control.Monad (foldM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Database.SQLite.Simple qualified as SQL
import Gen.Marconi.ChainIndex.Experimental.Indexers.Utxo (
  genShelleyEraUtxoEventsAtChainPoint,
  genTx',
  genTxBodyContentFromTxIns,
  genTxBodyContentFromTxInsWithPhase2Validation,
  genUtxoEventsWithTxs,
 )
import Gen.Marconi.ChainIndex.Mockchain (MockBlock, mockBlockTxs)
import Gen.Marconi.ChainIndex.Types (genBlockNo, genChainPoint', genChainPoints, genSlotNo)
import Helpers (addressAnyToShelley)
import Marconi.ChainIndex.Experimental.Indexers.Utxo ()
import Marconi.ChainIndex.Experimental.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (
  TargetAddresses,
  UtxoIndexerConfig (UtxoIndexerConfig),
  ucEnableUtxoTxOutRef,
  ucTargetAddresses,
 )
import Marconi.Core.Experiment qualified as Core

import Hedgehog (Gen, Property, cover, forAll, property, (/==), (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.ChainIndex.Experimental.Indexers.Utxo"
    [ testPropertyNamed
        "All queried UTXOs by address should be unspent."
        "allqueryUtxosShouldBeUnspent"
        allqueryUtxosShouldBeUnspent
    , testPropertyNamed
        "Collateral TxIn should be indexed only, When Phase-2 validation fails."
        "propTxInWhenPhase2ValidationFails"
        propTxInWhenPhase2ValidationFails
    , testPropertyNamed
        "Save and query UtxoEvents, with ListIndexer only"
        "propListIndexerUpdatesLastSyncPoint"
        propListIndexerUpdatesLastSyncPoint
    , testPropertyNamed
        "Querying UtxoEvents with both ListIndexer and mixedIndexers should provide the same results"
        "propMixedIndexerAndListIndexerProvideTheSameQueryResult"
        propMixedIndexerAndListIndexerProvideTheSameQueryResult
    , testPropertyNamed
        "Save and query UtxoEvents,then verify we get the same results from both in-memory and mixed-indexer."
        "propSaveAndQueryUtxoEvents"
        propSaveAndQueryUtxoEvents
    , testPropertyNamed
        "Querying all slots is the same as querying to the last slot"
        "propUtxoQueryAtLatestPointShouldBeSameAsQueryingAll"
        propUtxoQueryAtLatestPointShouldBeSameAsQueryingAll
    , testPropertyNamed
        "getUtxoEvents with target addresses corresponding to all addresses in generated txs should return the same 'UtxoEvent' as if no target addresses were provided"
        "propUsingAllAddressesOfTxsAsTargetAddressesShouldReturnUtxosAsIfNoFilterWasApplied "
        propUsingAllAddressesOfTxsAsTargetAddressesShouldReturnUtxosAsIfNoFilterWasApplied
    , testPropertyNamed
        "mixedIndexer and listIndexer provide the same query result for the same inserted events"
        "propListIndexerAndMixedIndexerInMemroyIndexerProvideTheSameQueryResult"
        propListIndexerAndMixedIndexerInMemroyIndexerProvideTheSameQueryResult
    , testPropertyNamed
        "Verify MixedIndexer's lastSyncPoint is the points stored in SQLite"
        "propLastSyncPointIsUpdatedOnInserts"
        propLastSyncPointIsUpdatedOnInserts
    , testPropertyNamed
        "On a rollback, the latest latest sync point is the one we rollback to if the indexer was ahead of it"
        "propLastChainPointOnRewindIndexer"
        propLastChainPointOnRewindIndexer
    ]

{- | The purpose of test is to make sure All Queried Utxo's are unSpent.
  The Utxo store consists of:
  * in-memory store:  UtxoEvents before they're flushed to SQlite
  * SQL-database store:  UtxoRows that are stored in SQLite
  In this test, We want to make sure:
    (1) all utxo query results from SQL-database store are unspent
    (2) all utxos query results from in-memory store are unspent
    (3) the edge case where although we satisfy (1) and (2),
        one or many of the query results from SQLite store may have `Spent` in the in-memory store.
    (4) furthermore, we want to prove that there is always at least one utxoRow returned from sotre.
  Point (4) is a consequence of the `genShelleyEraUtxoEvents` specifications:  __there is only Spent for previous generated UtxoEvent__
 Assumption:  SQLite vacuum is disabled, so that we can accouhnt for generated Spents
 Note:        We expect this test to fail in this branch.
-}
allqueryUtxosShouldBeUnspent :: Property
allqueryUtxosShouldBeUnspent = property $ do
  cp <- forAll $ genChainPoint' genBlockNo genSlotNo -- generate some non genesis chainpoints
  timedUtxoEvent :: Core.Timed C.ChainPoint Utxo.UtxoEvent <-
    forAll $ genShelleyEraUtxoEventsAtChainPoint cp
  conn <- liftIO $ Utxo.initSQLite ":memory:"
  let (keep, flush) = (1, 1) -- small memory to force SQL flush
      indexer :: Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer Utxo.UtxoEvent
      indexer = Utxo.mkMixedIndexer' conn keep flush
  mixedIndexer <-
    Core.indexEither (asInput timedUtxoEvent) indexer
      >>= Hedgehog.evalEither
  let unprocessedUtxos :: [Core.Timed C.ChainPoint Utxo.Utxo]
      unprocessedUtxos = Utxo.timedUtxosFromTimedUtxoEvent timedUtxoEvent

      queryUtxos :: [Utxo.QueryUtxoByAddress]
      queryUtxos = mkQuery unprocessedUtxos cp

  retrievedUtxos :: [Core.Timed C.ChainPoint Utxo.Utxo] <-
    liftIO $ concat . filter (not . null) <$> traverse (\q -> Core.query cp q mixedIndexer) queryUtxos

  let inputsFromTimedUtxoEvent :: [C.TxIn] -- get all the TxIn from quried UtxoRows
      inputsFromTimedUtxoEvent =
        (Set.toList $ timedUtxoEvent ^. Core.event . Utxo.ueInputs) & traversed %~ Utxo.unSpent

      txInsFromRetrieved :: [C.TxIn]
      txInsFromRetrieved = retrievedUtxos ^.. folded . Core.event . Utxo.utxoTxIn

  Hedgehog.assert (not . null $ retrievedUtxos)
  -- There should be no `Spent` in the retrieved UtxoRows
  Hedgehog.footnote
    "Regression test must return at least one Utxo. Utxo's may not have any Spent in the Orig. event"
  Hedgehog.assert $
    and [u `notElem` inputsFromTimedUtxoEvent | u <- txInsFromRetrieved]

{- | The property verifies that we
    * process/store all TxIns for valid transactions
    * use the collateral TxIns only to balance transactions when phase-2 validation failas
    * use the collateral TxOutsTxIns
-}
propTxInWhenPhase2ValidationFails :: Property
propTxInWhenPhase2ValidationFails = property $ do
  tx@(C.Tx (C.TxBody C.TxBodyContent{..}) _) <- forAll genTxWithCollateral
  cp <- forAll $ genChainPoint' genBlockNo genSlotNo
  let utxoIndexerConfig = UtxoIndexerConfig{ucTargetAddresses = Nothing, ucEnableUtxoTxOutRef = True} -- \^ index all addresses, and store scriptRef
      event :: Core.Timed C.ChainPoint Utxo.UtxoEvent
      event = Core.Timed cp $ Utxo.getUtxoEvents utxoIndexerConfig [tx]
      computedTxins :: [C.TxIn]
      computedTxins = Set.toList $ Set.map Utxo.unSpent (event ^. Core.event . Utxo.ueInputs)
      expectedTxins :: [C.TxIn]
      expectedTxins = fmap fst txIns

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
          let (Utxo.TxOutBalance _ ins) = Utxo.balanceUtxoFromTx utxoIndexerConfig tx
          -- This property shows collateral TxIns will be processed and balanced
          -- Note: not all collateral txins may be utilized in when phase-2 validation fails
          ins === Set.fromList txinsC_
          -- Note: Post transaction balancing, C.TxIns of Spent,
          -- are a subset of of the Collateral TxIns for the same reason as previous note
          -- empty list of computedTxis is a valid subset of collateral txins
          Hedgehog.assert $ all (== True) [u `elem` ins | u <- computedTxins]

-- -- we should only return txOut collateral

{- | The property we are testing here is we can:
   * can store UTXOs
   * retrieve UTXOs from MixedIndexer
   * verify the total number of UTXos in MixedIndexer
-}
propSaveAndQueryUtxoEvents :: Property
propSaveAndQueryUtxoEvents = property $ do
  -- we'll do many inserts, but keep the same connection to force DB flush
  cp <- forAll $ genChainPoint' genBlockNo genSlotNo -- generate some non genesis chainpoints
  timedUtxoEvent :: Core.Timed C.ChainPoint Utxo.UtxoEvent <-
    forAll $ genShelleyEraUtxoEventsAtChainPoint cp
  conn <- liftIO $ Utxo.initSQLite ":memory:"
  let (keep, flush) = (1, 3) -- small memory to force SQL flush
      indexer :: Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer Utxo.UtxoEvent
      indexer = Utxo.mkMixedIndexer' conn keep flush
  mixedIndexer <-
    Core.indexEither (asInput timedUtxoEvent) indexer
      >>= Hedgehog.evalEither

  [utxosSQLCount] <-
    liftIO
      (SQL.query_ conn "SELECT count(1) from unspent_transactions" :: IO [Integer])
  [spentSQLCount] <-
    liftIO
      (SQL.query_ conn "SELECT count(1) from spent" :: IO [Integer])

  let unprocessedUtxos :: [Core.Timed C.ChainPoint Utxo.Utxo]
      unprocessedUtxos = Utxo.timedUtxosFromTimedUtxoEvent timedUtxoEvent

      queryUtxos :: [Utxo.QueryUtxoByAddress]
      queryUtxos = mkQuery unprocessedUtxos cp
      inMemoryEvents :: [Core.Timed C.ChainPoint Utxo.UtxoEvent] =
        mixedIndexer ^. Core.inMemory . Core.events
      -- in-memory events are utxoEvents. We need to convert them to Utxos for the purpose of this test
      inMemoryUtxos = length $ concatMap Utxo.timedUtxosFromTimedUtxoEvent inMemoryEvents

      inDbSyncPoint :: C.ChainPoint
      inDbSyncPoint = mixedIndexer ^. Core.inDatabase . Core.dbLastSync

  retrievedUtxoMixed :: [Core.Timed C.ChainPoint Utxo.Utxo] <-
    liftIO $ concat . filter (not . null) <$> traverse (\q -> Core.query cp q mixedIndexer) queryUtxos
  -- test the mixedIndexer
  Hedgehog.assert $ -- make sure we've saved both Utxo and Spent
    (utxosSQLCount == 0 && spentSQLCount == 0)
      || (utxosSQLCount > 0 && spentSQLCount > 0)
  -- we have flushed to database and Syncpoint is updated correctly
  Hedgehog.assert $
    (utxosSQLCount > 0 && inDbSyncPoint /= C.ChainPointAtGenesis)
      || (utxosSQLCount == 0 && inDbSyncPoint == C.ChainPointAtGenesis) -- we have not flushed to database
      -- verify we saved all the events in storage
  utxosSQLCount + toInteger inMemoryUtxos === toInteger (length unprocessedUtxos)

  (Set.fromList $ retrievedUtxoMixed ^.. folded . Core.point) === Set.singleton cp
  -- We can retrieve balanced ueUtxos
  Hedgehog.assert $ and [u `elem` unprocessedUtxos | u <- retrievedUtxoMixed]

{- | The property we test here is that:
   * mixedIndexer and listIndexer will provide the same queryResults for the same stored events
   * mixedIndexer's components, in-memory and SQLite, combined will provide the same queryResult as the ListIndexer
-}
propMixedIndexerAndListIndexerProvideTheSameQueryResult :: Property
propMixedIndexerAndListIndexerProvideTheSameQueryResult = property $ do
  cp <- forAll $ genChainPoint' genBlockNo genSlotNo -- generate some non genesis chainpoints
  timedUtxoEvent :: Core.Timed C.ChainPoint Utxo.UtxoEvent <-
    forAll $ genShelleyEraUtxoEventsAtChainPoint cp
  conn <- liftIO $ Utxo.initSQLite ":memory:"
  let (keep, flush) = (0, 0) -- small memory to force SQL flush
      indexer :: Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer Utxo.UtxoEvent
      indexer = Utxo.mkMixedIndexer' conn keep flush
  mixedIndexer <-
    Core.indexEither (asInput timedUtxoEvent) indexer
      >>= Hedgehog.evalEither

  listIndexer :: Core.ListIndexer Utxo.UtxoEvent <-
    Core.index (asInput timedUtxoEvent) Core.mkListIndexer -- add events to in-memory listIndexer
  [utxosSQLCount] <-
    liftIO
      (SQL.query_ conn "SELECT count(1) from unspent_transactions" :: IO [Integer])
  let unprocessedUtxos :: [Core.Timed C.ChainPoint Utxo.Utxo]
      unprocessedUtxos = Utxo.timedUtxosFromTimedUtxoEvent timedUtxoEvent

      queryUtxos :: [Utxo.QueryUtxoByAddress]
      queryUtxos = mkQuery unprocessedUtxos cp
  -- test the mixedIndexer
  forM_ queryUtxos $ \q ->
    do
      fromSqliteIndexer <- Core.query cp q (mixedIndexer ^. Core.inDatabase)
      fromMixedIndexer <- Core.query cp q mixedIndexer
      fromMixedIndexerMem <- Core.query cp q (mixedIndexer ^. Core.inMemory)
      fromListIndexer <- Core.appendResult cp q listIndexer (pure [])
      Hedgehog.footnote $
        "\n== Counts =="
          <> "\nListIndexer count: "
          <> show (length $ listIndexer ^. Core.events)
          <> "\nmixedIndexer inMemory count: "
          <> show (length $ mixedIndexer ^. Core.inMemory . Core.events)
          <> "\nSQLite count unspent_transactions: "
          <> show utxosSQLCount
          <> "\nMiexedIndexer: "
          <> show (Set.fromList fromMixedIndexer)
          <> "\nlistIndexer: "
          <> show (Set.fromList fromListIndexer)
          <> "\nmixedIndexer SQLite count: "
          <> show (Set.fromList fromSqliteIndexer)

      -- Check we arrive at the same results from both indexers
      Set.fromList fromListIndexer === Set.fromList fromMixedIndexer
      -- Check that mixedIndexer's sql and in-memory indexers combined are the same as the listIndexer
      Set.fromList fromListIndexer
        === Set.union
          (Set.fromList fromSqliteIndexer)
          (Set.fromList fromMixedIndexerMem)

{- | The property we test here is that:
   * mixedIndexer's in-memory indexer behaves the same as the listIndexer
-}
propListIndexerAndMixedIndexerInMemroyIndexerProvideTheSameQueryResult :: Property
propListIndexerAndMixedIndexerInMemroyIndexerProvideTheSameQueryResult = property $ do
  cp <- forAll $ genChainPoint' genBlockNo genSlotNo -- generate some non genesis chainpoints
  timedUtxoEvent :: Core.Timed C.ChainPoint Utxo.UtxoEvent <-
    forAll $ genShelleyEraUtxoEventsAtChainPoint cp
  conn <- liftIO $ Utxo.initSQLite ":memory:"
  let
    -- we force mixedIndexer to use in-memory indexer only
    (keep, flush) = (2160, 2160) -- x-large memory size to prevent SQL flush
    indexer :: Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer Utxo.UtxoEvent
    indexer = Utxo.mkMixedIndexer' conn keep flush

  mixedIndexer <-
    Core.indexEither (asInput timedUtxoEvent) indexer
      >>= Hedgehog.evalEither
  listIndexer :: Core.ListIndexer Utxo.UtxoEvent <-
    Core.index (asInput timedUtxoEvent) Core.mkListIndexer -- add events to in-memory listIndexer

  -- this is to verify we did not flush to database
  [utxosSQLCount] <-
    liftIO
      (SQL.query_ conn "SELECT count(1) from unspent_transactions" :: IO [Integer])
  let unprocessedUtxos :: [Core.Timed C.ChainPoint Utxo.Utxo]
      unprocessedUtxos = Utxo.timedUtxosFromTimedUtxoEvent timedUtxoEvent

      queryUtxos :: [Utxo.QueryUtxoByAddress]
      queryUtxos = mkQuery unprocessedUtxos cp

  forM_ queryUtxos $ \q ->
    do
      fromSqliteIndexer <- Core.query cp q (mixedIndexer ^. Core.inDatabase)
      fromMixedIndexer <- Core.query cp q mixedIndexer
      fromMixedIndexerMem <- Core.query cp q (mixedIndexer ^. Core.inMemory)
      fromListIndexer <- Core.appendResult cp q listIndexer (pure [])

      -- Check we arrive at the same results from both indexers
      Set.fromList fromListIndexer === Set.fromList fromMixedIndexerMem

      -- Check we mixedIndexer as whole is still equal to listIndexer
      Set.fromList fromListIndexer === Set.fromList fromMixedIndexer
      -- Check that mixedIndexer's sql and in-memory indexers combined are the same as the listIndexer
      utxosSQLCount === 0 -- verify we did not flush to SQLite
      Hedgehog.assert $ null fromSqliteIndexer

{- | The property we test here is
    * Correct ChainPoint was computed, stored and retreived by the listIndexer
    * Query for all addresses should return all utxos stored into the system
TODO change the test name to prop.......
-}
propListIndexerUpdatesLastSyncPoint :: Property
propListIndexerUpdatesLastSyncPoint = property $ do
  cp <- forAll $ genChainPoint' genBlockNo genSlotNo -- generate some non genesis chainpoints
  timedUtxoEvent :: Core.Timed C.ChainPoint Utxo.UtxoEvent <-
    forAll $ genShelleyEraUtxoEventsAtChainPoint cp
  listIndexer :: Core.ListIndexer Utxo.UtxoEvent <-
    Core.index (asInput timedUtxoEvent) Core.mkListIndexer -- add events to in-memory listIndexer
  let unProcessedUtxos :: [Core.Timed C.ChainPoint Utxo.Utxo] -- These Utxos, are not processed yet and may have spent in them
      unProcessedUtxos = Utxo.timedUtxosFromTimedUtxoEvent timedUtxoEvent

      queryUtxos :: [Utxo.QueryUtxoByAddress]
      queryUtxos = mkQuery unProcessedUtxos cp

  retrievedUtxo <-
    concat
      <$> traverse
        ( \q ->
            Core.appendResult cp q listIndexer (pure [])
        )
        queryUtxos
  let retrievedCp :: Set C.ChainPoint
      retrievedCp = Set.fromList $ retrievedUtxo ^.. folded . Core.point

  Hedgehog.footnote $
    "retrievedUtxos"
      <> show (Set.size . Set.fromList $ retrievedUtxo)
      <> "\nqueryUtxos"
      <> show (Set.size . Set.fromList $ queryUtxos)
      <> "\nUnProcessedUtxos"
      <> show (Set.size $ Set.fromList unProcessedUtxos)

  retrievedCp === Set.singleton cp
  Hedgehog.assert $ and [u `elem` unProcessedUtxos | u <- retrievedUtxo]
  -- A property of the genertor is that after taking out Spent, there will be at least one UnspentTransaction
  -- Thus every query, should return one balance utxo
  Hedgehog.assert $ (Set.size $ Set.fromList retrievedUtxo) >= (Set.size $ Set.fromList queryUtxos)

propUtxoQueryAtLatestPointShouldBeSameAsQueryingAll :: Property
propUtxoQueryAtLatestPointShouldBeSameAsQueryingAll = property $ do
  highSlotNo <- forAll $ Gen.integral $ Range.constantFrom 7 5 20
  chainPoints :: [C.ChainPoint] <- forAll $ genChainPoints 2 highSlotNo
  timedUtxoEvents :: [Core.Timed C.ChainPoint Utxo.UtxoEvent] <-
    forAll $ traverse genShelleyEraUtxoEventsAtChainPoint chainPoints
  conn <- liftIO $ Utxo.initSQLite ":memory:"
  let (keep, flush) = (1, 1) -- small memory to force SQL flush
      indexer :: Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer Utxo.UtxoEvent
      indexer = Utxo.mkMixedIndexer' conn keep flush
  mixedIndexer <- Core.indexAllEither (asInput <$> timedUtxoEvents) indexer >>= Hedgehog.evalEither
  listIndexer :: Core.ListIndexer Utxo.UtxoEvent <-
    Core.indexAll (asInput <$> timedUtxoEvents) Core.mkListIndexer -- add events to in-memory listIndexer
  lastMemCp :: C.ChainPoint <- Core.lastSyncPoint (mixedIndexer ^. Core.inMemory)
  lastDbCp :: C.ChainPoint <- Core.lastSyncPoint (mixedIndexer ^. Core.inDatabase)
  lastListIndexerCp :: C.ChainPoint <- Core.lastSyncPoint listIndexer -- mixedIndexer
  Hedgehog.footnote $ "Mem chainpoint: " <> show lastMemCp
  Hedgehog.footnote $ "Db chainpoint: " <> show lastDbCp
  Hedgehog.footnote $ "Mem size: " <> show (length $ mixedIndexer ^. Core.inMemory . Core.events)
  -- syncpoint of listIndexer should reflect the latest chainpoint
  lastListIndexerCp === last chainPoints --
  -- with keep/flush at this level, we are garanteed to have some DB entires.
  -- the last syncpoint in DB should be less than the latest, but not genesis
  lastDbCp /== C.ChainPointAtGenesis -- we have flushed to SQLite
  Hedgehog.assert $ lastDbCp < last chainPoints

{- | Calling 'Utxo.getUtxoEventos' with target addresses that are extracted from all tx outputs from
 the initial generated txs should return the same 'UtxoEvent's as if there was no provided target
 addresses.
-}
propUsingAllAddressesOfTxsAsTargetAddressesShouldReturnUtxosAsIfNoFilterWasApplied :: Property
propUsingAllAddressesOfTxsAsTargetAddressesShouldReturnUtxosAsIfNoFilterWasApplied = property $ do
  timedUtxoEventsWithTxs :: [(Core.Timed C.ChainPoint Utxo.UtxoEvent, MockBlock C.BabbageEra)] <-
    forAll genUtxoEventsWithTxs
  forM_ timedUtxoEventsWithTxs $ \(expectedTimedUtxoEvent, block) -> do
    let txs = mockBlockTxs block
        expectedAddresses = mkTargetAddressFromTxs txs
        utxoIndexerConfig = UtxoIndexerConfig expectedAddresses True
    cover 50 "At least one address is used as a target address" $
      isJust expectedAddresses
    cover 1 "No target addresses are provided" $
      isNothing expectedAddresses
    let actualTimedUtxoEvents :: Core.Timed C.ChainPoint Utxo.UtxoEvent
        actualTimedUtxoEvents =
          Core.Timed
            (expectedTimedUtxoEvent ^. Core.point)
            $ Utxo.getUtxoEvents utxoIndexerConfig txs
    let
      -- (expectedTimedUtxoEvent ^. Core.event . Utxo.ueUtxos)
      filteredExpectedUtxoEvent :: Core.Timed C.ChainPoint Utxo.UtxoEvent
      filteredExpectedUtxoEvent =
        expectedTimedUtxoEvent
          & Core.event . Utxo.ueUtxos
            %~ Set.filter (\utxo -> isJust $ addressAnyToShelley $ utxo ^. Utxo.utxoAddress)

    -- If the 'expectedUtxoEvent' only contain Byron addresses, then 'filteredExpectedUtxoEvent'
    -- will have an empty set of utxos. In that scenario, the `getUtxoEvents` should not filter
    -- anything, so we just return 'pure ()'.
    if not
      (null $ expectedTimedUtxoEvent ^. Core.event . Utxo.ueUtxos)
      && null (filteredExpectedUtxoEvent ^. Core.event . Utxo.ueUtxos)
      then pure ()
      else filteredExpectedUtxoEvent === actualTimedUtxoEvents
  where
    mkTargetAddressFromTxs
      :: [C.Tx C.BabbageEra]
      -> Maybe TargetAddresses
    mkTargetAddressFromTxs txs =
      foldMap
        ( \(C.Tx (C.TxBody C.TxBodyContent{C.txOuts}) _) ->
            mkTargetAddressFromTxOuts txOuts
        )
        txs

    mkTargetAddressFromTxOuts
      :: [C.TxOut C.CtxTx C.BabbageEra]
      -> Maybe TargetAddresses
    mkTargetAddressFromTxOuts txOuts =
      nonEmpty $
        mapMaybe
          ( \(C.TxOut addr _ _ _) ->
              addressAnyToShelley $ Utxo.toAddr addr
          )
          txOuts

{- | The property we chack here is that upon inserts, the mixedIndexer maintains the most recent database sync point
   lastSyncPoint is where the database may resume from
-}
propLastSyncPointIsUpdatedOnInserts :: Property
propLastSyncPointIsUpdatedOnInserts = property $ do
  chainPoints :: [C.ChainPoint] <- forAll $ genChainPoints 5 10
  conn <- liftIO $ Utxo.initSQLite ":memory:"
  let (keep, flush) = (1, 2) -- small memory to force SQL flush
      indexer :: Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer Utxo.UtxoEvent
      indexer = Utxo.mkMixedIndexer' conn keep flush
  mixedIndexer <-
    foldM
      ( \indx cp ->
          forAll (genShelleyEraUtxoEventsAtChainPoint cp)
            >>= flip Core.indexEither indx . asInput
            >>= Hedgehog.evalEither
      )
      indexer
      chainPoints

  fromDbSlotNos' <-
    liftIO
      ( SQL.query_ conn "SELECT DISTINCT slotNo from unspent_transactions ORDER by slotNo DESC"
          :: IO [Integer]
      )

  let fromDbSlotNos :: [C.SlotNo] = fmap (C.SlotNo . fromIntegral) fromDbSlotNos'
      inMemSyncPoint = mixedIndexer ^. Core.inMemory . Core.latest
  inMemResumePoints <- Core.lastSyncPoint $ mixedIndexer ^. Core.inMemory

  dbLastSyncSlotNo <- case mixedIndexer ^. Core.inDatabase . Core.dbLastSync of
    (C.ChainPoint s _) -> pure s
    C.ChainPointAtGenesis ->
      Hedgehog.footnote "mixedIndexer's dbLastSyncpoint should not be Genesis"
        >> Hedgehog.failure

  Hedgehog.footnote $
    "mixIndexer dbSyncPoint SlotNo: "
      <> show dbLastSyncSlotNo
      <> "\ninMemory lastPoint: "
      <> show inMemSyncPoint
      <> "\ninMemory syncPoints: "
      <> show inMemResumePoints
      <> "\n slots from db: "
      <> show fromDbSlotNos

  head fromDbSlotNos === dbLastSyncSlotNo

{- | The property we check here is on a Rollback to a previous ChainPoint:
   * SQLite database reflects this change
   * MixedIndexer and SQLite indexer reflect this change
-}
propLastChainPointOnRewindIndexer :: Property
propLastChainPointOnRewindIndexer = property $ do
  chainPoints :: [C.ChainPoint] <- forAll $ genChainPoints 7 10
  conn <- liftIO $ Utxo.initSQLite ":memory:"
  let (keep, flush) = (1, 1) -- small memory to force a few SQL flush
      indexer :: Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer Utxo.UtxoEvent
      indexer = Utxo.mkMixedIndexer' conn keep flush
  mixedIndexer <-
    foldM
      ( \indx cp ->
          forAll (genShelleyEraUtxoEventsAtChainPoint cp)
            >>= flip Core.indexEither indx . asInput
            >>= Hedgehog.evalEither
      )
      indexer
      chainPoints
  fromDbSlotNosBefore <-
    liftIO
      ( SQL.query_ conn "SELECT DISTINCT slotNo from unspent_transactions ORDER by slotNo DESC"
          :: IO [Integer]
      )
  (C.ChainPoint dbLastSyncSlotNo b) <-
    evalChainPoint $ mixedIndexer ^. Core.inDatabase . Core.dbLastSync

  let rewindTo :: C.ChainPoint = C.ChainPoint (dbLastSyncSlotNo - 1) b -- rewind by 1 slot
  rewoundMixedIndexer <- Core.rollback rewindTo mixedIndexer
  fromDbSlotNosAfter <-
    liftIO
      ( SQL.query_ conn "SELECT DISTINCT slotNo from unspent_transactions ORDER by slotNo DESC"
          :: IO [Integer]
      )
  let rewoundDbLastSync = rewoundMixedIndexer ^. Core.inDatabase . Core.dbLastSync
  (C.ChainPoint rewoundDbLastSyncSlotNo _) <- evalChainPoint rewoundDbLastSync

  Hedgehog.footnote $
    "mixIndexer dbLastSyncPoint slotNO Before rewind: "
      <> show dbLastSyncSlotNo
      <> " slotNos from DB Before rewind: "
      <> show fromDbSlotNosBefore
      <> "\nmixeIndexer dbLastSyncPoint slotNo after rewind: "
      <> show rewoundDbLastSyncSlotNo
      <> " slotNos from DB after rewind: "
      <> show fromDbSlotNosAfter

  rewoundDbLastSync === rewindTo -- mixed indexer should reflect the new rwound chainpoint
  fromDbSlotNosAfter === tail fromDbSlotNosBefore -- SQLite should reflect the new rwound chainpoint

-- | make a unique query for every  Utxo
mkQuery :: [Core.Timed C.ChainPoint Utxo.Utxo] -> C.ChainPoint -> [Utxo.QueryUtxoByAddress]
mkQuery timedUtxos cp =
  let addressesToQuery :: [C.AddressAny]
      addressesToQuery =
        Set.toList . Set.fromList $
          timedUtxos ^.. folded . Core.event . Utxo.utxoAddress
   in mkUtxoQuery [(a, Just cp) | a <- addressesToQuery]

-- Make a query
mkUtxoQuery
  :: [(C.AddressAny, Maybe C.ChainPoint)]
  -- ^ Address Chainpoint pair
  -> [Utxo.QueryUtxoByAddress] -- resulted query
mkUtxoQuery =
  fmap
    ( \(a, cp) -> case cp of
        Just (C.ChainPoint sno _) -> Utxo.QueryUtxoByAddress (a, Just sno)
        _ -> Utxo.QueryUtxoByAddress (a, Nothing)
    )

evalChainPoint :: (MonadIO m, Hedgehog.MonadTest m) => C.ChainPoint -> m C.ChainPoint
evalChainPoint cp =
  case cp of
    C.ChainPointAtGenesis ->
      Hedgehog.footnote "ChainPointAtGenesis was not expected!"
        >> Hedgehog.failure
    _ -> pure cp

genTxWithNoCollateral :: Gen (C.Tx C.BabbageEra)
genTxWithNoCollateral = genTx' genTxBodyContentFromTxIns

genTxWithCollateral :: Gen (C.Tx C.BabbageEra)
genTxWithCollateral = genTx' genTxBodyContentFromTxInsWithPhase2Validation

asInput :: Core.Timed a event -> Core.Timed a (Maybe event)
asInput = fmap Just
