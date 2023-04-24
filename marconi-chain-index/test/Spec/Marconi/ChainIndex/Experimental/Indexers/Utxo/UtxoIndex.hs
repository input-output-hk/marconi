{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Marconi.ChainIndex.Experimental.Indexers.Utxo.UtxoIndex where

import Control.Lens (folded, (^.), (^..))
import Control.Monad (foldM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Database.SQLite.Simple qualified as SQL

import Cardano.Api qualified as C
import Gen.Marconi.ChainIndex.Experimental.Indexers.Utxo (genShelleyEraUtxoEventsAtChainPoint, genTx',
                                                          genTxBodyContentFromTxIns,
                                                          genTxBodyContentFromTxinsWihtPhase2Validation)
import Gen.Marconi.ChainIndex.Types (genBlockNo, genChainPoint', genChainPoints, genSlotNo)
import Marconi.ChainIndex.Experimental.Indexers.Utxo ()
import Marconi.ChainIndex.Experimental.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Orphans ()
import Marconi.Core.Experiment qualified as Core

import Hedgehog (Gen, Property, forAll, property, (/==), (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

-- for the pupose these tests, we're igonring ChainPoint
instance Eq e => Eq (Core.TimedEvent e) where
  u == u' = (u ^. Core.event) == (u' ^. Core.event)
instance Ord e => Ord (Core.TimedEvent e) where
  u `compare` u' = (u ^. Core.event) `compare` (u' ^. Core.event)

tests :: TestTree
tests = testGroup "Spec.Marconi.ChainIndex.Experimental.Indexers.Utxo"
  [
    testPropertyNamed
        "Collateral TxIn should be indexed only, When Phase-2 validation fails."
        "propTxInWhenPhase2ValidationFails"
        propTxInWhenPhase2ValidationFails

   , testPropertyNamed
        "Save and query UtxoEvents, with ListIndexer only"
        "testListIndexer"
        testListIndexer

   , testPropertyNamed
        "Querying UtxoEvents with both ListIndexer and mixedIndexers should provide the same results"
        "testIndexer"
        testIndexers

   , testPropertyNamed
        "Save and query UtxoEvents,then verify we get the same results from both in-memory and mixed-indexer."
        "propSaveAndQueryUtxoEvents"
        propSaveAndQueryUtxoEvents

   , testPropertyNamed
        "Querying all slots is the same as querying to the last slot"
        "propUtxoQueryAtLatestPointShouldBeSameAsQueryingAll"
        propUtxoQueryAtLatestPointShouldBeSameAsQueryingAll

  ]

-- | The property verifies that we
--    * process/store all TxIns for valid transactions
--    * use the collateral TxIns only to balance transactions when phase-2 validation failas
--    * use the collateral TxOutsTxIns
propTxInWhenPhase2ValidationFails :: Property
propTxInWhenPhase2ValidationFails = property $ do
  tx@(C.Tx (C.TxBody C.TxBodyContent {..})_) <- forAll genTxWithCollateral
  cp <- forAll $ genChainPoint' genBlockNo genSlotNo
  let event :: Core.TimedEvent Utxo.UtxoEvent = Utxo.getUtxoEvents Nothing [tx] cp
      computedTxins :: [C.TxIn]
      computedTxins = Set.toList $ Set.map Utxo.unSpent (event ^. Core.event . Utxo.ueInputs)
      expectedTxins :: [C.TxIn] = fmap fst txIns


  case txScriptValidity of
    -- this is the same as script is valid, see https://github.com/input-output-hk/cardano-node/pull/4569
    C.TxScriptValidityNone    ->
      Hedgehog.assert $ and [u `elem` expectedTxins| u <- computedTxins]
    (C.TxScriptValidity _ C.ScriptValid ) ->
      Hedgehog.assert $ and [u `elem` expectedTxins| u <- computedTxins]
    (C.TxScriptValidity _ C.ScriptInvalid ) -> do
      case txInsCollateral of
        C.TxInsCollateralNone -> Hedgehog.assert $ null computedTxins
        C.TxInsCollateral _ txinsC_ -> do
          Hedgehog.footnoteShow txReturnCollateral
          let (Utxo.TxOutBalance _ ins) = Utxo.balanceUtxoFromTx Nothing tx
          -- This property shows collateral TxIns will be processed and balanced
          -- Note: not all collateral txins may be utilized in when phase-2 validation fails
          ins === Set.fromList txinsC_
          -- Note: Post transaction balancing, C.TxIns of Spent,
          -- are a subset of of the Collateral TxIns for the same reason as previous note
          -- empty list of computedTxis is a valid subset of collateral txins
          Hedgehog.assert $ all (== True) [u `elem` ins | u <- computedTxins]
      -- -- we should only return txOut collateral

-- | make a unique query for every  Utxo
mkQuery :: [Core.TimedEvent Utxo.Utxo] -> [Utxo.QueryUtxoByAddress]
mkQuery timedUtxos =
  let
    addressesToQuery :: [C.AddressAny]
    addressesToQuery
      = Set.toList . Set.fromList
      $ timedUtxos  ^.. folded . Core.event . Utxo.utxoAddress
  in
    mkUtxoQuery [(a, Nothing)| a <-  addressesToQuery]


-- | The property we are testing here is we can:
--   * can store UTXOs
--   * retrieve UTXOs from MixedIndexer
--   * verify the total number of UTXos in MixedIndexer
propSaveAndQueryUtxoEvents :: Property
propSaveAndQueryUtxoEvents = property $ do
  -- we'll do many inserts, but keep the same connection to force DB flush
  cp <- forAll $ genChainPoint' genBlockNo genSlotNo -- generate some non genesis chainpoints
  timedUtxoEvents :: [Core.TimedEvent Utxo.UtxoEvent]
    <-  forAll $ genShelleyEraUtxoEventsAtChainPoint cp
  conn <- liftIO $ Utxo.initSQLite  ":memory:"
  let
    (keep,flush) = (0,0) -- small memory to force SQL flush
    indexer :: Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer Utxo.UtxoEvent
    indexer = Utxo.mkMixedIndexer' conn keep flush
  mixedIndexer <- Core.indexAll' timedUtxoEvents indexer
    >>= Hedgehog.evalEither

  [utxosSQLCount] <- liftIO
    (SQL.query_ conn "SELECT count(1) from unspent_transactions" :: IO [Integer])
  [spentSQLCount] <- liftIO
    (SQL.query_ conn "SELECT count(1) from spent" :: IO [Integer])

  let
    expectedUtxos :: [Core.TimedEvent Utxo.Utxo]
    expectedUtxos  = concatMap Utxo.timedUtxosFromTimedUtxoEvent timedUtxoEvents

    queryUtxos :: [Utxo.QueryUtxoByAddress]
    queryUtxos = mkQuery expectedUtxos
    inMemoryEvents :: [Core.TimedEvent Utxo.UtxoEvent] = mixedIndexer ^. Core.inMemory . Core.events
      -- in-memory events are utxoEvents. We need to convert them to Utxos for the purpose of this test
    inMemoryUtxos = length $ concatMap Utxo.timedUtxosFromTimedUtxoEvent inMemoryEvents

    inDbSyncPoint :: C.ChainPoint
    inDbSyncPoint = mixedIndexer ^. Core.inDatabase . Core.dbLastSync

  retrievedUtxoMixed :: [Core.TimedEvent Utxo.Utxo] <-
    liftIO $ concat . filter (not . null) <$> traverse (\q -> Core.query cp q mixedIndexer) queryUtxos
        -- test the mixedIndexer
  Hedgehog.assert $ -- make sure we've saved both Utxo and Spent
    (utxosSQLCount  == 0 && spentSQLCount == 0)
    ||
    (utxosSQLCount > 0 && spentSQLCount > 0)
  -- we have flushed to database and Syncpoint is updated correctly
  Hedgehog.assert $
    (utxosSQLCount  > 0 && inDbSyncPoint /= C.ChainPointAtGenesis )
    ||
    (utxosSQLCount == 0 && inDbSyncPoint  == C.ChainPointAtGenesis) -- we have not flushed to database
  -- verify we saved all the events in storage
  utxosSQLCount + toInteger inMemoryUtxos === toInteger (length expectedUtxos)

  (Set.fromList $ retrievedUtxoMixed ^.. folded . Core.point) === Set.singleton cp
  -- We can retrieve balanced ueUtxos
  Hedgehog.assert $   and [ u `elem` expectedUtxos | u <- retrievedUtxoMixed]

-- | Here we index with both listIndexer and mixedIndexer,
--    and test to make certain we get the same result
testIndexers :: Property
testIndexers = property $ do
  cp <- forAll $ genChainPoint' genBlockNo genSlotNo -- generate some non genesis chainpoints
  timedUtxoEvents :: [Core.TimedEvent Utxo.UtxoEvent] <-
    forAll $ genShelleyEraUtxoEventsAtChainPoint cp
  conn <- liftIO $ Utxo.initSQLite  ":memory:"
  let
    (keep,flush) = (2,3) -- small memory to force SQL flush
    indexer :: Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer Utxo.UtxoEvent
    indexer = Utxo.mkMixedIndexer' conn keep flush
  mixedIndexer <- Core.indexAll' timedUtxoEvents indexer
    >>= Hedgehog.evalEither

  listIndexer :: Core.ListIndexer Utxo.UtxoEvent <-
    foldM (flip Core.index) Core.listIndexer timedUtxoEvents -- add events to in-memory listIndexer
  [utxosSQLCount] <- liftIO
    (SQL.query_ conn "SELECT count(1) from unspent_transactions" :: IO [Integer])
  let
    expectedUtxos :: [Core.TimedEvent Utxo.Utxo]
    expectedUtxos  = concatMap Utxo.timedUtxosFromTimedUtxoEvent timedUtxoEvents

    queryUtxos :: [Utxo.QueryUtxoByAddress]
    queryUtxos = mkQuery expectedUtxos
        -- test the mixedIndexer
  forM_ queryUtxos $ \q ->
    (do
        fromMixedIndexer <- Core.query cp q mixedIndexer
        fromListIndexer <- Core.resumeResult cp q listIndexer (pure [])
        Hedgehog.footnote
          $ "Counts"
          <> "\nListIndexer count: " <> show ( length $ listIndexer ^. Core.events)
          <>  "\nmixedIndexer inMemory count: " <> show (length $ mixedIndexer ^. Core.inMemory . Core.events)
          <>  "\nSQLite count unspent_transactions: " <> show utxosSQLCount

    -- TODO once the test passes, take this out,
        (Set.size . Set.fromList $ fromListIndexer) === (Set.size . Set.fromList $ fromMixedIndexer)
        Set.fromList fromListIndexer === Set.fromList fromMixedIndexer
    )

-- | The property we test here is
--    * Correct ChainPoint was computed, stored and retreived by the listIndexer
--    * Query for all addresses should return all utxos stored into the system
testListIndexer :: Property
testListIndexer = property $ do
  cp <- forAll $ genChainPoint' genBlockNo genSlotNo -- generate some non genesis chainpoints
  timedUtxoEvents :: [Core.TimedEvent Utxo.UtxoEvent]
    <-  forAll $ genShelleyEraUtxoEventsAtChainPoint cp
  listIndexer :: Core.ListIndexer Utxo.UtxoEvent <-
    foldM (flip Core.index) Core.listIndexer timedUtxoEvents -- add events to in-memory listIndexer

  let
    expectedUtxos :: [Core.TimedEvent Utxo.Utxo]
    expectedUtxos  = concatMap Utxo.timedUtxosFromTimedUtxoEvent timedUtxoEvents

    queryUtxos :: [Utxo.QueryUtxoByAddress]
    queryUtxos = mkQuery expectedUtxos

  retrievedUtxo <-
      concat <$> traverse (\q ->
                             Core.resumeResult cp q listIndexer (pure [])
                          ) queryUtxos
  let
    retrievedCp :: Set C.ChainPoint
    retrievedCp =  Set.fromList $ retrievedUtxo  ^.. folded . Core.point

  retrievedCp === Set.singleton cp
  Hedgehog.assert $ and [ u `elem` expectedUtxos | u <- retrievedUtxo]


propUtxoQueryAtLatestPointShouldBeSameAsQueryingAll :: Property
propUtxoQueryAtLatestPointShouldBeSameAsQueryingAll = property $ do
  highSlotNo <- forAll $ Gen.integral $ Range.constantFrom 7 5 20
  chainPoints :: [C.ChainPoint]  <- forAll $ genChainPoints 2 highSlotNo
  timedUtxoEvents :: [Core.TimedEvent Utxo.UtxoEvent] <-
    forAll $ traverse genShelleyEraUtxoEventsAtChainPoint chainPoints <&> concat
  conn <- liftIO $ Utxo.initSQLite  ":memory:"
  let
    (keep,flush) = (1,2) -- small memory to force SQL flush
    indexer :: Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer Utxo.UtxoEvent
    indexer = Utxo.mkMixedIndexer' conn keep flush
  mixedIndexer <- Core.indexAll' timedUtxoEvents indexer
    >>= Hedgehog.evalEither
  listIndexer :: Core.ListIndexer Utxo.UtxoEvent <-
    foldM (flip Core.index) Core.listIndexer timedUtxoEvents -- add events to in-memory listIndexer

  lastMemCp :: C.ChainPoint  <- Core.lastSyncPoint (mixedIndexer ^. Core.inMemory)
  lastListIndexerCp :: C.ChainPoint  <- Core.lastSyncPoint listIndexer -- mixedIndexer
  Hedgehog.footnote $ show lastMemCp

  -- syncpoint of listIndexer should reflect the latest chainpoint
  lastListIndexerCp === last chainPoints --
  -- with keep/flush at this level, we are garanteed to have some DB entires.
  -- the last syncpoint in DB should be less than the latest, but not genesis
  lastMemCp /== C.ChainPointAtGenesis -- we have flushed to SQLite
  Hedgehog.assert $ lastMemCp < last chainPoints

 -- Make a query
mkUtxoQuery
  :: [(C.AddressAny, Maybe C.ChainPoint)] -- ^ Address Chainpoint pair
  ->  [Utxo.QueryUtxoByAddress] -- resulted query
mkUtxoQuery  =
  fmap (\(a, cp) -> case cp of
           Just (C.ChainPoint sno _) -> Utxo.QueryUtxoByAddress (a, Just sno)
           _                         -> Utxo.QueryUtxoByAddress (a, Nothing))

getSlot :: C.ChainPoint -> Maybe C.SlotNo
getSlot C.ChainPointAtGenesis = Nothing
getSlot (C.ChainPoint s _)    = Just s

genTxWithNoCollateral :: Gen (C.Tx C.BabbageEra)
genTxWithNoCollateral = genTx' genTxBodyContentFromTxIns

genTxWithCollateral :: Gen (C.Tx C.BabbageEra)
genTxWithCollateral = genTx' genTxBodyContentFromTxinsWihtPhase2Validation
