{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |

module Spec.Marconi.ChainIndex.Experimental.Indexers.Utxo.UtxoIndex where

import Control.Lens (folded, (^.), (^..))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Set qualified as Set
import Database.SQLite.Simple qualified as SQL

import Cardano.Api qualified as C
import Gen.Marconi.ChainIndex.Experimental.Indexers.Utxo (genShelleyEraUtxoEventsAtChainPoint, genTx',
                                                          genTxBodyContentFromTxIns,
                                                          genTxBodyContentFromTxinsWihtPhase2Validation)
import Gen.Marconi.ChainIndex.Types (genBlockNo, genChainPoint', genChainPoints, genSlotNo)
import Marconi.ChainIndex.Experimental.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (SecurityParam (SecurityParam))
import Marconi.Core.Experiment qualified as Core

import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)


tests :: TestTree
tests = testGroup "Spec.Marconi.ChainIndex.Experimental.Indexers.Utxo"
  [ testPropertyNamed
    "Save and retrieve events by address and slot number from storage test."
    "propUtxoQueryByAddressAndSlot"
    propUtxoQueryByAddressAndSlot
  ,
    testPropertyNamed
        "Collateral TxIn should be indexed only, When Phase-2 validation fails."
        "propTxInWhenPhase2ValidationFails"
        propTxInWhenPhase2ValidationFails

   , testPropertyNamed
        "Roundtrip save and retrieve events by address from storage test."
        "propSaveAndRetrieveUtxoEvents"
        propSaveAndRetrieveUtxoEvents

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

-- |  The purpose of this test is to query the in memory part Utxo runIndexers
-- A property of tht Utxo Indexer QyeryResult is that contains the original ChainPoint and QueryAddress
-- A property of the genShelleyEraUtxoEventsAtChainPoint is that at least it contains one Utxo after
--  the `Utxo Spent` have been realized
propUtxoQueryByAddressAndSlot :: Property
propUtxoQueryByAddressAndSlot = property $ do
  highSlotNo <- forAll $ Gen.integral $ Range.constantFrom 7 5 20
  chainPoints :: [C.ChainPoint]  <- forAll $ genChainPoints 2 highSlotNo
  forM_ chainPoints (\cp -> do

    memoryIndexer :: Core.ListIndexer Utxo.UtxoEvent
      <- forAll $ genShelleyEraUtxoEventsAtChainPoint cp
    let
      addressesToQuery :: [C.AddressAny]
      addressesToQuery = memoryIndexer ^. Core.events ^.. folded . Core.event ^.. folded . Utxo.ueUtxos . folded . Utxo.utxoAddress

      queryUtxos :: [Utxo.QueryUtxoByAddress]
      queryUtxos = mkUtxoQuery cp addressesToQuery

    queryResponse :: [[Core.TimedEvent Utxo.Utxo]] <- liftIO $
      traverse (\q -> Core.resumeResult cp  q memoryIndexer (pure [])) queryUtxos
    let
      computedAddresses :: [C.AddressAny]
      computedAddresses = queryResponse ^.. folded  . folded  . Core.event . Utxo.utxoAddress

      computedCp :: Set.Set C.ChainPoint
      computedCp =  Set.fromList $ queryResponse ^.. folded . folded . Core.point

    Hedgehog.footnote $ "cp=" <> show cp
    computedCp === Set.singleton cp
    Hedgehog.assert $ all (== True)[addr `elem` addressesToQuery | addr <- computedAddresses]


                    )

-- | Insert Utxo events in storage
--   The property we're checking here is:
--    * store events in both in-memory and in SQLite
--    * there are events in in-memory post flushing to database
--    * the indexer latest syncpoint is not genesis
propSaveAndRetrieveUtxoEvents :: Property
propSaveAndRetrieveUtxoEvents = property $ do
  cp <- forAll $ genChainPoint' genBlockNo genSlotNo -- generate some non genesis chainpoints
  lixEvents :: [Core.ListIndexer Utxo.UtxoEvent] <-
    forAll $ Gen.list (Range.constantFrom  4 7 10) $ genShelleyEraUtxoEventsAtChainPoint cp
  let
    events :: [Core.TimedEvent Utxo.UtxoEvent]
    events = concat $ lixEvents ^.. folded . Core.events

    numEvents = length events

    mySecurityParam = SecurityParam  --  we set the flush size low to force SQL writes
        $ fromIntegral
        $ numEvents `div` 2

  conn <- liftIO $ Utxo.initSQLite  ":memory:"
  let
    indexer :: Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer Utxo.UtxoEvent
    indexer = Utxo.mkMixedIndexer conn mySecurityParam
  mixindexerResult :: (Either Core.IndexError (Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer Utxo.UtxoEvent) )
    <- Core.indexAll' events indexer
  mixindexer :: Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer Utxo.UtxoEvent
    <- Hedgehog.evalEither mixindexerResult
  [utxosInDb] <- liftIO  (SQL.query_ conn "SELECT count(1) from unspent_transactions" :: IO [Integer])
  let
    inMemoryEvents = mixindexer ^. Core.inMemory . Core.events
    inDbSyncPoint = mixindexer  ^. Core.inDatabase  . Core.dbLastSync
    numberOfEventsInMemroy = length inMemoryEvents

  Hedgehog.footnote
    $ "inmemoryEvents length: " <> show (length inMemoryEvents)
    <> ", numEvents: " <> show numEvents
    <> ", generated chainpoint used for the generated event:" <> show cp
    <> ".  utxosInDB: " <> show utxosInDb
  cp === inDbSyncPoint
  Hedgehog.assert $ numberOfEventsInMemroy < numEvents
  -- verify we saved some events to database. Note there is a one-to-many relationship between utxoEvent and utxo
  -- Thus our expectation is to have more utxo events in DB than there are events
  Hedgehog.assert $ utxosInDb >= toInteger (numEvents - numberOfEventsInMemroy)


mkUtxoQuery :: C.ChainPoint -> [C.AddressAny ] ->  [Utxo.QueryUtxoByAddress]
mkUtxoQuery cp =
  fmap (\a -> case cp of
           C.ChainPointAtGenesis -> Utxo.QueryUtxoByAddress (a, Nothing)
           (C.ChainPoint sno _)  -> Utxo.QueryUtxoByAddress (a, Just sno))

getSlot :: C.ChainPoint -> Maybe C.SlotNo
getSlot C.ChainPointAtGenesis = Nothing
getSlot (C.ChainPoint s _)    = Just s

genTxWithNoCollateral :: Gen (C.Tx C.BabbageEra)
genTxWithNoCollateral = genTx' genTxBodyContentFromTxIns

genTxWithCollateral :: Gen (C.Tx C.BabbageEra)
genTxWithCollateral = genTx' genTxBodyContentFromTxinsWihtPhase2Validation
