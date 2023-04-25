{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Sidechain.Api.Query.Indexers.Utxo (tests) where

import Cardano.Api qualified as C
import Control.Concurrent.STM (atomically)
import Control.Lens.Operators ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (unpack)
import Data.Traversable (for)
import Gen.Marconi.ChainIndex.Indexers.Utxo (genShelleyEraUtxoEvents)
import Hedgehog (Property, assert, forAll, property, (===))
import Hedgehog qualified
import Helpers (addressAnyToShelley)
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.Sidechain.Api.Query.Indexers.Utxo qualified as AddressUtxoIndexer
import Marconi.Sidechain.Api.Routes (AddressUtxoResult, GetCurrentSyncedBlockResult (GetCurrentSyncedBlockResult),
                                     GetUtxosFromAddressResult (GetUtxosFromAddressResult, unAddressUtxosResult))
import Marconi.Sidechain.Api.Types (sidechainAddressUtxoIndexer, sidechainEnvIndexers)
import Marconi.Sidechain.Bootstrap (initializeSidechainEnv)
import Network.JsonRpc.Client.Types ()
import Network.JsonRpc.Types (JsonRpcResponse (Result))
import Spec.Marconi.Sidechain.RpcClientAction (RpcClientAction (insertUtxoEventsAction, queryAddressUtxosAction, querySyncedBlockAction),
                                               mocUtxoWorker)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: RpcClientAction -> TestTree
tests rpcClientAction = testGroup "marconi-sidechain-utxo query Api Specs"
    [ testPropertyNamed
        "marconi-sidechain-utxo, Insert events and query for utxo's with address in the generated ShelleyEra targetAddresses"
        "queryTargetAddressTest"
        queryTargetAddressTest

    , testGroup "marconi-sidechain-utxo JSON-RPC test-group"
        [ testPropertyNamed
            "Stores UtxoEvents and retrieve them through the RPC server using an RPC client"
            "propUtxoEventInsertionAndJsonRpcQueryRoundTrip"
            (propUtxoEventInsertionAndJsonRpcQueryRoundTrip rpcClientAction)
        , testPropertyNamed
              "stores UtxoEvents, and get the current sync slot"
              "propUtxoEventInsertionAndJsonRpcCurrentSlotQuery"
              (propUtxoEventInsertionAndJsonRpcCurrentSlotQuery rpcClientAction)
        ]
    ]

-- | generate some Utxo events, store and fetch the Utxos, then make sure JSON conversion is idempotent

queryTargetAddressTest :: Property
queryTargetAddressTest = property $ do
  events <- forAll genShelleyEraUtxoEvents
  env <- liftIO $ initializeSidechainEnv Nothing Nothing
  let
    callback :: Utxo.UtxoIndexer -> IO ()
    callback = atomically
             . AddressUtxoIndexer.updateEnvState
                (env ^. sidechainEnvIndexers . sidechainAddressUtxoIndexer) -- update the indexer
  liftIO $ mocUtxoWorker callback events
  fetchedRows <-
    liftIO
    . fmap (fmap concat)
    . traverse (\addr ->
        fmap unAddressUtxosResult <$> AddressUtxoIndexer.findByAddress
            (env ^. sidechainEnvIndexers . sidechainAddressUtxoIndexer)
            addr
            Nothing)
    . Set.toList . Set.fromList  -- required to remove the potential duplicate addresses
    . fmap Utxo._address
    . concatMap (Set.toList . Utxo.ueUtxos)
    $ events

  let numOfFetched = length fetchedRows
  Hedgehog.classify "Retrieved Utxos are greater than or Equal to 5" $ numOfFetched >= 5
  Hedgehog.classify "Retrieved Utxos are greater than 1" $ numOfFetched > 1

  Hedgehog.assert (not . null $ fetchedRows)
  (Set.fromList . mapMaybe (Aeson.decode .  Aeson.encode ) $ fetchedRows) === Set.fromList fetchedRows

-- | Test roundtrip Utxos thruough JSON-RPC http server.
-- We compare a represenation of the generated UtxoEvents
-- with those fetched from the JSON-RPC  server. The purpose of this is:
--   + RPC server routes the request to the correct handler
--   + Events are serialized/deserialized thru the RPC layer and it various wrappers correctly
propUtxoEventInsertionAndJsonRpcQueryRoundTrip
  :: RpcClientAction
  -> Property
propUtxoEventInsertionAndJsonRpcQueryRoundTrip action = property $ do
  events <- forAll genShelleyEraUtxoEvents
  liftIO $ insertUtxoEventsAction action events
  let (qAddresses :: [String]) =
        Set.toList . Set.fromList . fmap (unpack . C.serialiseAddress)
        . mapMaybe (addressAnyToShelley . Utxo._address)
        . concatMap  (Set.toList . Utxo.ueUtxos)
        $ events
  rpcResponses <- liftIO $ for qAddresses (queryAddressUtxosAction action)
  let
    fetchedUtxoRows = concatMap fromQueryResult rpcResponses

  Hedgehog.assert (not . null $ fetchedUtxoRows)
  (Set.fromList . mapMaybe (Aeson.decode . Aeson.encode) $ fetchedUtxoRows) === Set.fromList fetchedUtxoRows

fromQueryResult :: JsonRpcResponse e GetUtxosFromAddressResult -> [AddressUtxoResult]
fromQueryResult (Result _ (GetUtxosFromAddressResult rows) ) = rows
fromQueryResult _otherResponses                              = []

-- | Test inserting events and querying the current sync point
-- We check that the response is the last sync point of the inserted events.
-- The purpose of this is:
--   + RPC server routes the request to the correct handler
--   + Events are serialized/deserialized thru the RPC layer and it various wrappers correctly
propUtxoEventInsertionAndJsonRpcCurrentSlotQuery
  :: RpcClientAction
  -> Property
propUtxoEventInsertionAndJsonRpcCurrentSlotQuery action = property $ do
  events <- forAll genShelleyEraUtxoEvents
  let chainPoints = Utxo.ueChainPoint <$> events
      getChainPointSlot C.ChainPointAtGenesis   = Nothing
      getChainPointSlot (C.ChainPoint slotNo _) = Just slotNo
  Hedgehog.cover 90 "Non empty events" $ not $ null events

  -- Now, we are storing the events in the index
  liftIO $ insertUtxoEventsAction action events

  Result _ (GetCurrentSyncedBlockResult resp') <- liftIO $ querySyncedBlockAction action
  Hedgehog.cover 40 "Should have some significant non genesis chainpoints results" $ resp' /= C.ChainPointAtGenesis && getChainPointSlot resp' > Just (C.SlotNo 0)
  assert $ resp' `elem` chainPoints
