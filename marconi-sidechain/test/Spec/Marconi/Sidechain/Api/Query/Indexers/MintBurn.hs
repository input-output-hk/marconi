{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Sidechain.Api.Query.Indexers.MintBurn (tests) where

import Cardano.Api (AssetName, PolicyId)
import Control.Concurrent.STM (atomically)
import Control.Lens.Operators ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Traversable (for)
import Gen.Marconi.ChainIndex.Indexers.MintBurn (genMintEvents)
import Hedgehog (Property, forAll, property, (===))
import Hedgehog qualified
import Marconi.ChainIndex.Indexers.MintBurn (MintAsset (mintAssetAssetName, mintAssetPolicyId))
import Marconi.ChainIndex.Indexers.MintBurn qualified as MintBurn
import Marconi.Sidechain.Api.Query.Indexers.MintBurn qualified as MintBurnIndexer
import Marconi.Sidechain.Api.Routes (
  AssetIdTxResult,
  GetBurnTokenEventsResult (GetBurnTokenEventsResult),
 )
import Marconi.Sidechain.Api.Types (
  mintBurnIndexerEnvIndexer,
  sidechainEnvIndexers,
  sidechainMintBurnIndexer,
 )
import Marconi.Sidechain.Bootstrap (initializeSidechainEnv)
import Network.JsonRpc.Client.Types ()
import Network.JsonRpc.Types (JsonRpcResponse (Result))
import Spec.Marconi.Sidechain.RpcClientAction (
  RpcClientAction,
  insertMintBurnEventsAction,
  mocMintBurnWorker,
  queryMintBurnAction,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: RpcClientAction -> TestTree
tests rpcClientAction =
  testGroup
    "marconi-sidechain-minting-policy query Api Specs"
    [ testPropertyNamed
        "marconi-sidechain-minting-policy"
        "queryMintingPolicyTest"
        queryMintingPolicyTest
    , testGroup
        "marconi-sidechain-mint-burn JSON-RPC test-group"
        [ testPropertyNamed
            "Stores MintBurnEvents and retrieve them through the RPC server using an RPC client"
            "propMintBurnEventInsertionAndJsonRpcQueryRoundTrip"
            (propMintBurnEventInsertionAndJsonRpcQueryRoundTrip rpcClientAction)
        ]
    ]

-- | generate some MintBurn events, store and fetch the AssetTxResults, then make sure JSON conversion is idempotent
queryMintingPolicyTest :: Property
queryMintingPolicyTest = property $ do
  (events, _) <- forAll genMintEvents
  env <- liftIO $ initializeSidechainEnv Nothing Nothing Nothing
  let callback :: MintBurn.MintBurnIndexer -> IO ()
      callback =
        atomically
          . MintBurnIndexer.updateEnvState
            (env ^. sidechainEnvIndexers . sidechainMintBurnIndexer . mintBurnIndexerEnvIndexer)
  liftIO $ mocMintBurnWorker callback $ MintBurn.MintBurnEvent <$> events
  fetchedRows <-
    liftIO
      . fmap (fmap (concatMap pure))
      . traverse
        ( \params ->
            MintBurnIndexer.findByAssetIdAtSlot
              env
              (mintAssetPolicyId params)
              (Just $ mintAssetAssetName params)
              Nothing
        )
      . Set.toList
      . Set.fromList -- required to remove the potential duplicate assets
      . concatMap (NonEmpty.toList . MintBurn.txMintAsset)
      . foldMap MintBurn.txMintEventTxAssets
      $ events

  let numOfFetched = length fetchedRows
  Hedgehog.classify "Retrieved MintBurnEvents are greater than or Equal to 5" $ numOfFetched >= 5
  Hedgehog.classify "Retrieved MintBurnEvents are greater than 1" $ numOfFetched > 1

  (Set.fromList . mapMaybe (Aeson.decode . Aeson.encode) $ fetchedRows) === Set.fromList fetchedRows

{- | Test roundtrip MintBurnEvents thruough JSON-RPC http server.
 We compare a represenation of the generated MintBurnEvents
 with those fetched from the JSON-RPC  server. The purpose of this is:
   + RPC server routes the request to the correct handler
   + Events are serialized/deserialized thru the RPC layer and it various wrappers correctly
-}
propMintBurnEventInsertionAndJsonRpcQueryRoundTrip
  :: RpcClientAction
  -> Property
propMintBurnEventInsertionAndJsonRpcQueryRoundTrip action = property $ do
  (events, _) <- forAll genMintEvents
  liftIO $ insertMintBurnEventsAction action $ MintBurn.MintBurnEvent <$> events
  let (qParams :: [(PolicyId, Maybe AssetName)]) =
        Set.toList $
          Set.fromList $
            fmap (\mps -> (mintAssetPolicyId mps, Just $ mintAssetAssetName mps)) $
              concatMap (NonEmpty.toList . MintBurn.txMintAsset) $
                foldMap MintBurn.txMintEventTxAssets events
  rpcResponses <- liftIO $ for qParams (queryMintBurnAction action)
  let fetchedBurnEventRows = concatMap fromQueryResult rpcResponses

  (Set.fromList $ mapMaybe (Aeson.decode . Aeson.encode) fetchedBurnEventRows)
    === Set.fromList fetchedBurnEventRows

fromQueryResult :: JsonRpcResponse e GetBurnTokenEventsResult -> [AssetIdTxResult]
fromQueryResult (Result _ (GetBurnTokenEventsResult rows)) = rows
fromQueryResult _otherResponses = []
