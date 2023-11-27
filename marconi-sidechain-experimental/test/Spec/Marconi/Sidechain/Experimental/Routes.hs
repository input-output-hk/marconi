module Spec.Marconi.Sidechain.Experimental.Routes (tests) where

import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.CurrentSyncedBlock (
  GetCurrentSyncedBlockResult (GetCurrentSyncedBlockResult),
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.Sidechain.Experimental.Routes"
    [ testGroup
        "Golden test for query result JSON shapes"
        [ goldenVsStringDiff
            "Golden test for GetCurrentSyncedBlockResult in JSON format when chain point is at genesis"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/current-synced-point-response-1.json"
            goldenGetCurrentChainPointGenesisResult
            -- TODO: PLT-8630
            --        , goldenVsStringDiff
            --            "Golden test for CurrentSyncedBlockResult in JSON format when chain point is at point other than genesis"
            --            (\expected actual -> ["diff", "--color=always", expected, actual])
            --            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/current-synced-point-response-2.json"
            --            goldenCurrentChainPointResult
            --        , goldenVsStringDiff
            --            "Golden test for AddressUtxoResult in JSON format"
            --            (\expected actual -> ["diff", "--color=always", expected, actual])
            --            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/address-utxo-response.json"
            --            goldenAddressUtxoResult
            --        , goldenVsStringDiff
            --            "Golden test for MintingPolicyHashTxResult in JSON format"
            --            (\expected actual -> ["diff", "--color=always", expected, actual])
            --            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/mintingpolicyhash-tx-response.json"
            --            goldenMintingPolicyHashTxResult
            --        , goldenVsStringDiff
            --            "Golden test for EpochStakePoolDelegationResult in JSON format"
            --            (\expected actual -> ["diff", "--color=always", expected, actual])
            --            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/epoch-stakepooldelegation-response.json"
            --            goldenEpochStakePoolDelegationResult
            --        , goldenVsStringDiff
            --            "Golden test for EpochNonResult in JSON format"
            --            (\expected actual -> ["diff", "--color=always", expected, actual])
            --            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/epoch-nonce-response.json"
            --            goldenEpochNonceResult
        ]
    ]

-- TODO: PLT-8630 put in same order as for api

{- GetCurrentSyncedBlockResult -}

emptyGetCurrentSyncedBlockResult :: GetCurrentSyncedBlockResult
emptyGetCurrentSyncedBlockResult = GetCurrentSyncedBlockResult Nothing Nothing Nothing Nothing Nothing Nothing

goldenGetCurrentChainPointGenesisResult :: IO ByteString
goldenGetCurrentChainPointGenesisResult = pure $ Aeson.encodePretty emptyGetCurrentSyncedBlockResult
