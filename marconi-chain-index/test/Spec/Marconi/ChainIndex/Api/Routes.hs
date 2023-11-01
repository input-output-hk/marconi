{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Marconi.ChainIndex.Api.Routes (tests) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as CS
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Ledger.Shelley.API qualified as Ledger
import Control.Monad (forM)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy (Proxy))
import Hedgehog (
  Property,
  forAll,
  property,
  tripping,
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.CurrentSyncedBlock (
  GetCurrentSyncedBlockResult (GetCurrentSyncedBlockResult),
 )
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.CurrentSyncedBlock.Tip (Tip (Tip))
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.EpochState (
  ActiveSDDResult (ActiveSDDResult),
 )
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.MintBurnToken (
  BurnTokenEventResult (BurnTokenEventResult),
  GetBurnTokenEventsResult (GetBurnTokenEventsResult),
 )
import Marconi.ChainIndex.Indexers.EpochState (EpochNonce (EpochNonce))
import Spec.Marconi.ChainIndex.Api.Gen (
  genBurnTokenEventResult,
  genGetBurnTokenEventsParams,
 )
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.ChainIndex.Api.Routes"
    [ testGroup
        "ToJSON/FromJSON rountrip"
        [ testPropertyNamed
            "GetBurnTokenEventsParams"
            "propJSONRountripGetBurnTokenEventsParams"
            propJSONRountripGetBurnTokenEventsParams
        , testPropertyNamed
            "GetBurnTokenEventsResult"
            "propJSONRountripGetBurnTokenEventsResult"
            propJSONRountripGetBurnTokenEventsResult
        ]
    , testGroup
        "Golden test for query results"
        [ goldenVsStringDiff
            "Golden test for CurrentSyncedBlockResult in JSON format when chain point is at genesis"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/current-synced-point-response-1.json"
            goldenCurrentChainPointGenesisResult
        , goldenVsStringDiff
            "Golden test for CurrentSyncedBlockResult in JSON format when chain point is at point other than genesis"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/current-synced-point-response-2.json"
            goldenCurrentChainPointResult
        , goldenVsStringDiff
            "Golden test for MintingPolicyHashTxResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/mintingpolicyhash-tx-response.json"
            goldenMintingPolicyHashTxResult
        , goldenVsStringDiff
            "Golden test for EpochStakePoolDelegationResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/epoch-stakepooldelegation-response.json"
            goldenEpochStakePoolDelegationResult
        , goldenVsStringDiff
            "Golden test for EpochNonResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/epoch-nonce-response.json"
            goldenEpochNonceResult
        ]
    ]

propJSONRountripGetBurnTokenEventsParams :: Property
propJSONRountripGetBurnTokenEventsParams = property $ do
  r <- forAll genGetBurnTokenEventsParams
  tripping r Aeson.encode Aeson.eitherDecode

propJSONRountripGetBurnTokenEventsResult :: Property
propJSONRountripGetBurnTokenEventsResult = property $ do
  r <- fmap GetBurnTokenEventsResult $ forAll $ Gen.list (Range.linear 0 10) $ do
    hsd <- Gen.maybe CGen.genHashableScriptData
    genBurnTokenEventResult hsd
  tripping r Aeson.encode Aeson.eitherDecode

goldenCurrentChainPointGenesisResult :: IO ByteString
goldenCurrentChainPointGenesisResult = do
  pure $ Aeson.encodePretty $ GetCurrentSyncedBlockResult Nothing Nothing Nothing Nothing

goldenCurrentChainPointResult :: IO ByteString
goldenCurrentChainPointResult = do
  let blockHeaderHashRawBytes = "6161616161616161616161616161616161616161616161616161616161616161"
      blockNo = C.BlockNo 64903
  blockHeaderHash <-
    either
      (error . show)
      pure
      $ C.deserialiseFromRawBytesHex
        (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader))
        blockHeaderHashRawBytes

  pure $
    Aeson.encodePretty $
      GetCurrentSyncedBlockResult
        (Just blockNo)
        (Just blockHeaderHash)
        (Just (C.SlotNo 1))
        (Just $ Tip blockNo blockHeaderHash (C.SlotNo 1))

goldenMintingPolicyHashTxResult :: IO ByteString
goldenMintingPolicyHashTxResult = do
  let redeemerData = C.ScriptDataNumber 34
  let txIdRawBytes = "ec7d3bd7c6a3a31368093b077af0db46ceac77956999eb842373e08c6420f000"
  txId <-
    either
      (error . show)
      pure
      $ C.deserialiseFromRawBytesHex C.AsTxId txIdRawBytes

  let blockHeaderHashRawBytes = "6161616161616161616161616161616161616161616161616161616161616161"
  blockHeaderHash <-
    either
      (error . show)
      pure
      $ C.deserialiseFromRawBytesHex
        (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader))
        blockHeaderHashRawBytes

  let mints =
        [ BurnTokenEventResult
            (Just (C.SlotNo 1))
            (Just blockHeaderHash)
            (C.BlockNo 1047)
            txId
            (Just $ C.hashScriptDataBytes $ C.unsafeHashableScriptData redeemerData)
            (Just redeemerData)
            (C.AssetName "")
            (C.Quantity 10)
            True
        ]
      result = GetBurnTokenEventsResult mints
  pure $ Aeson.encodePretty result

goldenEpochStakePoolDelegationResult :: IO ByteString
goldenEpochStakePoolDelegationResult = do
  let blockHeaderHashRawBytes = "578f3cb70f4153e1622db792fea9005c80ff80f83df028210c7a914fb780a6f6"
  blockHeaderHash <-
    either
      (error . show)
      pure
      $ C.deserialiseFromRawBytesHex
        (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader))
        blockHeaderHashRawBytes

  let poolIdsBech32 =
        [ "pool1z22x50lqsrwent6en0llzzs9e577rx7n3mv9kfw7udwa2rf42fa"
        , "pool1547tew8vmuj0g6vj3k5jfddudextcw6hsk2hwgg6pkhk7lwphe6"
        , "pool174mw7e20768e8vj4fn8y6p536n8rkzswsapwtwn354dckpjqzr8"
        ]
  poolIds <- forM poolIdsBech32 $ \poolIdBech32 -> do
    either
      (error . show)
      pure
      $ C.deserialiseFromBech32 (C.AsHash (C.proxyToAsType $ Proxy @CS.StakePoolKey)) poolIdBech32

  let lovelace = C.Lovelace 100000000000000
      slotNo = Just $ C.SlotNo 1382422
      blockNo = C.BlockNo 64903

  let sdds = fmap (\poolId -> ActiveSDDResult poolId lovelace slotNo (Just blockHeaderHash) blockNo) poolIds
      result = sdds
  pure $ Aeson.encodePretty result

goldenEpochNonceResult :: IO ByteString
goldenEpochNonceResult = do
  let nonce =
        Ledger.Nonce $
          Crypto.castHash $
            Crypto.hashWith id "162d29c4e1cf6b8a84f2d692e67a3ac6bc7851bc3e6e4afe64d15778bed8bd86"

  let result =
        Just $
          EpochNonce
            (C.EpochNo 2)
            nonce
            (C.BlockNo 21645)
  pure $ Aeson.encodePretty result
