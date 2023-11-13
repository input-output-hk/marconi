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
import Data.Text (Text)
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
  EpochNonceResult (EpochNonceResult),
  nonceToMaybe,
 )
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.MintBurnToken (
  BurnTokenEventResult (BurnTokenEventResult),
  GetBurnTokenEventsResult (GetBurnTokenEventsResult),
 )
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo (
  AddressUtxoResult (AddressUtxoResult),
  GetUtxosFromAddressResult (GetUtxosFromAddressResult),
  SpentInfoResult (SpentInfoResult),
 )
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.Wrappers (
  UtxoTxInput (UtxoTxInput),
  ValueWrapper (ValueWrapper),
 )
import Marconi.ChainIndex.Types (TxIndexInBlock (TxIndexInBlock))
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
            "Golden test for EpochStakePoolDelegationAtGenesisResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/epoch-stakepooldelegation-at-genesis-response.json"
            goldenEpochStakePoolDelegationAtGenesisResult
        , goldenVsStringDiff
            "Golden test for AddressUtxoResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/address-utxo-response.json"
            goldenAddressUtxoResult
        , goldenVsStringDiff
            "Golden test for EpochNonResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/epoch-nonce-response.json"
            goldenEpochNonceResult
        , goldenVsStringDiff
            "Golden test for EpochNonResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/epoch-nonce-byron-response.json"
            goldenEpochNonceByronResult
        , goldenVsStringDiff
            "Golden test for EpochNonResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/epoch-nonce-at-genesis-response.json"
            goldenEpochNonceAtGenesisResult
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
goldenCurrentChainPointGenesisResult =
  do
    pure
    $ Aeson.encodePretty
    $ GetCurrentSyncedBlockResult Nothing Nothing Nothing Nothing Nothing Nothing

goldenCurrentChainPointResult :: IO ByteString
goldenCurrentChainPointResult = do
  let blockHeaderHashRawBytes = "6161616161616161616161616161616161616161616161616161616161616161"
      epochNo = C.EpochNo 6
      blockNo = C.BlockNo 64903
      blockTimestamp = 0
  blockHeaderHash <- getBlockHeaderHash blockHeaderHashRawBytes

  pure $
    Aeson.encodePretty $
      GetCurrentSyncedBlockResult
        (Just blockNo)
        (Just blockTimestamp)
        (Just blockHeaderHash)
        (Just (C.SlotNo 1))
        (Just epochNo)
        (Just $ Tip blockNo blockHeaderHash (C.SlotNo 1))

goldenMintingPolicyHashTxResult :: IO ByteString
goldenMintingPolicyHashTxResult = do
  let redeemerData = C.ScriptDataNumber 34
  let txIdRawBytes = "ec7d3bd7c6a3a31368093b077af0db46ceac77956999eb842373e08c6420f000"
  txId <- getTxIdHash txIdRawBytes

  let blockHeaderHashRawBytes = "6161616161616161616161616161616161616161616161616161616161616161"
  blockHeaderHash <- getBlockHeaderHash blockHeaderHashRawBytes

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

goldenAddressUtxoResult :: IO ByteString
goldenAddressUtxoResult = do
  let datum = C.ScriptDataNumber 34
  let txIdRawBytes = "ec7d3bd7c6a3a31368093b077af0db46ceac77956999eb842373e08c6420f000"
  txId <-
    either
      (error . show)
      pure
      $ C.deserialiseFromRawBytesHex C.AsTxId txIdRawBytes

  let txId2RawBytes = "2f1f574c0365afd9865332eec4ff75e599d80c525afc7b7d6e38d27d0a01bf47"
  txId2 <-
    either
      (error . show)
      pure
      $ C.deserialiseFromRawBytesHex C.AsTxId txId2RawBytes

  let blockHeaderHashRawBytes = "6161616161616161616161616161616161616161616161616161616161616161"
  blockHeaderHash <-
    either
      (error . show)
      pure
      $ C.deserialiseFromRawBytesHex
        (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader))
        blockHeaderHashRawBytes

  let spentTxIdRawBytes = "2e19f40cdf462444234d0de049163d5269ee1150feda868560315346dd12807d"
  spentTxId <-
    either
      (error . show)
      pure
      $ C.deserialiseFromRawBytesHex C.AsTxId spentTxIdRawBytes

  let utxos =
        [ AddressUtxoResult
            (C.SlotNo 1)
            blockHeaderHash
            (C.EpochNo 0)
            (C.BlockNo 1)
            (TxIndexInBlock 0)
            txId
            (C.TxIx 0)
            Nothing
            Nothing
            (ValueWrapper $ C.valueFromList [(C.AdaAssetId, 10)])
            Nothing
            [UtxoTxInput txId2 (C.TxIx 1)]
        , AddressUtxoResult
            (C.SlotNo 1)
            blockHeaderHash
            (C.EpochNo 0)
            (C.BlockNo 1)
            (TxIndexInBlock 0)
            txId
            (C.TxIx 0)
            (Just $ C.hashScriptDataBytes $ C.unsafeHashableScriptData datum)
            (Just datum)
            (ValueWrapper $ C.valueFromList [(C.AdaAssetId, 1)])
            (Just $ SpentInfoResult (C.SlotNo 12) spentTxId)
            [UtxoTxInput txId (C.TxIx 0)]
        ]
      result = GetUtxosFromAddressResult utxos
  pure $ Aeson.encodePretty result

goldenEpochStakePoolDelegationResult :: IO ByteString
goldenEpochStakePoolDelegationResult = do
  let blockHeaderHashRawBytes = "578f3cb70f4153e1622db792fea9005c80ff80f83df028210c7a914fb780a6f6"
  blockHeaderHash <- getBlockHeaderHash blockHeaderHashRawBytes

  let poolIdsBech32 =
        [ "pool1z22x50lqsrwent6en0llzzs9e577rx7n3mv9kfw7udwa2rf42fa"
        , "pool1547tew8vmuj0g6vj3k5jfddudextcw6hsk2hwgg6pkhk7lwphe6"
        , "pool174mw7e20768e8vj4fn8y6p536n8rkzswsapwtwn354dckpjqzr8"
        ]
  poolIds <- getStakePoolHashes poolIdsBech32

  let lovelace = C.Lovelace 100000000000000
      slotNo = Just $ C.SlotNo 1382422
      blockNo = C.BlockNo 64903
      epochNo = C.EpochNo 123

  let result =
        fmap
          (\poolId -> ActiveSDDResult poolId lovelace slotNo (Just blockHeaderHash) blockNo epochNo)
          poolIds
  pure $ Aeson.encodePretty result

goldenEpochStakePoolDelegationAtGenesisResult :: IO ByteString
goldenEpochStakePoolDelegationAtGenesisResult = do
  let poolIdsBech32 =
        [ "pool1z22x50lqsrwent6en0llzzs9e577rx7n3mv9kfw7udwa2rf42fa"
        ]
  poolIds <- getStakePoolHashes poolIdsBech32

  let lovelace = C.Lovelace 100000000000000
      slotNo = Nothing
      blockHeaderHash = Nothing
      blockNo = C.BlockNo 0
      epochNo = C.EpochNo 0

  let result =
        fmap
          (\poolId -> ActiveSDDResult poolId lovelace slotNo blockHeaderHash blockNo epochNo)
          poolIds
  pure $ Aeson.encodePretty result

goldenEpochNonceResult :: IO ByteString
goldenEpochNonceResult = do
  let nonce =
        Ledger.Nonce $
          Crypto.castHash $
            Crypto.hashWith id "162d29c4e1cf6b8a84f2d692e67a3ac6bc7851bc3e6e4afe64d15778bed8bd86"
      epochNo = Just $ C.EpochNo 2
      blockNo = Just $ C.BlockNo 21645
      slotNo = Just $ C.SlotNo 1382422
      blockHeaderHashRawBytes = "578f3cb70f4153e1622db792fea9005c80ff80f83df028210c7a914fb780a6f6"
  blockHeaderHash <- getBlockHeaderHash blockHeaderHashRawBytes

  let result =
        EpochNonceResult (Just blockHeaderHash) blockNo epochNo slotNo (nonceToMaybe nonce)
  pure $ Aeson.encodePretty result

goldenEpochNonceByronResult :: IO ByteString
goldenEpochNonceByronResult = do
  let epochNo = Just $ C.EpochNo 2
      blockNo = Just $ C.BlockNo 21645
      slotNo = Just $ C.SlotNo 1382422
      blockHeaderHashRawBytes = "578f3cb70f4153e1622db792fea9005c80ff80f83df028210c7a914fb780a6f6"
  blockHeaderHash <- getBlockHeaderHash blockHeaderHashRawBytes

  let result =
        EpochNonceResult (Just blockHeaderHash) blockNo epochNo slotNo (nonceToMaybe Ledger.NeutralNonce)
  pure $ Aeson.encodePretty result

goldenEpochNonceAtGenesisResult :: IO ByteString
goldenEpochNonceAtGenesisResult = do
  let result =
        EpochNonceResult Nothing Nothing Nothing Nothing (nonceToMaybe Ledger.NeutralNonce)
  pure $ Aeson.encodePretty result

getBlockHeaderHash :: (Applicative f) => Crypto.ByteString -> f (CS.Hash CS.BlockHeader)
getBlockHeaderHash rawBytes =
  either
    (error . show)
    pure
    $ C.deserialiseFromRawBytesHex
      (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader))
      rawBytes

getTxIdHash :: (Applicative f) => Crypto.ByteString -> f CS.TxId
getTxIdHash rawBytes =
  either
    (error . show)
    pure
    $ C.deserialiseFromRawBytesHex C.AsTxId rawBytes

getStakePoolHashes :: (Traversable t, Monad m) => t Text -> m (t (CS.Hash CS.StakePoolKey))
getStakePoolHashes poolIdsBech32 =
  forM poolIdsBech32 $ \poolIdBech32 -> do
    either
      (error . show)
      pure
      $ C.deserialiseFromBech32 (C.AsHash (C.proxyToAsType $ Proxy @CS.StakePoolKey)) poolIdBech32
