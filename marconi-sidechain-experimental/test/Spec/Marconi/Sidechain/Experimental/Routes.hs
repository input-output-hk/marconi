{-# LANGUAGE TypeApplications #-}

module Spec.Marconi.Sidechain.Experimental.Routes (tests) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Crypto.Hash.Class qualified as Crypto
import Control.Monad (forM)
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy (Proxy))
import Marconi.Cardano.Core.Types (TxIndexInBlock (TxIndexInBlock))
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.BurnTokenEvent (
  BurnTokenEventResult (BurnTokenEventResult),
  GetBurnTokenEventsResult (GetBurnTokenEventsResult),
 )
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.CurrentSyncedBlock (
  GetCurrentSyncedBlockResult (GetCurrentSyncedBlockResult),
  Tip (Tip),
 )
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.EpochActiveStakePoolDelegation (
  ActiveSDDResult (ActiveSDDResult),
  GetEpochActiveStakePoolDelegationResult (GetEpochActiveStakePoolDelegationResult),
 )
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.EpochNonce (
  NonceResult (NonceResult),
 )
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo (
  AddressUtxoResult (AddressUtxoResult),
  GetUtxosFromAddressResult (GetUtxosFromAddressResult),
  SpentInfoResult (SpentInfoResult),
  UtxoTxInput (UtxoTxInput),
  ValueWrapper (ValueWrapper),
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
            goldenGetCurrentSyncedBlockChainPointGenesisResult
        , goldenVsStringDiff
            "Golden test for GetCurrentSyncedBlockResult in JSON format when chain point is at point other than genesis"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/current-synced-point-response-2.json"
            goldenCurrentSyncedBlockChainPointResult
        , goldenVsStringDiff
            "Golden test for GetUtxosFromAddressResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/address-utxo-response.json"
            goldenGetUtxosFromAddressResult
        , goldenVsStringDiff
            "Golden test for GetBurnTokenEventsResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/mintingpolicyhash-tx-response.json"
            goldenGetBurnTokenEventResult
        , goldenVsStringDiff
            "Golden test for GetEpochStakePoolDelegationResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/epoch-stakepooldelegation-response.json"
            goldenGetEpochActiveStakePoolDelegationResult
        , goldenVsStringDiff
            "Golden test for GetEpochNonceResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/epoch-nonce-response.json"
            goldenGetEpochNonceResult
        ]
    ]

{- GetCurrentSyncedBlockResult -}

emptyGetCurrentSyncedBlockResult :: GetCurrentSyncedBlockResult
emptyGetCurrentSyncedBlockResult = GetCurrentSyncedBlockResult Nothing Nothing Nothing Nothing Nothing Nothing

goldenGetCurrentSyncedBlockChainPointGenesisResult :: IO ByteString
goldenGetCurrentSyncedBlockChainPointGenesisResult = pure $ Aeson.encodePretty emptyGetCurrentSyncedBlockResult

goldenCurrentSyncedBlockChainPointResult :: IO ByteString
goldenCurrentSyncedBlockChainPointResult = do
  let blockHeaderHashRawBytes = "6161616161616161616161616161616161616161616161616161616161616161"
      epochNo = C.EpochNo 6
      blockNo = C.BlockNo 64903
      blockTimestamp = 0
      slotNo = C.SlotNo 1
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
        (Just blockTimestamp)
        (Just blockHeaderHash)
        (Just slotNo)
        (Just epochNo)
        (Just $ Tip blockNo blockHeaderHash slotNo)

{- GetUtxosFromAddressResult -}
goldenGetUtxosFromAddressResult :: IO ByteString
goldenGetUtxosFromAddressResult = do
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

{- GetBurnTokenEvent -}
goldenGetBurnTokenEventResult :: IO ByteString
goldenGetBurnTokenEventResult = do
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
            (Just $ C.SlotNo 1)
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

{- GetEpochActiveStakePoolDelegationResult -}

goldenGetEpochActiveStakePoolDelegationResult :: IO ByteString
goldenGetEpochActiveStakePoolDelegationResult = do
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
      $ C.deserialiseFromBech32 (C.AsHash (C.proxyToAsType $ Proxy @C.StakePoolKey)) poolIdBech32

  let lovelace = C.Lovelace 100000000000000
      slotNo = Just $ C.SlotNo 1382422
      blockNo = C.BlockNo 64903

  let sdds = fmap (\poolId -> ActiveSDDResult poolId lovelace slotNo (Just blockHeaderHash) blockNo) poolIds
      result = GetEpochActiveStakePoolDelegationResult sdds
  pure $ Aeson.encodePretty result

{- GetEpochNonceResult -}

goldenGetEpochNonceResult :: IO ByteString
goldenGetEpochNonceResult = do
  let blockHeaderHashRawBytes = "fdd5eb1b1e9fc278a08aef2f6c0fe9b576efd76966cc552d8c5a59271dc01604"
  blockHeaderHash <-
    either
      (error . show)
      pure
      $ C.deserialiseFromRawBytesHex
        (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader))
        blockHeaderHashRawBytes

  let nonce =
        Just $
          Crypto.castHash $
            Crypto.hashWith id "162d29c4e1cf6b8a84f2d692e67a3ac6bc7851bc3e6e4afe64d15778bed8bd86"

  let result =
        Just $
          NonceResult
            nonce
            (Just $ C.SlotNo 518400)
            (Just blockHeaderHash)
            (C.BlockNo 21645)
  pure $ Aeson.encodePretty result
