{-# LANGUAGE TypeApplications #-}

module Spec.Marconi.Sidechain.Experimental.Routes (tests) where

import Cardano.Api qualified as C
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy (Proxy))
import Marconi.Cardano.Core.Types (TxIndexInBlock (TxIndexInBlock))
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.CurrentSyncedBlock (
  GetCurrentSyncedBlockResult (GetCurrentSyncedBlockResult),
  Tip (Tip),
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
            goldenGetCurrentChainPointGenesisResult
        , goldenVsStringDiff
            "Golden test for CurrentSyncedBlockResult in JSON format when chain point is at point other than genesis"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/current-synced-point-response-2.json"
            goldenCurrentChainPointResult
        , goldenVsStringDiff
            "Golden test for AddressUtxoResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Golden/Routes/address-utxo-response.json"
            goldenGetUtxosFromAddressResult
            -- TODO: PLT-8630
            --        , goldenVsStringDiff
            --            "Golden test for MintingPolicyHashTxResult in JSON format"
            --            (\expected actual -> ["diff", "--color=always", expected, actual])
            --            "test/Spec/Golden/Routes/mintingpolicyhash-tx-response.json"
            --            goldenMintingPolicyHashTxResult
            --        , goldenVsStringDiff
            --            "Golden test for EpochStakePoolDelegationResult in JSON format"
            --            (\expected actual -> ["diff", "--color=always", expected, actual])
            --            "test/Spec/Golden/Routes/epoch-stakepooldelegation-response.json"
            --            goldenEpochStakePoolDelegationResult
            --        , goldenVsStringDiff
            --            "Golden test for EpochNonResult in JSON format"
            --            (\expected actual -> ["diff", "--color=always", expected, actual])
            --            "test/Spec/Golden/Routes/epoch-nonce-response.json"
            --            goldenEpochNonceResult
        ]
    ]

{- GetCurrentSyncedBlockResult -}

emptyGetCurrentSyncedBlockResult :: GetCurrentSyncedBlockResult
emptyGetCurrentSyncedBlockResult = GetCurrentSyncedBlockResult Nothing Nothing Nothing Nothing Nothing Nothing

goldenGetCurrentChainPointGenesisResult :: IO ByteString
goldenGetCurrentChainPointGenesisResult = pure $ Aeson.encodePretty emptyGetCurrentSyncedBlockResult

goldenCurrentChainPointResult :: IO ByteString
goldenCurrentChainPointResult = do
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
