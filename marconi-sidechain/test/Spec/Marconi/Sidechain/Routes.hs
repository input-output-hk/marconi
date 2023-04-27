{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Spec.Marconi.Sidechain.Routes (tests) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Ledger.Shelley.API qualified as Ledger
import Control.Monad (forM)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy (Proxy))
import Gen.Marconi.ChainIndex.Types qualified as Gen
import Hedgehog (Property, forAll, property, tripping)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Marconi.ChainIndex.Indexers.EpochState (EpochNonceRow (EpochNonceRow), EpochSDDRow (EpochSDDRow))
import Marconi.Sidechain.Api.Routes (AddressUtxoResult (AddressUtxoResult), AssetIdTxResult (AssetIdTxResult),
                                     GetCurrentSyncedBlockResult (GetCurrentSyncedBlockResult),
                                     GetEpochNonceResult (GetEpochNonceResult),
                                     GetEpochStakePoolDelegationResult (GetEpochStakePoolDelegationResult),
                                     GetTxsBurningAssetIdParams (GetTxsBurningAssetIdParams),
                                     GetTxsBurningAssetIdResult (GetTxsBurningAssetIdResult),
                                     GetUtxosFromAddressParams (GetUtxosFromAddressParams),
                                     GetUtxosFromAddressResult (GetUtxosFromAddressResult))
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests = testGroup "Spec.Marconi.Sidechain.Routes"
    [ testGroup "ToJSON/FromJSON rountrip"
        [ testPropertyNamed
            "GetCurrentSyncedBlockResult"
            "propJSONRountripCurrentSyncedBlockResult"
            propJSONRountripCurrentSyncedBlockResult
        , testPropertyNamed
            "GetEpochStakePoolDelegationResult"
            "propJSONRountripEpochStakePoolDelegationResult"
            propJSONRountripEpochStakePoolDelegationResult
        , testPropertyNamed
            "GetUtxosFromAddressParams"
            "propJSONRountripGetUtxosFromAddressParams"
            propJSONRountripGetUtxosFromAddressParams
        , testPropertyNamed
            "GetUtxosFromAddressResult"
            "propJSONRountripGetUtxosFromAddressResult"
            propJSONRountripGetUtxosFromAddressResult
        , testPropertyNamed
            "GetTxsBurningAssetIdParams"
            "propJSONRountripGetTxsBurningAssetIdParams"
            propJSONRountripGetTxsBurningAssetIdParams
        , testPropertyNamed
            "GetTxsBurningAssetIdResult"
            "propJSONRountripGetTxsBurningAssetIdResult"
            propJSONRountripGetTxsBurningAssetIdResult
        ]
    , testGroup "Golden test for query results"
        [ goldenVsStringDiff
            "Golden test for CurrentSyncedBlockResult in JSON format when chain point is at genesis"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/current-synced-point-response-1.json"
            goldenCurrentChainPointGenesisResult
        , goldenVsStringDiff
            "Golden test for CurrentSyncedBlockResult in JSON format when chain point is at point other than genesis"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/current-synced-point-response-2.json"
            goldenCurrentChainPointResult
        , goldenVsStringDiff
            "Golden test for AddressUtxoResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/address-utxo-response.json"
            goldenAddressUtxoResult
        , goldenVsStringDiff
            "Golden test for MintingPolicyHashTxResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/mintingpolicyhash-tx-response.json"
            goldenMintingPolicyHashTxResult
        , goldenVsStringDiff
            "Golden test for EpochStakePoolDelegationResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/epoch-stakepooldelegation-response.json"
            goldenEpochStakePoolDelegationResult
        , goldenVsStringDiff
            "Golden test for EpochNonResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/epoch-nonce-response.json"
            goldenEpochNonceResult
        ]
    ]

propJSONRountripCurrentSyncedBlockResult :: Property
propJSONRountripCurrentSyncedBlockResult = property $ do
    cp <- GetCurrentSyncedBlockResult <$> forAll Gen.genChainPoint
    tripping cp Aeson.encode Aeson.decode

propJSONRountripGetUtxosFromAddressParams :: Property
propJSONRountripGetUtxosFromAddressParams = property $ do
    r <- forAll $ GetUtxosFromAddressParams
            <$> Gen.string (Range.linear 1 10) Gen.alphaNum
            <*> Gen.maybe (Gen.word64 (Range.linear 1 100))
    tripping r Aeson.encode Aeson.decode

propJSONRountripGetUtxosFromAddressResult :: Property
propJSONRountripGetUtxosFromAddressResult = property $ do
    r <- fmap GetUtxosFromAddressResult $ forAll $ Gen.list (Range.linear 0 10) $ do
        (C.TxIn txId txIx) <- CGen.genTxIn
        hsd <- Gen.maybe CGen.genHashableScriptData
        AddressUtxoResult
            <$> Gen.genHashBlockHeader
            <*> Gen.genSlotNo
            <*> pure txId
            <*> pure txIx
            <*> (fmap (\(C.AddressInEra _ addr) -> C.toAddressAny addr)
                      $ Gen.genAddressInEra C.BabbageEra
                )
            <*> pure (fmap C.hashScriptDataBytes hsd)
            <*> pure (fmap C.getScriptData hsd)
    tripping r Aeson.encode Aeson.decode

propJSONRountripGetTxsBurningAssetIdParams :: Property
propJSONRountripGetTxsBurningAssetIdParams = property $ do
    r <- forAll $ GetTxsBurningAssetIdParams
            <$> Gen.string (Range.linear 1 10) Gen.alphaNum
    tripping r Aeson.encode Aeson.decode

propJSONRountripGetTxsBurningAssetIdResult :: Property
propJSONRountripGetTxsBurningAssetIdResult = property $ do
    r <- fmap GetTxsBurningAssetIdResult $ forAll $ Gen.list (Range.linear 0 10) $ do
        hsd <- Gen.maybe CGen.genHashableScriptData
        AssetIdTxResult
            <$> Gen.genHashBlockHeader
            <*> Gen.genSlotNo
            <*> CGen.genTxId
            <*> pure (fmap C.hashScriptDataBytes hsd)
            <*> pure (fmap C.getScriptData hsd)
            <*> Gen.genQuantity (Range.linear 0 10)
    tripping r Aeson.encode Aeson.decode

propJSONRountripEpochStakePoolDelegationResult :: Property
propJSONRountripEpochStakePoolDelegationResult = property $ do
    sdds <- fmap GetEpochStakePoolDelegationResult $ forAll $ Gen.list (Range.linear 1 10) $ do
        EpochSDDRow
            <$> Gen.genEpochNo
            <*> Gen.genPoolId
            <*> CGen.genLovelace
            <*> Gen.genSlotNo
            <*> Gen.genHashBlockHeader
            <*> Gen.genBlockNo
    tripping sdds Aeson.encode Aeson.decode

goldenCurrentChainPointGenesisResult :: IO ByteString
goldenCurrentChainPointGenesisResult = do
    pure $ Aeson.encodePretty $ GetCurrentSyncedBlockResult C.ChainPointAtGenesis

goldenCurrentChainPointResult :: IO ByteString
goldenCurrentChainPointResult = do
    let blockHeaderHashRawBytes = "6161616161616161616161616161616161616161616161616161616161616161"
    blockHeaderHash <-
        either
            (error . show)
            pure
            $ C.deserialiseFromRawBytesHex (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader)) blockHeaderHashRawBytes

    pure $ Aeson.encodePretty $ GetCurrentSyncedBlockResult $ C.ChainPoint (C.SlotNo 1) blockHeaderHash

goldenAddressUtxoResult :: IO ByteString
goldenAddressUtxoResult = do
    let addressBech32 = "addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc"
    addr <-
        either
            (error . show)
            pure
            $ C.deserialiseFromBech32 (C.AsAddress C.AsShelleyAddr) addressBech32

    let datumCbor = "4"
    datum <-
        either
            (error . show)
            pure
            $ C.deserialiseFromCBOR C.AsScriptData datumCbor

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
            $ C.deserialiseFromRawBytesHex (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader)) blockHeaderHashRawBytes

    let utxos =
            [ AddressUtxoResult
                blockHeaderHash
                (C.SlotNo 1)
                txId
                (C.TxIx 0)
                (C.AddressShelley addr)
                Nothing
                Nothing
            , AddressUtxoResult
                blockHeaderHash
                (C.SlotNo 1)
                txId
                (C.TxIx 0)
                (C.AddressShelley addr)
                (Just $ C.hashScriptDataBytes $ C.unsafeHashableScriptData datum)
                (Just datum)
            ]
        result = GetUtxosFromAddressResult utxos
    pure $ Aeson.encodePretty result

goldenMintingPolicyHashTxResult :: IO ByteString
goldenMintingPolicyHashTxResult = do
    let redeemerCbor = "4"
    redeemerData <-
        either
            (error . show)
            pure
            $ C.deserialiseFromCBOR C.AsScriptData redeemerCbor

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
            $ C.deserialiseFromRawBytesHex (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader)) blockHeaderHashRawBytes

    let mints =
            [ AssetIdTxResult
                blockHeaderHash
                (C.SlotNo 1)
                txId
                (Just $ C.hashScriptDataBytes $ C.unsafeHashableScriptData redeemerData)
                (Just redeemerData)
                (C.Quantity $ -10)
            , AssetIdTxResult
                blockHeaderHash
                (C.SlotNo 1)
                txId
                (Just $ C.hashScriptDataBytes $ C.unsafeHashableScriptData redeemerData)
                (Just redeemerData)
                (C.Quantity 10)
            ]
        result = GetTxsBurningAssetIdResult mints
    pure $ Aeson.encodePretty result

goldenEpochStakePoolDelegationResult :: IO ByteString
goldenEpochStakePoolDelegationResult = do
    let blockHeaderHashRawBytes = "578f3cb70f4153e1622db792fea9005c80ff80f83df028210c7a914fb780a6f6"
    blockHeaderHash <-
        either
            (error . show)
            pure
            $ C.deserialiseFromRawBytesHex (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader)) blockHeaderHashRawBytes

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
        slotNo = C.SlotNo 1382422
        epochNo = C.EpochNo 6
        blockNo = C.BlockNo 64903

    let sdds = fmap (\poolId -> EpochSDDRow epochNo poolId lovelace slotNo blockHeaderHash blockNo) poolIds
        result = GetEpochStakePoolDelegationResult sdds
    pure $ Aeson.encodePretty result

goldenEpochNonceResult :: IO ByteString
goldenEpochNonceResult = do
    let blockHeaderHashRawBytes = "fdd5eb1b1e9fc278a08aef2f6c0fe9b576efd76966cc552d8c5a59271dc01604"
    blockHeaderHash <-
        either
            (error . show)
            pure
            $ C.deserialiseFromRawBytesHex (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader)) blockHeaderHashRawBytes

    let nonce = Ledger.Nonce
              $ Crypto.castHash
              $ Crypto.hashWith id "162d29c4e1cf6b8a84f2d692e67a3ac6bc7851bc3e6e4afe64d15778bed8bd86"

    let result = GetEpochNonceResult
               $ Just
               $ EpochNonceRow
                    (C.EpochNo 4)
                    nonce
                    (C.SlotNo 518400)
                    blockHeaderHash
                    (C.BlockNo 21645)
    pure $ Aeson.encodePretty result
