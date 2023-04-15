-- | Defines REST and JSON-RPC routes

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Marconi.Sidechain.Api.Routes where

import Cardano.Api qualified as C
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), object, (.:), (.:?), (.=))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Indexers.EpochState qualified as EpochState
import Network.JsonRpc.Types (JsonRpc, RawJsonRpc)
import Servant.API (Get, JSON, PlainText, (:<|>), (:>))

-- | marconi-sidechain APIs
type API = JsonRpcAPI :<|> RestAPI

----------------------------------------------
--  RPC types
--  methodName -> parameter(s) -> return-type
----------------------------------------------

-- | JSON-RPC API, endpoint
type JsonRpcAPI = "json-rpc" :> RawJsonRpc RpcAPI

-- | RPC routes
type RpcAPI = RpcEchoMethod
         :<|> RpcTargetAddressesMethod
         :<|> RpcCurrentSyncedBlockMethod
         :<|> RpcPastAddressUtxoMethod
         :<|> RpcMintingPolicyHashTxMethod
         :<|> RpcEpochStakePoolDelegationMethod
         :<|> RpcEpochNonceMethod

type RpcEchoMethod = JsonRpc "echo" String String String

type RpcTargetAddressesMethod = JsonRpc "getTargetAddresses" String String [Text]

type RpcCurrentSyncedBlockMethod =
    JsonRpc "getCurrentSyncedBlock"
            String
            String
            GetCurrentSyncedBlockResult

type RpcPastAddressUtxoMethod =
    JsonRpc "getUtxosFromAddress"
            GetUtxosFromAddressParams
            String
            GetUtxosFromAddressResult

type RpcMintingPolicyHashTxMethod =
    JsonRpc "getTxsBurningAssetId"
            String
            String
            GetTxsBurningAssetIdResult

type RpcEpochStakePoolDelegationMethod =
    JsonRpc "getStakePoolDelegationByEpoch"
            Word64
            String
            GetEpochStakePoolDelegationResult

type RpcEpochNonceMethod =
    JsonRpc "getNonceByEpoch"
            Word64
            String
            GetEpochNonceResult

--------------------
-- REST related ---
--------------------

-- | REST API, endpoints
type RestAPI = "rest" :> (GetTime :<|> GetTargetAddresses)

type GetTime = "time" :> Get '[PlainText] String

type GetTargetAddresses = "addresses" :> Get '[JSON] [Text]

--------------------------
-- Query and Result types
--------------------------

newtype GetCurrentSyncedBlockResult =
    GetCurrentSyncedBlockResult
        C.ChainPoint
    deriving (Eq, Ord, Generic, Show)

instance ToJSON GetCurrentSyncedBlockResult where
    toJSON (GetCurrentSyncedBlockResult chainPoint) =
        let chainPointObj = case chainPoint of
                              C.ChainPointAtGenesis ->
                                  []
                              (C.ChainPoint (C.SlotNo slotNo) bhh) ->
                                  [ "blockHeaderHash" .= bhh, "slotNo" .= slotNo ]
         in object chainPointObj

instance FromJSON GetCurrentSyncedBlockResult where
    parseJSON (Object v) = do
        (slotNoM :: Maybe C.SlotNo) <- v .:? "slotNo"
        (bhhM :: Maybe (C.Hash C.BlockHeader)) <- v .:? "blockHeaderHash"
        cp <- case (slotNoM, bhhM) of
          (Just slotNo, Just bhh) -> pure $ C.ChainPoint slotNo bhh
          _ifOneIsNothing         -> pure C.ChainPointAtGenesis
        pure $ GetCurrentSyncedBlockResult cp
    parseJSON _ = mempty

data GetUtxosFromAddressParams
    = GetUtxosFromAddressParams
    { queryAddress       :: !String
    , queryUnspentAtSlot :: !(Maybe Word64)
    } deriving (Eq, Show)

instance FromJSON GetUtxosFromAddressParams where
    parseJSON (Object v) = GetUtxosFromAddressParams <$> (v .: "address")  <*> (v .:? "unspentBeforeSlotNo")
    parseJSON _          = mempty

instance ToJSON GetUtxosFromAddressParams where
    toJSON q =
        object $ catMaybes
           [ Just ("address" .= queryAddress q)
           , ("unspentBeforeSlotNo" .=) <$> queryUnspentAtSlot q
           ]

newtype GetUtxosFromAddressResult = GetUtxosFromAddressResult
    { unAddressUtxosResult :: [AddressUtxoResult] }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data AddressUtxoResult = AddressUtxoResult
    !(C.Hash C.BlockHeader)
    !C.SlotNo
    !C.TxId
    !C.TxIx
    !C.AddressAny
    !(Maybe (C.Hash C.ScriptData))
    !(Maybe C.ScriptData)
    deriving (Eq, Ord, Show, Generic)

instance ToJSON AddressUtxoResult where
    toJSON (AddressUtxoResult bhh slotNo txId txIx addr dath dat) =
        object
            [ "blockHeaderHash" .= bhh
            , "slotNo" .= slotNo
            , "txId" .= txId
            , "txIx" .= txIx
            , "address" .= addr
            , "datumHash" .= dath
            , "datum" .= dat
            ]

instance FromJSON AddressUtxoResult where
    parseJSON (Object v) = do
        AddressUtxoResult
            <$> v .: "blockHeaderHash"
            <*> v .: "slotNo"
            <*> v .: "txId"
            <*> v .: "txIx"
            <*> v .: "address"
            <*> v .: "datumHash"
            <*> v .: "datum"
    parseJSON _ = mempty

newtype GetTxsBurningAssetIdParams = GetTxsBurningAssetIdParams
    { policyId :: String
    } deriving (Eq, Show)

instance FromJSON GetTxsBurningAssetIdParams where
    parseJSON (Object v) =
        GetTxsBurningAssetIdParams
            <$> (v .: "policyId")
    parseJSON _          = mempty

instance ToJSON GetTxsBurningAssetIdParams where
    toJSON q = object
       [ "policyId" .= policyId q
       ]

newtype GetTxsBurningAssetIdResult =
    GetTxsBurningAssetIdResult [AssetIdTxResult]
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data AssetIdTxResult = AssetIdTxResult
    !(C.Hash C.BlockHeader)
    !C.SlotNo
    !C.TxId
    !(Maybe (C.Hash C.ScriptData))
    !(Maybe C.ScriptData)
    !C.Quantity
    deriving (Eq, Ord, Show, Generic)

instance ToJSON AssetIdTxResult where
    toJSON (AssetIdTxResult bhh slotNo txId redh red qty) =
        object
            [ "blockHeaderHash" .= bhh
            , "slotNo" .= slotNo
            , "txId" .= txId
            , "redeemerHash" .= redh
            , "redeemer" .= red
            , "quantity" .= qty
            ]

instance FromJSON AssetIdTxResult where
    parseJSON (Object v) = do
        AssetIdTxResult
            <$> v .: "blockHeaderHash"
            <*> v .: "slotNo"
            <*> v .: "txId"
            <*> v .: "redeemerHash"
            <*> v .: "redeemer"
            <*> v .: "quantity"
    parseJSON _ = mempty


newtype GetEpochStakePoolDelegationResult =
    GetEpochStakePoolDelegationResult [EpochState.EpochSDDRow]
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype GetEpochNonceResult =
    GetEpochNonceResult (Maybe EpochState.EpochNonceRow)
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
