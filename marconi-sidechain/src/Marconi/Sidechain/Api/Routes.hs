{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Defines REST and JSON-RPC routes
module Marconi.Sidechain.Api.Routes where

import Cardano.Api qualified as C
import Cardano.Slotting.Slot (WithOrigin (At, Origin), withOriginFromMaybe)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), object, (.:), (.:?), (.=))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Indexers.EpochState qualified as EpochState
import Marconi.ChainIndex.Indexers.Utxo (BlockInfo (BlockInfo))
import Marconi.ChainIndex.Types (TxIndexInBlock)
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
type RpcAPI =
  RpcEchoMethod
    :<|> RpcTargetAddressesMethod
    :<|> RpcCurrentSyncedBlockMethod
    :<|> RpcPastAddressUtxoMethod
    :<|> RpcGetBurnTokenEventsMethod
    :<|> RpcEpochActiveStakePoolDelegationMethod
    :<|> RpcEpochNonceMethod

type RpcEchoMethod = JsonRpc "echo" String String String

type RpcTargetAddressesMethod = JsonRpc "getTargetAddresses" String String [Text]

type RpcCurrentSyncedBlockMethod =
  JsonRpc
    "getCurrentSyncedBlock"
    String
    String
    GetCurrentSyncedBlockResult

type RpcPastAddressUtxoMethod =
  JsonRpc
    "getUtxosFromAddress"
    GetUtxosFromAddressParams
    String
    GetUtxosFromAddressResult

type RpcGetBurnTokenEventsMethod =
  JsonRpc
    "getBurnTokenEvents"
    GetBurnTokenEventsParams
    String
    GetBurnTokenEventsResult

type RpcEpochActiveStakePoolDelegationMethod =
  JsonRpc
    "getActiveStakePoolDelegationByEpoch"
    Word64
    String
    GetEpochActiveStakePoolDelegationResult

type RpcEpochNonceMethod =
  JsonRpc
    "getNonceByEpoch"
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

newtype GetCurrentSyncedBlockResult
  = GetCurrentSyncedBlockResult (WithOrigin BlockInfo)
  deriving (Eq, Ord, Generic, Show)

instance ToJSON GetCurrentSyncedBlockResult where
  toJSON (GetCurrentSyncedBlockResult blockInfoM) =
    let chainPointObj = case blockInfoM of
          (At (BlockInfo sn bhh bn bt en)) ->
            [ "blockNo" .= bn
            , "blockTimestamp" .= bt
            , "blockHeaderHash" .= bhh
            , "slotNo" .= sn
            , "epochNo" .= en
            ]
          Origin -> []
     in object chainPointObj

instance FromJSON GetCurrentSyncedBlockResult where
  parseJSON (Object v) = do
    slotNoM <- v .:? "slotNo"
    bhhM <- v .:? "blockHeaderHash"
    bnM <- v .:? "blockNo"
    blockTimestampM <- v .:? "blockTimestamp"
    epochNoM <- v .:? "epochNo"
    let blockInfoM = withOriginFromMaybe $ BlockInfo <$> slotNoM <*> bhhM <*> bnM <*> blockTimestampM <*> epochNoM
    pure $ GetCurrentSyncedBlockResult blockInfoM
  parseJSON _ = mempty

data GetUtxosFromAddressParams = GetUtxosFromAddressParams
  { queryAddress :: !String
  -- ^ address to query for
  , queryCreatedAfterSlotNo :: !(Maybe Word64)
  -- ^ query upper bound slotNo interval, unspent before or at this slot
  , queryUnspentBeforeSlotNo :: !Word64
  -- ^ query lower bound slotNo interval, filter out UTxO that were created during or before that slo
  }
  deriving (Show, Eq)

instance FromJSON GetUtxosFromAddressParams where
  parseJSON (Object v) =
    GetUtxosFromAddressParams
      <$> (v .: "address")
      <*> (v .:? "createdAfterSlotNo")
      <*> (v .: "unspentBeforeSlotNo")
  parseJSON _ = mempty

instance ToJSON GetUtxosFromAddressParams where
  toJSON q =
    object $
      catMaybes
        [ Just ("address" .= queryAddress q)
        , ("createdAfterSlotNo" .=) <$> queryCreatedAfterSlotNo q
        , Just $ "unspentBeforeSlotNo" .= queryUnspentBeforeSlotNo q
        ]

newtype GetUtxosFromAddressResult = GetUtxosFromAddressResult
  {unAddressUtxosResult :: [AddressUtxoResult]}
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data SpentInfoResult
  = SpentInfoResult
      !C.SlotNo
      !C.TxId
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SpentInfoResult where
  toJSON (SpentInfoResult sn txid) = object ["slotNo" .= sn, "txId" .= txid]

instance FromJSON SpentInfoResult where
  parseJSON (Object v) = SpentInfoResult <$> v .: "slotNo" <*> v .: "txId"
  parseJSON _ = mempty

data AddressUtxoResult
  = AddressUtxoResult
      !C.SlotNo
      !(C.Hash C.BlockHeader)
      !C.BlockNo
      !TxIndexInBlock
      !C.TxIn
      !C.AddressAny
      !(Maybe (C.Hash C.ScriptData))
      !(Maybe C.ScriptData)
      !(Maybe SpentInfoResult)
      ![UtxoTxInput] -- List of inputs that were used in the transaction that created this UTxO.
  deriving (Eq, Ord, Show, Generic)

instance ToJSON AddressUtxoResult where
  toJSON (AddressUtxoResult slotNo bhh bn txIndexInBlock txIn addr dath dat spentBy txInputs) =
    let C.TxIn txId txIx = txIn
     in object
          [ "slotNo" .= slotNo
          , "blockHeaderHash" .= bhh
          , "blockNo" .= bn
          , "txIndexInBlock" .= txIndexInBlock
          , "txId" .= txId
          , "txIx" .= txIx
          , "address" .= addr
          , "datumHash" .= dath
          , "datum" .= dat
          , "spentBy" .= spentBy
          , "txInputs" .= txInputs
          ]

instance FromJSON AddressUtxoResult where
  parseJSON (Object v) = do
    AddressUtxoResult
      <$> v .: "slotNo"
      <*> v .: "blockHeaderHash"
      <*> v .: "blockNo"
      <*> v .: "txIndexInBlock"
      <*> (C.TxIn <$> v .: "txId" <*> v .: "txIx")
      <*> v .: "address"
      <*> v .: "datumHash"
      <*> v .: "datum"
      <*> v .: "spentBy"
      <*> v .: "txInputs"
  parseJSON _ = mempty

newtype UtxoTxInput = UtxoTxInput C.TxIn
  deriving (Eq, Ord, Show, Generic)

instance ToJSON UtxoTxInput where
  toJSON (UtxoTxInput (C.TxIn txId txIx)) =
    object
      [ "txId" .= txId
      , "txIx" .= txIx
      ]

instance FromJSON UtxoTxInput where
  parseJSON (Object v) = do
    txIn <-
      C.TxIn
        <$> v .: "txId"
        <*> v .: "txIx"
    pure $ UtxoTxInput txIn
  parseJSON _ = mempty

data GetBurnTokenEventsParams = GetBurnTokenEventsParams
  { policyId :: !C.PolicyId
  , assetName :: !(Maybe C.AssetName)
  , beforeSlotNo :: !(Maybe Word64)
  , afterTx :: !(Maybe C.TxId)
  }
  deriving (Eq, Show)

instance FromJSON GetBurnTokenEventsParams where
  parseJSON (Object v) =
    GetBurnTokenEventsParams
      <$> (v .: "policyId")
      <*> (v .:? "assetName")
      <*> (v .:? "slotNo")
      <*> (v .:? "afterTx")
  parseJSON _ = mempty

instance ToJSON GetBurnTokenEventsParams where
  toJSON q =
    object $
      catMaybes
        [ Just ("policyId" .= policyId q)
        , ("assetName" .=) <$> assetName q
        , ("slotNo" .=) <$> beforeSlotNo q
        , ("afterTx" .=) <$> afterTx q
        ]

newtype GetBurnTokenEventsResult
  = GetBurnTokenEventsResult [AssetIdTxResult]
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data AssetIdTxResult
  = AssetIdTxResult
      !C.SlotNo
      !(C.Hash C.BlockHeader)
      !C.BlockNo
      !C.TxId
      !(Maybe (C.Hash C.ScriptData))
      !(Maybe C.ScriptData)
      !C.Quantity
  deriving (Eq, Ord, Show, Generic)

instance ToJSON AssetIdTxResult where
  toJSON (AssetIdTxResult slotNo bhh bn txId redh red qty) =
    object
      [ "slotNo" .= slotNo
      , "blockHeaderHash" .= bhh
      , "blockNo" .= bn
      , "txId" .= txId
      , "redeemerHash" .= redh
      , "redeemer" .= red
      , "burnAmount" .= qty
      ]

instance FromJSON AssetIdTxResult where
  parseJSON (Object v) = do
    AssetIdTxResult
      <$> v .: "slotNo"
      <*> v .: "blockHeaderHash"
      <*> v .: "blockNo"
      <*> v .: "txId"
      <*> v .: "redeemerHash"
      <*> v .: "redeemer"
      <*> v .: "burnAmount"
  parseJSON _ = mempty

newtype GetEpochStakePoolDelegationResult
  = GetEpochStakePoolDelegationResult [EpochState.EpochSDDRow]
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype GetEpochActiveStakePoolDelegationResult
  = GetEpochActiveStakePoolDelegationResult [EpochState.EpochSDDRow]
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype GetEpochNonceResult
  = GetEpochNonceResult (Maybe EpochState.EpochNonceRow)
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
