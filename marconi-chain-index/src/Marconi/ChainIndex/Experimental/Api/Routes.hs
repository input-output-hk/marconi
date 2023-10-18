{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Defines REST and JSON-RPC routes
module Marconi.ChainIndex.Experimental.Api.Routes (
  module Marconi.ChainIndex.Experimental.Api.Routes,
) where

import Cardano.Api qualified as C
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Experimental.Indexers.EpochState qualified as EpochState
import Marconi.ChainIndex.Orphans ()
import Marconi.Core qualified as Core
import Network.JsonRpc.Types (JsonRpc, RawJsonRpc, UnusedRequestParams)
import Servant.API (Get, JSON, PlainText, (:<|>), (:>))

-- | marconi-chain-indexer APIs
type API = JsonRpcAPI :<|> RestAPI

----------------------------------------------
--  JSON-RPC types
--  methodName -> parameter(s) -> return-type
----------------------------------------------

-- | JSON-RPC API endpoint
type JsonRpcAPI = "json-rpc" :> RawJsonRpc RpcAPI

-- | JSON-RPC methods
type RpcAPI =
  RpcEchoMethod
    :<|> RpcTargetAddressesMethod
    :<|> RpcEpochActiveStakePoolDelegationMethod
    :<|> RpcEpochNonceMethod
    :<|> RpcGetBurnTokenEventsMethod

type RpcEchoMethod = JsonRpc "echo" String String String

type RpcTargetAddressesMethod =
  JsonRpc
    "getTargetAddresses"
    UnusedRequestParams
    String
    [Text]

type RpcEpochActiveStakePoolDelegationMethod =
  JsonRpc
    "getActiveStakePoolDelegationByEpoch"
    EpochState.ActiveSDDByEpochNoQuery
    String
    (Core.Result EpochState.ActiveSDDByEpochNoQuery)

type RpcEpochNonceMethod =
  JsonRpc
    "getNonceByEpoch"
    EpochState.NonceByEpochNoQuery
    String
    (Core.Result EpochState.NonceByEpochNoQuery)

type RpcGetBurnTokenEventsMethod =
  JsonRpc
    "getBurnTokenEvents"
    GetBurnTokenEventsParams
    String
    GetBurnTokenEventsResult

------------------------
-- REST API endpoints --
------------------------

type RestAPI =
  GetTime
    :<|> GetParams
    :<|> GetTargetAddresses
    :<|> GetMetrics

type GetTime = "time" :> Get '[PlainText] String

type GetParams = "params" :> Get '[JSON] Aeson.Value

type GetTargetAddresses = "addresses" :> Get '[JSON] [Text]

type GetMetrics = "metrics" :> Get '[PlainText] Text

----------------------------
-- Query and result types --
----------------------------

data GetBurnTokenEventsParams = GetBurnTokenEventsParams
  { policyId :: !C.PolicyId
  , assetName :: !(Maybe C.AssetName)
  , beforeSlotNo :: !(Maybe C.SlotNo)
  , afterTx :: !(Maybe C.TxId)
  }
  deriving (Eq, Show)

instance FromJSON GetBurnTokenEventsParams where
  parseJSON =
    let parseParams v =
          GetBurnTokenEventsParams
            <$> (v .: "policyId" <|> fail "The 'policyId' param value must be a valid minting policy hash.")
            <*> (v .:? "assetName")
            <*> (v .:? "createdBeforeSlotNo" <|> fail "The 'slotNo' param value must be a natural number.")
            <*> (v .:? "createdAfterTx" <|> fail "The 'afterTx' param value must be a valid transaction ID.")
     in Aeson.withObject "GetBurnTokenEventsParams" parseParams

instance ToJSON GetBurnTokenEventsParams where
  toJSON q =
    Aeson.object $
      catMaybes
        [ Just ("policyId" .= policyId q)
        , ("assetName" .=) <$> assetName q
        , ("createdBeforeSlotNo" .=) <$> beforeSlotNo q
        , ("createdAfterTx" .=) <$> afterTx q
        ]

newtype GetBurnTokenEventsResult
  = GetBurnTokenEventsResult [BurnTokenEventResult]
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The quantity represents a burn amount only, so this is always a positive number.
data BurnTokenEventResult
  = BurnTokenEventResult
      !C.SlotNo
      !(C.Hash C.BlockHeader)
      !C.BlockNo
      !C.TxId
      !(Maybe (C.Hash C.ScriptData))
      !(Maybe C.ScriptData)
      !C.AssetName
      !C.Quantity
      !Bool
  deriving (Eq, Ord, Show, Generic)

instance ToJSON BurnTokenEventResult where
  toJSON (BurnTokenEventResult slotNo bhh bn txId redh red an qty isStable) =
    Aeson.object
      [ "slotNo" .= slotNo
      , "blockHeaderHash" .= bhh
      , "blockNo" .= bn
      , "txId" .= txId
      , "redeemerHash" .= redh
      , "redeemer" .= red
      , "assetName" .= an
      , "burnAmount" .= qty
      , "isStable" .= isStable
      ]

instance FromJSON BurnTokenEventResult where
  parseJSON =
    let parseAssetIdTxResult v =
          BurnTokenEventResult
            <$> v
              .: "slotNo"
            <*> v
              .: "blockHeaderHash"
            <*> v
              .: "blockNo"
            <*> v
              .: "txId"
            <*> v
              .: "redeemerHash"
            <*> v
              .: "redeemer"
            <*> v
              .: "assetName"
            <*> v
              .: "burnAmount"
            <*> v
              .: "isStable"
     in Aeson.withObject "AssetIdTxResult" parseAssetIdTxResult
