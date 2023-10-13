{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

-- | Defines REST and JSON-RPC routes
module Marconi.ChainIndex.Experimental.Api.Routes (
  module Marconi.ChainIndex.Experimental.Api.Routes,
) where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
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
