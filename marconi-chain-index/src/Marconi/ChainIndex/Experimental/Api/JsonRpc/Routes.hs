{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.ChainIndex.Experimental.Api.JsonRpc.Routes (JsonRpcAPI) where

import Marconi.ChainIndex.Experimental.Api.JsonRpc.Endpoint.Echo (RpcEchoMethod)
import Marconi.ChainIndex.Experimental.Api.JsonRpc.Endpoint.EpochState (
  RpcEpochActiveStakePoolDelegationMethod,
  RpcEpochNonceMethod,
 )
import Marconi.ChainIndex.Experimental.Api.JsonRpc.Endpoint.MintBurnToken (
  RpcGetBurnTokenEventsMethod,
 )
import Marconi.ChainIndex.Experimental.Api.JsonRpc.Endpoint.TargetAddresses (
  RpcTargetAddressesMethod,
 )
import Network.JsonRpc.Types (RawJsonRpc)
import Servant ((:<|>), (:>))

-- | JSON-RPC API endpoint
type JsonRpcAPI = "json-rpc" :> RawJsonRpc RpcAPI

-- | JSON-RPC methods
type RpcAPI =
  RpcEchoMethod
    :<|> RpcTargetAddressesMethod
    :<|> RpcEpochActiveStakePoolDelegationMethod
    :<|> RpcEpochNonceMethod
    :<|> RpcGetBurnTokenEventsMethod
