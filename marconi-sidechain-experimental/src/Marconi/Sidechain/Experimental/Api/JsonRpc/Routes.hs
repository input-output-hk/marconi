{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Routes where

import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.BurnTokenEvent (
  RpcGetBurnTokenEventsMethod,
 )
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.CurrentSyncedBlock (
  RpcCurrentSyncedBlockMethod,
 )
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.Echo (RpcEchoMethod)
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.EpochActiveStakePoolDelegation (
  RpcEpochActiveStakePoolDelegationMethod,
 )
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.EpochNonce (RpcEpochNonceMethod)
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo (
  RpcPastAddressUtxoMethod,
 )
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.TargetAddresses (
  RpcTargetAddressesMethod,
 )
import Network.JsonRpc.Types (RawJsonRpc)
import Servant.API ((:<|>), (:>))

type JsonRpcAPI = "json-rpc" :> RawJsonRpc RpcAPI

-- | JSON-RPC methods
type RpcAPI =
  RpcEchoMethod
    -- TODO:
    -- :<|> RpcTargetAddressesMethod
    -- :<|> RpcCurrentSyncedBlockMethod
    -- :<|> RpcPastAddressUtxoMethod
    -- :<|> RpcGetBurnTokenEventsMethod
    -- :<|> RpcEpochActiveStakePoolDelegationMethod
    :<|> RpcEpochNonceMethod
