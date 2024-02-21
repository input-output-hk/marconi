{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.Sidechain.Api.JsonRpc.Routes where

import Marconi.Sidechain.Api.JsonRpc.Endpoint.BurnTokenEvent (
  RpcGetBurnTokenEventsMethod,
 )
import Marconi.Sidechain.Api.JsonRpc.Endpoint.CurrentSyncedBlock (
  RpcCurrentSyncedBlockMethod,
 )
import Marconi.Sidechain.Api.JsonRpc.Endpoint.Echo (RpcEchoMethod)
import Marconi.Sidechain.Api.JsonRpc.Endpoint.EpochActiveStakePoolDelegation (
  RpcEpochActiveStakePoolDelegationMethod,
 )
import Marconi.Sidechain.Api.JsonRpc.Endpoint.EpochNonce (RpcEpochNonceMethod)
import Marconi.Sidechain.Api.JsonRpc.Endpoint.PastAddressUtxo (
  RpcPastAddressUtxoMethod,
 )
import Marconi.Sidechain.Api.JsonRpc.Endpoint.TargetAddresses (
  RpcTargetAddressesMethod,
 )
import Network.JsonRpc.Types (RawJsonRpc)
import Servant.API ((:<|>), (:>))

type JsonRpcAPI = "json-rpc" :> RawJsonRpc RpcAPI

-- | JSON-RPC methods
type RpcAPI =
  RpcEchoMethod
    :<|> RpcTargetAddressesMethod
    :<|> RpcCurrentSyncedBlockMethod
    :<|> RpcPastAddressUtxoMethod
    :<|> RpcGetBurnTokenEventsMethod
    :<|> RpcEpochActiveStakePoolDelegationMethod
    :<|> RpcEpochNonceMethod
