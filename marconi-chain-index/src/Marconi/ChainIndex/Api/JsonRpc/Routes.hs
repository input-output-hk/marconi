{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.ChainIndex.Api.JsonRpc.Routes (JsonRpcAPI) where

import Marconi.ChainIndex.Api.JsonRpc.Endpoint.CurrentSyncedBlock (
  RpcGetCurrentSyncedBlock,
 )
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Echo (RpcEchoMethod)
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.EpochState (
  RpcEpochActiveStakePoolDelegationMethod,
  RpcEpochNonceMethod,
 )
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.MintBurnToken (
  RpcGetBurnTokenEventsMethod,
 )
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.TargetAddresses (
  RpcTargetAddressesMethod,
 )
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo (RpcGetUtxosFromAddressMethod)
import Network.JsonRpc.Types (RawJsonRpc)
import Servant ((:<|>), (:>))

-- | JSON-RPC API endpoint
type JsonRpcAPI = "json-rpc" :> RawJsonRpc RpcAPI

-- | JSON-RPC methods
type RpcAPI =
  RpcEchoMethod
    -- :<|> RpcTargetAddressesMethod
    -- :<|> RpcEpochActiveStakePoolDelegationMethod
    -- :<|> RpcEpochNonceMethod
    -- :<|> RpcGetBurnTokenEventsMethod
    -- :<|> RpcGetCurrentSyncedBlock
    :<|> RpcGetUtxosFromAddressMethod
