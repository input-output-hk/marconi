module Marconi.Sidechain.Api.JsonRpc.Server where

import Marconi.Core.JsonRpc (ReaderServer)
import Marconi.Sidechain.Api.JsonRpc.Endpoint.BurnTokenEvent (
  getBurnTokenEventsHandler,
 )
import Marconi.Sidechain.Api.JsonRpc.Endpoint.CurrentSyncedBlock (
  getCurrentSyncedBlockHandler,
 )
import Marconi.Sidechain.Api.JsonRpc.Endpoint.Echo (echo)
import Marconi.Sidechain.Api.JsonRpc.Endpoint.EpochActiveStakePoolDelegation (
  getEpochActiveStakePoolDelegationHandler,
 )
import Marconi.Sidechain.Api.JsonRpc.Endpoint.EpochNonce (getEpochNonceHandler)
import Marconi.Sidechain.Api.JsonRpc.Endpoint.PastAddressUtxo (
  getPastAddressUtxoHandler,
 )
import Marconi.Sidechain.Api.JsonRpc.Endpoint.TargetAddresses (
  getTargetAddressesQueryHandler,
 )
import Marconi.Sidechain.Api.JsonRpc.Routes (JsonRpcAPI)
import Marconi.Sidechain.Api.Types (SidechainHttpServerConfig)
import Servant.API ((:<|>) ((:<|>)))

-- | Handlers for the JSON-RPC
jsonRpcServer :: ReaderServer SidechainHttpServerConfig JsonRpcAPI
jsonRpcServer =
  echo
    :<|> getTargetAddressesQueryHandler
    :<|> getCurrentSyncedBlockHandler
    :<|> getPastAddressUtxoHandler
    :<|> getBurnTokenEventsHandler
    :<|> getEpochActiveStakePoolDelegationHandler
    :<|> getEpochNonceHandler
