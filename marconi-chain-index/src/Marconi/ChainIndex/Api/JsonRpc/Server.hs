module Marconi.ChainIndex.Api.JsonRpc.Server (jsonRpcServer) where

import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Echo (echo)
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.EpochState (
  getEpochNonceHandler,
  getEpochStakePoolDelegationHandler,
 )
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.MintBurnToken (
  getBurnTokenEventsHandler,
 )
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.TargetAddresses (
  getTargetAddressesQueryHandler,
 )
import Marconi.ChainIndex.Api.JsonRpc.Routes (JsonRpcAPI)
import Marconi.ChainIndex.Api.Types (HttpServerConfig)
import Marconi.Core.JsonRpc (ReaderServer)
import Servant.API ((:<|>) ((:<|>)))

-- | Handlers for the JSON-RPC
jsonRpcServer :: ReaderServer HttpServerConfig JsonRpcAPI
jsonRpcServer =
  echo
    :<|> getTargetAddressesQueryHandler
    :<|> getEpochStakePoolDelegationHandler
    :<|> getEpochNonceHandler
    :<|> getBurnTokenEventsHandler
