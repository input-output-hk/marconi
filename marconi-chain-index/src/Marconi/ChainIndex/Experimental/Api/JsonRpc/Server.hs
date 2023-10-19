module Marconi.ChainIndex.Experimental.Api.JsonRpc.Server (jsonRpcServer) where

import Marconi.ChainIndex.Experimental.Api.JsonRpc.Endpoint.Echo (echo)
import Marconi.ChainIndex.Experimental.Api.JsonRpc.Endpoint.EpochState (
  getEpochNonceHandler,
  getEpochStakePoolDelegationHandler,
 )
import Marconi.ChainIndex.Experimental.Api.JsonRpc.Endpoint.MintBurnToken (
  getBurnTokenEventsHandler,
 )
import Marconi.ChainIndex.Experimental.Api.JsonRpc.Endpoint.TargetAddresses (
  getTargetAddressesQueryHandler,
 )
import Marconi.ChainIndex.Experimental.Api.JsonRpc.Routes (JsonRpcAPI)
import Marconi.ChainIndex.Experimental.Api.Types (HttpServerConfig)
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
