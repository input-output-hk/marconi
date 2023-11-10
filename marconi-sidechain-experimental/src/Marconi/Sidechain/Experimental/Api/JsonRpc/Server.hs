module Marconi.Sidechain.Experimental.Api.JsonRpc.Server where

import Marconi.Core.JsonRpc (ReaderServer)
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.Echo (echo)
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.EpochNonce (getEpochNonceHandler)
import Marconi.Sidechain.Experimental.Api.JsonRpc.Routes (JsonRpcAPI)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig)
import Servant.API ((:<|>) ((:<|>)))

-- | Handlers for the JSON-RPC
jsonRpcServer :: ReaderServer SidechainHttpServerConfig JsonRpcAPI
jsonRpcServer = echo :<|> getEpochNonceHandler

-- TODO: PLT-8076
-- Chainindex handlers
--  echo
--    :<|> getTargetAddressesQueryHandler
--    :<|> getEpochStakePoolDelegationHandler
--    :<|> getEpochNonceHandler
--    :<|> getBurnTokenEventsHandler
--    :<|> getCurrentSyncedBlockHandler
