module Marconi.Sidechain.Experimental.Api.JsonRpc.Server where

import Marconi.Core.JsonRpc (ReaderServer)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig)

-- | Handlers for the JSON-RPC
jsonRpcServer :: ReaderServer SidechainHttpServerConfig JsonRpcAPI
jsonRpcServer = undefined

-- Chainindex handlers
--  echo
--    :<|> getTargetAddressesQueryHandler
--    :<|> getEpochStakePoolDelegationHandler
--    :<|> getEpochNonceHandler
--    :<|> getBurnTokenEventsHandler
--    :<|> getCurrentSyncedBlockHandler
