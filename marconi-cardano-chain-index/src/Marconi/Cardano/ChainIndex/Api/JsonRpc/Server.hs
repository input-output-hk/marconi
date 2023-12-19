module Marconi.Cardano.ChainIndex.Api.JsonRpc.Server (jsonRpcServer) where

import Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.CurrentSyncedBlock (
  getCurrentSyncedBlockHandler,
 )
import Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.Echo (echo)
import Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.EpochState (
  getEpochNonceHandler,
  getEpochStakePoolDelegationHandler,
 )
import Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.MintBurnToken (
  getBurnTokenEventsHandler,
 )
import Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.TargetAddresses (
  getTargetAddressesQueryHandler,
 )
import Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.Utxo (getUtxosFromAddressQueryHandler)
import Marconi.Cardano.ChainIndex.Api.JsonRpc.Routes (JsonRpcAPI)
import Marconi.Cardano.ChainIndex.Api.Types (HttpServerConfig)
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
    :<|> getCurrentSyncedBlockHandler
    :<|> getUtxosFromAddressQueryHandler
