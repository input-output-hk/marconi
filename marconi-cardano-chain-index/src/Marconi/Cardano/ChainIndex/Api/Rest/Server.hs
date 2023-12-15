module Marconi.Cardano.ChainIndex.Api.Rest.Server (restApiServer) where

import Marconi.Cardano.ChainIndex.Api.Rest.Endpoint.Metrics (getMetricsHandler)
import Marconi.Cardano.ChainIndex.Api.Rest.Endpoint.Params (getParamsHandler)
import Marconi.Cardano.ChainIndex.Api.Rest.Endpoint.TargetAddresses (getTargetAddressesHandler)
import Marconi.Cardano.ChainIndex.Api.Rest.Endpoint.Time (getTimeHandler)
import Marconi.Cardano.ChainIndex.Api.Rest.Routes (RestAPI)
import Marconi.Cardano.ChainIndex.Api.Types (HttpServerConfig)
import Marconi.Core.JsonRpc (ReaderServer)
import Servant ((:<|>) ((:<|>)))

-- | Handlers for the REST API
restApiServer :: ReaderServer HttpServerConfig RestAPI
restApiServer =
  getTimeHandler
    :<|> getParamsHandler
    :<|> getTargetAddressesHandler
    :<|> getMetricsHandler
