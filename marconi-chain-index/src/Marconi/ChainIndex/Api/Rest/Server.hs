module Marconi.ChainIndex.Api.Rest.Server (restApiServer) where

import Marconi.ChainIndex.Api.Rest.Endpoint.Metrics (getMetricsHandler)
import Marconi.ChainIndex.Api.Rest.Endpoint.Params (getParamsHandler)
import Marconi.ChainIndex.Api.Rest.Endpoint.TargetAddresses (getTargetAddressesHandler)
import Marconi.ChainIndex.Api.Rest.Endpoint.Time (getTimeHandler)
import Marconi.ChainIndex.Api.Rest.Routes (RestAPI)
import Marconi.ChainIndex.Api.Types (HttpServerConfig)
import Marconi.Core.JsonRpc (ReaderServer)
import Servant ((:<|>) ((:<|>)))

-- | Handlers for the REST API
restApiServer :: ReaderServer HttpServerConfig RestAPI
restApiServer =
  getTimeHandler
    :<|> getParamsHandler
    :<|> getTargetAddressesHandler
    :<|> getMetricsHandler
