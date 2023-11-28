module Marconi.Sidechain.Experimental.Api.Rest.Server (restApiServer) where

import Marconi.Core.JsonRpc (ReaderServer)
import Marconi.Sidechain.Experimental.Api.Rest.Endpoint.Metrics (getMetricsHandler)
import Marconi.Sidechain.Experimental.Api.Rest.Routes (RestAPI)
import Marconi.Sidechain.Experimental.Api.Types (
  SidechainHttpServerConfig,
 )

-- | Handlers for the REST API
restApiServer :: ReaderServer SidechainHttpServerConfig RestAPI
restApiServer = getMetricsHandler
