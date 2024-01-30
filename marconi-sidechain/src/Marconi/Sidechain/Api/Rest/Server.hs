module Marconi.Sidechain.Api.Rest.Server (restApiServer) where

import Marconi.Core.JsonRpc (ReaderServer)
import Marconi.Sidechain.Api.Rest.Endpoint.Metrics (getMetricsHandler)
import Marconi.Sidechain.Api.Rest.Routes (RestAPI)
import Marconi.Sidechain.Api.Types (
  SidechainHttpServerConfig,
 )

-- | Handlers for the REST API
restApiServer :: ReaderServer SidechainHttpServerConfig RestAPI
restApiServer = getMetricsHandler
