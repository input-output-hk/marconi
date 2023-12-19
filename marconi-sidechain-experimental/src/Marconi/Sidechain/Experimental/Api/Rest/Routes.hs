module Marconi.Sidechain.Experimental.Api.Rest.Routes (RestAPI) where

import Marconi.Cardano.ChainIndex.Api.Rest.Endpoint.Metrics (GetMetrics)

-- | Routes for the REST API
type RestAPI = GetMetrics
