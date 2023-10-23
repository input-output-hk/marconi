{-# LANGUAGE TypeOperators #-}

module Marconi.ChainIndex.Experimental.Api.Rest.Routes (RestAPI) where

import Marconi.ChainIndex.Experimental.Api.Rest.Endpoint.Metrics (GetMetrics)
import Marconi.ChainIndex.Experimental.Api.Rest.Endpoint.Params (GetParams)
import Marconi.ChainIndex.Experimental.Api.Rest.Endpoint.TargetAddresses (GetTargetAddresses)
import Marconi.ChainIndex.Experimental.Api.Rest.Endpoint.Time (GetTime)
import Servant ((:<|>))

-- | Routes for the REST API
type RestAPI =
  GetTime
    :<|> GetParams
    :<|> GetTargetAddresses
    :<|> GetMetrics
