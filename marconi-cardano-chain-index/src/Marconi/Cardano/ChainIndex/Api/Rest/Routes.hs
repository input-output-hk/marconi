{-# LANGUAGE TypeOperators #-}

module Marconi.Cardano.ChainIndex.Api.Rest.Routes (RestAPI) where

import Marconi.Cardano.ChainIndex.Api.Rest.Endpoint.Metrics (GetMetrics)
import Marconi.Cardano.ChainIndex.Api.Rest.Endpoint.Params (GetParams)
import Marconi.Cardano.ChainIndex.Api.Rest.Endpoint.TargetAddresses (GetTargetAddresses)
import Marconi.Cardano.ChainIndex.Api.Rest.Endpoint.Time (GetTime)
import Servant ((:<|>))

-- | Routes for the REST API
type RestAPI =
  GetTime
    :<|> GetParams
    :<|> GetTargetAddresses
    :<|> GetMetrics
