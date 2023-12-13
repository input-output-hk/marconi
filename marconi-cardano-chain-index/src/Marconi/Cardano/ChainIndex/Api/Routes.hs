{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

-- | Defines REST and JSON-RPC routes
module Marconi.Cardano.ChainIndex.Api.Routes (
  module Marconi.Cardano.ChainIndex.Api.Routes,
) where

import Marconi.Cardano.ChainIndex.Api.JsonRpc.Routes (JsonRpcAPI)
import Marconi.Cardano.ChainIndex.Api.Rest.Routes (RestAPI)
import Marconi.Cardano.Core.Orphans ()
import Servant.API ((:<|>))

-- | marconi-cardano-chain-index APIs
type API = JsonRpcAPI :<|> RestAPI
