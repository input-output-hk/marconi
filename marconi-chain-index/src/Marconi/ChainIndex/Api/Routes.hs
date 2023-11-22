{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

-- | Defines REST and JSON-RPC routes
module Marconi.ChainIndex.Api.Routes (
  module Marconi.ChainIndex.Api.Routes,
) where

import Marconi.Cardano.Core.Orphans ()
import Marconi.ChainIndex.Api.JsonRpc.Routes (JsonRpcAPI)
import Marconi.ChainIndex.Api.Rest.Routes (RestAPI)
import Servant.API ((:<|>))

-- | marconi-chain-indexer APIs
type API = JsonRpcAPI :<|> RestAPI
