{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

-- | Defines REST and JSON-RPC routes
module Marconi.ChainIndex.Experimental.Api.Routes (
  module Marconi.ChainIndex.Experimental.Api.Routes,
) where

import Marconi.ChainIndex.Experimental.Api.JsonRpc.Routes (JsonRpcAPI)
import Marconi.ChainIndex.Experimental.Api.Rest.Routes (RestAPI)
import Marconi.ChainIndex.Orphans ()
import Servant.API ((:<|>))

-- | marconi-chain-indexer APIs
type API = JsonRpcAPI :<|> RestAPI
