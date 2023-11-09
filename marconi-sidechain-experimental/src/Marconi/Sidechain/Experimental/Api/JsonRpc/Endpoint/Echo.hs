{-# LANGUAGE DataKinds #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.Echo where

import Network.JsonRpc.Types (JsonRpc)

-- | TODO: PLT-8076 taken from sidechain. possibly need to adapt.
type RpcEchoMethod = JsonRpc "echo" String String String
