{-# LANGUAGE DataKinds #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.TargetAddresses where

import Data.Text (Text)
import Network.JsonRpc.Types (JsonRpc, UnusedRequestParams)

-- | TODO: PLT-8076 taken from sidechain and need to adapt
type RpcTargetAddressesMethod =
  JsonRpc
    "getTargetAddresses"
    UnusedRequestParams
    String
    [Text]
