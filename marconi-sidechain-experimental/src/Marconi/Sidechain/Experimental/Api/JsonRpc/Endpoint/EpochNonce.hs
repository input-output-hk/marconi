{-# LANGUAGE DataKinds #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.EpochNonce where

import Data.Word (Word64)
import Network.JsonRpc.Types (JsonRpc, RawJsonRpc, UnusedRequestParams)

-- | TODO: PLT-8076
type RpcEpochNonceMethod =
  JsonRpc
    "getNonceByEpoch"
    Word64
    String
    GetEpochNonceResult
