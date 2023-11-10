{-# LANGUAGE DataKinds #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.BurnTokenEvent where

import Network.JsonRpc.Types (JsonRpc, RawJsonRpc, UnusedRequestParams)

-- | TODO: PLT-8076
type RpcGetBurnTokenEventsMethod = ()

--  JsonRpc
--    "getBurnTokenEvents"
--    GetBurnTokenEventsParams
--    String
--    GetBurnTokenEventsResult
