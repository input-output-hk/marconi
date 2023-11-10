{-# LANGUAGE DataKinds #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.CurrentSyncedBlock where

import Network.JsonRpc.Types (JsonRpc, RawJsonRpc, UnusedRequestParams)

-- | TODO: PLT-8076
type RpcCurrentSyncedBlockMethod = ()

--  JsonRpc
--    "getCurrentSyncedBlock"
--    UnusedRequestParams
--    String
--    GetCurrentSyncedBlockResult
