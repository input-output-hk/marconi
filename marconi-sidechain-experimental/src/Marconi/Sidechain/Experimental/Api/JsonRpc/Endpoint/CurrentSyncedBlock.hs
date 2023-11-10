{-# LANGUAGE DataKinds #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.CurrentSyncedBlock where

import Network.JsonRpc.Types (JsonRpc, RawJsonRpc, UnusedRequestParams)

-- | TODO:
type RpcCurrentSyncedBlockMethod = ()

--  JsonRpc
--    "getCurrentSyncedBlock"
--    UnusedRequestParams
--    String
--    GetCurrentSyncedBlockResult
