{-# LANGUAGE DataKinds #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo where

import Network.JsonRpc.Types (JsonRpc, RawJsonRpc, UnusedRequestParams)

-- | TODO:
type RpcPastAddressUtxoMethod = ()

--  JsonRpc
--    "getUtxosFromAddress"
--    GetUtxosFromAddressParams
--    String
--    GetUtxosFromAddressResult
