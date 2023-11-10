{-# LANGUAGE DataKinds #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.EpochActiveStakePoolDelegation where

import Data.Word (Word64)
import Network.JsonRpc.Types (JsonRpc, RawJsonRpc, UnusedRequestParams)

-- | TODO: PLT-8076
type RpcEpochActiveStakePoolDelegationMethod = ()

--  JsonRpc
--    "getActiveStakePoolDelegationByEpoch"
--    Word64
--    String
--    GetEpochActiveStakePoolDelegationResult
