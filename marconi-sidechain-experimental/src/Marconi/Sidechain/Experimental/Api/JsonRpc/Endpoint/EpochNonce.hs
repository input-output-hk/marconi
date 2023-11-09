{-# LANGUAGE DataKinds #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.EpochNonce where

import Data.Word (Word64)
import qualified Marconi.ChainIndex.Api.JsonRpc.Endpoint.EpochState as ChainIndex.EpochState
import Marconi.Core.JsonRpc (ReaderHandler)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr)

{- METHOD -}

-- | TODO: PLT-8076
type RpcEpochNonceMethod =
  JsonRpc
    "getNonceByEpoch"
    Word64
    String
    GetEpochNonceResult

{- TYPES -}

{- | TODO: PLT-8076 change this to one that supports the same json shape as the
sidechain package
-}
type GetEpochNonceResult = ChainIndex.EpochState.EpochNonceResult

{- HANDLER -}
--

-- | TODO: PLT-8076 will need dimap handler from rpc package. Handler for retrieving stake pool delegation per epoch
getEpochNonceHandler
  :: Word64
  -- ^ EpochNo
  -> ReaderHandler SidechainHttpServerConfig (Either (JsonRpcErr String) GetEpochNonceResult)
getEpochNonceHandler = ChainIndex.EpochState.getEpochNonceHandler
