{-# LANGUAGE DataKinds #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.TargetAddresses where

import Data.Text (Text)
import qualified Marconi.ChainIndex.Api.JsonRpc.Endpoint.TargetAddresses as ChainIndex.TargetAddresses
import Marconi.Core.JsonRpc (ReaderHandler)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig, mapChainIndexExceptT)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr, UnusedRequestParams)

{- METHOD -}
type RpcTargetAddressesMethod =
  JsonRpc
    "getTargetAddresses"
    UnusedRequestParams
    String
    [Text]

{- HANDLER -}
getTargetAddressesQueryHandler
  :: UnusedRequestParams
  -- ^ Will be an empty string, empty object, or null, as we are ignoring this param, and returning everything
  -> ReaderHandler SidechainHttpServerConfig (Either (JsonRpcErr String) [Text])
getTargetAddressesQueryHandler = mapChainIndexExceptT . ChainIndex.TargetAddresses.getTargetAddressesQueryHandler
