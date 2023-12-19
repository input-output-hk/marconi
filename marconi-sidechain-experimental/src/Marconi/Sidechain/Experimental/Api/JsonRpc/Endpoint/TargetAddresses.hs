{-# LANGUAGE DataKinds #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.TargetAddresses where

import Data.Text (Text)
import Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.TargetAddresses qualified as ChainIndex.TargetAddresses
import Marconi.Core.JsonRpc (ReaderHandler)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig, withChainIndexHandler)
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
getTargetAddressesQueryHandler = withChainIndexHandler . ChainIndex.TargetAddresses.getTargetAddressesQueryHandler
