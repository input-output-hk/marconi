{-# LANGUAGE DataKinds #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.TargetAddresses where

import Data.Text (Text)
import qualified Marconi.ChainIndex.Api.JsonRpc.Endpoint.TargetAddresses as ChainIndex.TargetAddresses
import Marconi.Core.JsonRpc (ReaderHandler)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr, UnusedRequestParams)

-- | TODO: PLT-8076 taken from sidechain and need to adapt
type RpcTargetAddressesMethod =
  JsonRpc
    "getTargetAddresses"
    UnusedRequestParams
    String
    [Text]

-- | TODO: PLT-8076
getTargetAddressesQueryHandler
  :: UnusedRequestParams
  -- ^ Will be an empty string, empty object, or null, as we are ignoring this param, and returning everything
  -> ReaderHandler SidechainHttpServerConfig (Either (JsonRpcErr String) [Text])
getTargetAddressesQueryHandler = ChainIndex.TargetAddresses.getTargetAddressesQueryHandler
