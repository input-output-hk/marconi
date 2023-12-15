{-# LANGUAGE DataKinds #-}

module Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.TargetAddresses (
  RpcTargetAddressesMethod,
  getTargetAddressesQueryHandler,
) where

import Cardano.Api (SerialiseAddress (serialiseAddress))
import Control.Lens (view)
import Data.Text (Text)
import Marconi.Cardano.ChainIndex.Api.Types (HttpServerConfig, configTrackedAddresses)
import Marconi.Core.JsonRpc (ReaderHandler)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr, UnusedRequestParams)

------------------
-- Method types --
------------------

type RpcTargetAddressesMethod =
  JsonRpc
    "getTargetAddresses"
    UnusedRequestParams
    String
    [Text]

--------------
-- Handlers --
--------------

-- | Return the list of TargetAddresses in Bech32 representation.
getTargetAddressesQueryHandler
  :: UnusedRequestParams
  -- ^ Will be an empty string, empty object, or null, as we are ignoring this param, and returning everything
  -> ReaderHandler HttpServerConfig (Either (JsonRpcErr String) [Text])
getTargetAddressesQueryHandler _ = do
  addresses <- view configTrackedAddresses
  pure $ Right $ serialiseAddress <$> addresses
