{-# LANGUAGE DataKinds #-}

module Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.Echo (
  RpcEchoMethod,
  echo,
) where

import Marconi.Cardano.ChainIndex.Api.Types (HttpServerConfig)
import Marconi.Core.JsonRpc (ReaderHandler)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr)

------------------
-- Method types --
------------------

type RpcEchoMethod = JsonRpc "echo" String String String

--------------
-- Handlers --
--------------

-- | Echo a message back as a JSON-RPC response. Used for testing the server.
echo
  :: String
  -> ReaderHandler HttpServerConfig (Either (JsonRpcErr String) String)
echo = return . Right
