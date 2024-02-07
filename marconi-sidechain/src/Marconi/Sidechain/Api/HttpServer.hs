{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Run the HTTP server for the JSON-RPC API.
module Marconi.Sidechain.Api.HttpServer where

import Control.Lens ((^.))
import Control.Monad.Reader (ReaderT, ask, lift)
import Data.Proxy (Proxy (Proxy))
import Marconi.Core.JsonRpc (
  ReaderHandler,
  ReaderServer,
  catchHttpHandlerExceptions,
  hoistReaderHandler,
  mkHttpRequestTracer,
 )
import Marconi.Sidechain.Api.JsonRpc.Routes (JsonRpcAPI)
import Marconi.Sidechain.Api.JsonRpc.Server (jsonRpcServer)
import Marconi.Sidechain.Api.Rest.Routes (RestAPI)
import Marconi.Sidechain.Api.Rest.Server (restApiServer)
import Marconi.Sidechain.Api.Types (
  SidechainHttpServerConfig,
  chainIndexHttpServerConfig,
  configPort,
  configTrace,
 )
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Servant.API ((:<|>) ((:<|>)))
import Servant.Server (
  Application,
  Handler,
  hoistServer,
  serve,
 )

-- | Bootstraps the HTTP server.
runHttpServer :: ReaderT SidechainHttpServerConfig IO ()
runHttpServer = do
  config <- ask
  let settings = setPort (config ^. chainIndexHttpServerConfig . configPort) defaultSettings
  requestTracer <- mkHttpRequestTracer (config ^. chainIndexHttpServerConfig . configTrace)
  lift $ runSettings settings $ requestTracer $ sidechainApp config

sidechainApp :: SidechainHttpServerConfig -> Application
sidechainApp config = do
  let trace = config ^. chainIndexHttpServerConfig . configTrace
      wrapHandler :: ReaderHandler SidechainHttpServerConfig a -> Handler a
      wrapHandler = catchHttpHandlerExceptions trace . hoistReaderHandler config
  serve (Proxy @API) $
    hoistServer (Proxy @API) wrapHandler httpServer

httpServer :: ReaderServer SidechainHttpServerConfig API
httpServer = jsonRpcServer :<|> restApiServer

type API = JsonRpcAPI :<|> RestAPI
