module Marconi.Cardano.ChainIndex.Api.HttpServer where

import Control.Lens ((^.))
import Control.Monad.Reader (ReaderT, ask, lift)
import Data.Proxy (Proxy (Proxy))
import Marconi.Cardano.ChainIndex.Api.JsonRpc.Server (jsonRpcServer)
import Marconi.Cardano.ChainIndex.Api.Rest.Server (restApiServer)
import Marconi.Cardano.ChainIndex.Api.Routes (
  API,
 )
import Marconi.Cardano.ChainIndex.Api.Types (
  HttpServerConfig,
  configPort,
  configTrace,
 )
import Marconi.Core.JsonRpc (
  ReaderHandler,
  ReaderServer,
  catchHttpHandlerExceptions,
  hoistReaderHandler,
  mkHttpRequestTracer,
 )
import Network.JsonRpc.Server.Types ()
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Servant.API ((:<|>) ((:<|>)))
import Servant.Server (
  Application,
  Handler,
  hoistServer,
  serve,
 )

-- | Bootstraps the HTTP server
runHttpServer :: ReaderT HttpServerConfig IO ()
runHttpServer = do
  config <- ask
  let settings = setPort (config ^. configPort) defaultSettings
  requestTracer <- mkHttpRequestTracer (config ^. configTrace)
  lift $ runSettings settings $ requestTracer $ marconiApp config

marconiApp :: HttpServerConfig -> Application
marconiApp config = do
  let trace = config ^. configTrace
      wrapHandler :: ReaderHandler HttpServerConfig a -> Handler a
      wrapHandler = catchHttpHandlerExceptions trace . hoistReaderHandler config
  serve (Proxy @API) $
    hoistServer (Proxy @API) wrapHandler httpRpcServer

httpRpcServer :: ReaderServer HttpServerConfig API
httpRpcServer = jsonRpcServer :<|> restApiServer
