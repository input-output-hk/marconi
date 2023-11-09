{-# LANGUAGE TypeApplications #-}

-- | Boilerplate to run the HTTP server as in marconi-chain-index.
module Marconi.Sidechain.Experimental.Api.HttpServer where

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
import Marconi.Sidechain.Experimental.Api.Routes (API)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig, configPort, configTrace)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Servant.Server (
  Application,
  Handler,
  hoistServer,
  serve,
 )

-- | Bootstraps the HTTP server
runHttpServer :: ReaderT SidechainHttpServerConfig IO ()
runHttpServer = do
  config <- ask
  let settings = setPort (config ^. configPort) defaultSettings
  requestTracer <- mkHttpRequestTracer (config ^. configTrace)
  lift $ runSettings settings $ requestTracer $ sidechainApp config

sidechainApp :: SidechainHttpServerConfig -> Application
sidechainApp config = do
  let trace = config ^. configTrace
      wrapHandler :: ReaderHandler SidechainHttpServerConfig a -> Handler a
      wrapHandler = catchHttpHandlerExceptions trace . hoistReaderHandler config
  serve (Proxy @API) $
    hoistServer (Proxy @API) wrapHandler httpRpcServer

-- TODO: PLT-8076 jsonRpcServer :<|> restApiServer
httpRpcServer :: ReaderServer SidechainHttpServerConfig API
httpRpcServer = undefined
