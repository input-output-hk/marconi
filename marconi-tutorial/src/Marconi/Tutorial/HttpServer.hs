{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.Tutorial.HttpServer where

import Data.Data (Proxy (Proxy))
import Network.JsonRpc.Server.Types ()
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr, RawJsonRpc)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Servant.API ((:>))
import Servant.Server (Application, Handler, Server, serve)

runHttpServer :: Maybe Int -> IO ()
runHttpServer maybePort = do
  let httpSettings = maybe defaultSettings (flip setPort defaultSettings) maybePort
  runSettings
    httpSettings
    httpApp

httpApp :: Application
httpApp = serve (Proxy @API) httpServer

httpServer :: Server API
httpServer = getAddressCountHttpHandler

getAddressCountHttpHandler :: String -> Handler (Either (JsonRpcErr String) Int)
getAddressCountHttpHandler _ =
  pure $ Right 1

type API = "api" :> RawJsonRpc RpcMethod

type RpcMethod = RpcAddressCountMethod

type RpcAddressCountMethod =
  JsonRpc
    "getAddressCount"
    String
    String
    Int
