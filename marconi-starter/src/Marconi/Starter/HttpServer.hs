{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.Starter.HttpServer where

import Control.Lens (to, view, (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Data (Proxy (Proxy))
import Marconi.Cardano.Core.Indexer.Worker qualified as Core
import Marconi.Core qualified as Core
import Marconi.Core.JsonRpc qualified as Core
import Marconi.Starter.CLI (optionsHttpPort)
import Marconi.Starter.Env (
  Env,
  addressCountIndexerWorker,
  envCliArgs,
 )
import Marconi.Starter.Indexers.AddressCount (AddressCountQuery)
import Network.JsonRpc.Server.Types ()
import Network.JsonRpc.Types (JsonRpc, RawJsonRpc)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Servant ((:>))
import Servant.Server (Application, Server, serve)

{----------------------
   API specification
-----------------------}

type API = "api" :> RawJsonRpc RpcMethod

type RpcMethod = RpcAddressCountMethod

type RpcAddressCountMethod =
  JsonRpc
    "getAddressCount"
    AddressCountQuery
    String
    (Core.Result AddressCountQuery)

{----------------------
   API implementation
-----------------------}

runHttpServer :: ReaderT Env IO ()
runHttpServer = do
  env <- ask
  let cliOptions = view envCliArgs env
  let httpSettings = setPort (optionsHttpPort cliOptions) defaultSettings
  liftIO $ runSettings httpSettings (httpApp env)

httpApp
  :: Env
  -> Application
httpApp env = serve (Proxy @API) (httpServer env)

httpServer
  :: Env
  -> Server API
httpServer env =
  Core.queryIndexerVarHttpHandler $
    env ^. addressCountIndexerWorker . to Core.standardWorkerIndexerVar
