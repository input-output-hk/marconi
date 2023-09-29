{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.Tutorial.HttpServer where

import Control.Concurrent (MVar, readMVar)
import Control.Lens (to, view, (^.))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Data (Proxy (Proxy))
import Marconi.ChainIndex.Experimental.Indexers.Worker qualified as Core
import Marconi.Core qualified as Core
import Marconi.Tutorial.CLI (optionsHttpPort)
import Marconi.Tutorial.Env (
  Env,
  addressCountIndexerWorker,
  envCliArgs,
 )
import Marconi.Tutorial.Indexers.AddressCount (AddressCountQuery)
import Network.JsonRpc.Server.Types ()
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr, RawJsonRpc, mkJsonRpcInternalErr)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Servant ((:>))
import Servant.Server (Application, Handler, Server, serve)

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
  let httpSettings = maybe id setPort (optionsHttpPort cliOptions) defaultSettings
  liftIO $ runSettings httpSettings (httpApp env)

httpApp
  :: Env
  -> Application
httpApp env = serve (Proxy @API) (httpServer env)

httpServer
  :: Env
  -> Server API
httpServer = getAddressCountHttpHandler

getAddressCountHttpHandler
  :: Env
  -> AddressCountQuery
  -> Handler (Either (JsonRpcErr String) (Core.Result AddressCountQuery))
getAddressCountHttpHandler env =
  queryIndexerVarHandler (env ^. addressCountIndexerWorker . to Core.standardWorkerIndexerVar)

queryIndexerVarHandler
  :: ( Show (Core.Result query)
     , Ord (Core.Point event)
     , Core.Queryable (ExceptT (Core.QueryError query) Handler) event query indexer
     , Core.IsSync (ExceptT (Core.QueryError query) Handler) event indexer
     )
  => MVar (indexer event)
  -> query
  -> Handler (Either (JsonRpcErr String) (Core.Result query))
queryIndexerVarHandler indexerVar q = do
  indexer <- liftIO $ readMVar indexerVar
  toHttpHandler indexer q

toHttpHandler
  :: ( Show (Core.Result query)
     , Ord (Core.Point event)
     , Core.Queryable (ExceptT (Core.QueryError query) Handler) event query indexer
     , Core.IsSync (ExceptT (Core.QueryError query) Handler) event indexer
     )
  => indexer event
  -> query
  -> Handler (Either (JsonRpcErr String) (Core.Result query))
toHttpHandler indexer q = do
  countE <- runExceptT $ Core.queryLatest q indexer
  case countE of
    Left err -> pure $ Left $ mkJsonRpcInternalErr (Just $ show err)
    Right count -> pure $ Right count
