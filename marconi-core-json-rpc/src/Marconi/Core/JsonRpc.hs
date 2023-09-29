{-# LANGUAGE RankNTypes #-}

{- |
 This library provides some utilities for running `marconi-core`'s 'Queryable' interface throught a
 JSON-RPC HTTP server.

 Particularly, these utilities use the `servant-server` Haskell package.
-}
module Marconi.Core.JsonRpc (
  queryHttpHandler,
  queryIndexerVarHttpHandler,
  queryIndexerHttpHandler,
) where

import Control.Concurrent (MVar, readMVar)
import Control.Lens (Getter, (^.))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Marconi.Core qualified as Core
import Network.JsonRpc.Server.Types ()
import Network.JsonRpc.Types (JsonRpcErr, mkJsonRpcInternalErr)
import Servant.Server (Handler)

{- | Given an @env@ and a way to get the indexer inside an 'MVar', run the @query@ in a Servant
'Handler'.
-}
queryHttpHandler
  :: ( Show (Core.Result query)
     , Ord (Core.Point event)
     , Core.Queryable (ExceptT (Core.QueryError query) Handler) event query indexer
     , Core.IsSync (ExceptT (Core.QueryError query) Handler) event indexer
     )
  => Getter env (MVar (indexer event))
  -> env
  -> query
  -> Handler (Either (JsonRpcErr String) (Core.Result query))
queryHttpHandler indexerVarGetter env query = do
  queryIndexerVarHttpHandler (env ^. indexerVarGetter) query

-- | Given an @indexer event@ inside an 'MVar', run the @query@ in a Servant 'Handler'.
queryIndexerVarHttpHandler
  :: ( Show (Core.Result query)
     , Ord (Core.Point event)
     , Core.Queryable (ExceptT (Core.QueryError query) Handler) event query indexer
     , Core.IsSync (ExceptT (Core.QueryError query) Handler) event indexer
     )
  => MVar (indexer event)
  -> query
  -> Handler (Either (JsonRpcErr String) (Core.Result query))
queryIndexerVarHttpHandler indexerVar q = do
  indexer <- liftIO $ readMVar indexerVar
  queryIndexerHttpHandler indexer q

-- | Given an @indexer event@, run the @query@ in a Servant 'Handler'.
queryIndexerHttpHandler
  :: ( Show (Core.Result query)
     , Ord (Core.Point event)
     , Core.Queryable (ExceptT (Core.QueryError query) Handler) event query indexer
     , Core.IsSync (ExceptT (Core.QueryError query) Handler) event indexer
     )
  => indexer event
  -> query
  -> Handler (Either (JsonRpcErr String) (Core.Result query))
queryIndexerHttpHandler indexer q = do
  countE <- runExceptT $ Core.queryLatest q indexer
  case countE of
    Left err -> pure $ Left $ mkJsonRpcInternalErr (Just $ show err)
    Right count -> pure $ Right count
