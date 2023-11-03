{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{- |
 This library provides some utilities for running `marconi-core`'s 'Queryable' interface throught a
 JSON-RPC HTTP server.

 Particularly, these utilities use the `servant-server` Haskell package.
-}
module Marconi.Core.JsonRpc (
  ReaderHandler,
  ReaderServer,
  catchHttpHandlerExceptions,
  dimapHandler,
  hoistHttpHandler,
  hoistReaderHandler,
  mkHttpRequestTracer,
  queryHttpHandler,
  queryHttpReaderHandler,
  queryIndexerHttpHandler,
  queryIndexerVarHttpHandler,
  queryErrToRpcErr,
) where

import Cardano.BM.Trace (Trace, logDebug, logError)
import Control.Concurrent (MVar, readMVar)
import Control.Exception (SomeAsyncException, SomeException, catches, displayException, throwIO)
import Control.Exception qualified as Exception
import Control.Lens (Getter, view, (^.))
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (ReaderT), runReaderT)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Default (def)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Marconi.Core qualified as Core
import Network.HTTP.Types (hContentType)
import Network.JsonRpc.Server.Types ()
import Network.JsonRpc.Types (
  JsonRpcErr,
  JsonRpcResponse (Errors),
  mkJsonRpcInternalErr,
  mkJsonRpcInvalidParamsErr,
 )
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (
  Destination (Callback),
  OutputFormat (DetailedWithSettings),
  destination,
  mPrelogRequests,
  mkRequestLogger,
  outputFormat,
 )
import Servant.Server (
  Handler (Handler),
  ServerError (errBody, errHeaders, errReasonPhrase),
  ServerT,
  err500,
  runHandler,
 )
import System.Log.FastLogger (fromLogStr)

type ReaderHandler env = ExceptT ServerError (ReaderT env IO)
type ReaderServer env api = ServerT api (ReaderHandler env)

hoistReaderHandler :: env -> ReaderHandler env a -> Handler a
hoistReaderHandler env = Handler . ExceptT . flip runReaderT env . runExceptT

hoistHttpHandler :: Handler a -> ReaderHandler env a
hoistHttpHandler = ExceptT . ReaderT . const . runHandler

catchHttpHandlerExceptions :: Trace IO Text -> Handler a -> Handler a
catchHttpHandlerExceptions trace action =
  Handler . ExceptT $
    catches
      (runHandler action)
      [ Exception.Handler $ \(e :: ServerError) -> pure $ Left e
      , Exception.Handler $ \(e :: SomeAsyncException) -> throwIO e
      , Exception.Handler $ \(e :: SomeException) -> do
          liftIO . logError trace . Text.pack $
            "Exception caught while handling HTTP request: " <> displayException e
          let
            -- TODO: Provide better info, especially the request ID
            jsonErr = Errors @Int @Aeson.Value Nothing $ mkJsonRpcInternalErr Nothing
          pure . Left $
            err500
              { errHeaders = [(hContentType, "application/json; charset=utf-8")]
              , errReasonPhrase = "Internal server error"
              , errBody = Aeson.encode jsonErr
              }
      ]

mkHttpRequestTracer :: (MonadIO m) => Trace IO Text -> m Middleware
mkHttpRequestTracer trace =
  liftIO $
    mkRequestLogger
      def
        { destination = Callback $ logDebug trace . Text.decodeUtf8 . fromLogStr
        , outputFormat = DetailedWithSettings def{mPrelogRequests = True}
        }

{- | Given two mapping functions, one for the query and one for the result,
convert a query handler into one that takes a modified version of the query
and returns a modified version of the result.

This is technically a profunctor operation with some extra levels of @Functor@
on the right: @dimapHandler l ≡ dimap l . fmap . fmap@

However, since we need it only for @(->)@ it's not worth pulling in the `profunctor`
dependency.

=== __Example__ ===
@
originalHandler
  :: SomeQuery -> ReaderHandler HttpServerConfig (Either (JsonRpcErr String) SomeResult)
originalHandler = queryHttpReaderHandler someLens

modifiedHandler
  :: ModifiedQuery -> ReaderHandler HttpServerConfig (Either (JsonRpcErr String) ModifiedResult)
modifiedHandler =
  let mapQuery = ... :: ModifiedQuery -> SomeQuery
      mapResult = ... :: SomeResult -> ModifiedResult
   in dimapHandler mapQuery mapResult originalHandler
@
-}
dimapHandler
  :: (Functor m)
  => (qry' -> qry)
  -> (res -> res')
  -> (qry -> m (Either e res))
  -> (qry' -> m (Either e res'))
dimapHandler mapQry mapRes handler = fmap (fmap mapRes) . handler . mapQry

-- | Given a way to get the indexer from the environment, run the @query@ in a 'ReaderHandler'.
queryHttpReaderHandler
  :: ( Show (Core.Result query)
     , Ord (Core.Point event)
     , Core.Queryable (ExceptT (Core.QueryError query) IO) event query indexer
     , Core.IsSync IO event indexer
     )
  => Getter env (MVar (indexer event))
  -> query
  -> ReaderHandler env (Either (JsonRpcErr String) (Core.Result query))
queryHttpReaderHandler indexerVarGetter query = do
  var <- view indexerVarGetter
  hoistHttpHandler $ queryIndexerVarHttpHandler var query

{- | Given an @env@ and a way to get the indexer inside an 'MVar', run the @query@ in a Servant
'Handler'.
-}
queryHttpHandler
  :: ( Show (Core.Result query)
     , Ord (Core.Point event)
     , Core.Queryable (ExceptT (Core.QueryError query) IO) event query indexer
     , Core.IsSync IO event indexer
     )
  => Getter env (MVar (indexer event))
  -> env
  -> query
  -> Handler (Either (JsonRpcErr String) (Core.Result query))
queryHttpHandler indexerVarGetter env =
  queryIndexerVarHttpHandler (env ^. indexerVarGetter)

-- | Given an @indexer event@ inside an 'MVar', run the @query@ in a Servant 'Handler'.
queryIndexerVarHttpHandler
  :: ( Show (Core.Result query)
     , Ord (Core.Point event)
     , Core.Queryable (ExceptT (Core.QueryError query) IO) event query indexer
     , Core.IsSync IO event indexer
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
     , Core.Queryable (ExceptT (Core.QueryError query) IO) event query indexer
     , Core.IsSync IO event indexer
     )
  => indexer event
  -> query
  -> Handler (Either (JsonRpcErr String) (Core.Result query))
queryIndexerHttpHandler indexer q =
  liftIO $ first queryErrToRpcErr <$> Core.queryLatestEither q indexer

-- | Convert a query error to a JSON-RPC protocol error.
queryErrToRpcErr
  :: (Show (Core.Result query))
  => Core.QueryError query
  -> JsonRpcErr String
queryErrToRpcErr = \case
  Core.AheadOfLastSync mResult ->
    mkJsonRpcInvalidParamsErr . Just $
      "The required point is ahead of the current index"
        <> case mResult of
          Nothing -> ""
          Just r -> ". The latest result was: " <> show r
  Core.NotStoredAnymore ->
    mkJsonRpcInvalidParamsErr . Just $
      "The requested point is too far in the past and has been pruned"
  Core.IndexerQueryError msg ->
    mkJsonRpcInternalErr . Just $
      "The indexer query failed: " <> Text.unpack msg
  Core.SlotNoBoundsInvalid msg ->
    mkJsonRpcInvalidParamsErr . Just $
      "The SlotNo bounds provided in the query are not consistent: " <> Text.unpack msg
