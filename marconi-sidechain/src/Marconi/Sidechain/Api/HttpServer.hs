{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Marconi.Sidechain.Api.HttpServer where

import Cardano.Api ()
import Cardano.BM.Trace (logDebug, logError)
import Control.Exception (SomeAsyncException, SomeException, catches, displayException, throwIO)
import Control.Exception qualified as Exception
import Control.Lens (view, (^.))
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (bimap), first)
import Data.ByteString qualified as BS
import Data.Default (def)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding qualified as Text
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Word (Word64)
import Marconi.ChainIndex.Types (MarconiTrace)
import Marconi.Sidechain.Api.Query.Indexers.EpochState qualified as EpochState
import Marconi.Sidechain.Api.Query.Indexers.MintBurn qualified as Q.Mint
import Marconi.Sidechain.Api.Query.Indexers.Utxo qualified as Q.Utxo
import Marconi.Sidechain.Api.Routes (
  API,
  GetBurnTokenEventsParams (afterTx, assetName, beforeSlotNo, policyId),
  GetBurnTokenEventsResult (GetBurnTokenEventsResult),
  GetCurrentSyncedBlockResult,
  GetEpochActiveStakePoolDelegationResult,
  GetEpochNonceResult,
  GetUtxosFromAddressParams (queryAddress, querySearchInterval),
  GetUtxosFromAddressResult,
  JsonRpcAPI,
  RestAPI,
 )
import Marconi.Sidechain.CLI (CliArgs)
import Marconi.Sidechain.Env (
  SidechainEnv,
  sidechainAddressUtxoIndexer,
  sidechainCliArgs,
  sidechainIndexersEnv,
  sidechainQueryEnv,
  sidechainQueryEnvHttpSettings,
  sidechainQueryEnvSecurityParam,
  sidechainTrace,
 )
import Marconi.Sidechain.Error (
  QueryExceptions (IndexerInternalError, QueryError, UnexpectedQueryResult, UntrackedPolicy),
 )
import Network.HTTP.Types (hContentType)
import Network.JsonRpc.Server.Types ()
import Network.JsonRpc.Types (
  JsonRpcErr,
  JsonRpcResponse (Errors),
  UnusedRequestParams,
  mkJsonRpcInternalErr,
  mkJsonRpcInvalidParamsErr,
 )
import Network.Wai.Handler.Warp (runSettings)
import Network.Wai.Middleware.RequestLogger (
  Destination (Callback),
  OutputFormat (DetailedWithSettings),
  destination,
  mPrelogRequests,
  mkRequestLogger,
  outputFormat,
 )
import Prettyprinter (Pretty (pretty))
import Prometheus qualified as P
import Servant.API ((:<|>) ((:<|>)))
import Servant.Server (
  Application,
  Handler (Handler),
  ServerError (errBody, errHeaders, errReasonPhrase),
  ServerT,
  err500,
  hoistServer,
  serve,
 )
import System.Log.FastLogger (fromLogStr)

-- | Bootstraps the HTTP server
runHttpServer :: ReaderT (SidechainEnv ann) IO ()
runHttpServer = do
  env <- ask
  requestLogger <-
    lift $
      mkRequestLogger
        def
          { destination = Callback $ logDebug (env ^. sidechainTrace) . pretty . Text.decodeUtf8 . fromLogStr
          , outputFormat = DetailedWithSettings def{mPrelogRequests = True}
          }
  lift
    $ runSettings
      (env ^. sidechainQueryEnv . sidechainQueryEnvHttpSettings)
    $ requestLogger
    $ marconiApp env

marconiApp :: SidechainEnv ann -> Application
marconiApp env =
  serve (Proxy @API) $
    hoistServer (Proxy @API) (hoistHandler env) httpRpcServer

type ReaderHandler env = ExceptT ServerError (ReaderT env IO)
type ReaderServer env api = ServerT api (ReaderHandler env)

hoistHandler :: SidechainEnv ann -> ReaderHandler (SidechainEnv ann) a -> Handler a
hoistHandler env = Handler . ExceptT . catchExceptions (env ^. sidechainTrace) . flip runReaderT env . runExceptT

catchExceptions :: MarconiTrace IO ann -> IO (Either ServerError a) -> IO (Either ServerError a)
catchExceptions trace action =
  action
    `catches` [ Exception.Handler $ \(e :: ServerError) -> pure $ Left e
              , Exception.Handler $ \(e :: SomeAsyncException) -> throwIO e
              , Exception.Handler $ \(e :: SomeException) -> do
                  logError trace $ "Exception caught while handling HTTP request: " <> pretty (displayException e)
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

jsonRpcServer :: ReaderServer (SidechainEnv ann) JsonRpcAPI
jsonRpcServer =
  echo
    :<|> getTargetAddressesQueryHandler
    :<|> getCurrentSyncedBlockHandler
    :<|> getAddressUtxoHandler
    :<|> getMintingPolicyHashTxHandler
    :<|> getEpochStakePoolDelegationHandler
    :<|> getEpochNonceHandler

restApiServer :: ReaderServer (SidechainEnv ann) RestAPI
restApiServer =
  getTimeHandler
    :<|> getParamsHandler
    :<|> getTargetAddressesHandler
    :<|> getMetricsHandler

httpRpcServer :: ReaderServer (SidechainEnv ann) API
httpRpcServer = jsonRpcServer :<|> restApiServer

-- | Echoes message back as a Jsonrpc response. Used for testing the server.
echo
  :: String
  -> ReaderHandler (SidechainEnv ann) (Either (JsonRpcErr String) String)
echo = return . Right

{- | Echos current time as REST response. Used for testing the http server outside of jsonrpc
 protocol.
-}
getTimeHandler :: ReaderHandler (SidechainEnv ann) String
getTimeHandler = timeString <$> liftIO getCurrentTime
  where
    timeString = formatTime defaultTimeLocale "%T"

-- | Returns params given through CLI as a JSON REST response.
getParamsHandler :: ReaderHandler (SidechainEnv ann) CliArgs
getParamsHandler = view sidechainCliArgs

-- | Prints TargetAddresses Bech32 representation to the console
getTargetAddressesHandler :: ReaderHandler (SidechainEnv ann) [Text]
getTargetAddressesHandler = do
  indexer <- view $ sidechainIndexersEnv . sidechainAddressUtxoIndexer
  pure $ Q.Utxo.reportBech32Addresses indexer

getMetricsHandler :: ReaderHandler (SidechainEnv ann) Text
getMetricsHandler = liftIO $ Text.decodeUtf8 . BS.toStrict <$> P.exportMetricsAsText

-- | Prints TargetAddresses Bech32 representation as thru JsonRpc
getTargetAddressesQueryHandler
  :: UnusedRequestParams
  -- ^ Will be an empty string, empty object, or null, as we are ignoring this param, and returning everything
  -> ReaderHandler (SidechainEnv ann) (Either (JsonRpcErr String) [Text])
getTargetAddressesQueryHandler _ = do
  indexer <- view $ sidechainIndexersEnv . sidechainAddressUtxoIndexer
  pure $ Right $ Q.Utxo.reportBech32Addresses indexer

-- | Handler for retrieving current synced chain point.
getCurrentSyncedBlockHandler
  :: UnusedRequestParams
  -- ^ Will be an empty string, empty object, or null, as we are ignoring this param, and returning everything
  -> ReaderHandler (SidechainEnv ann) (Either (JsonRpcErr String) GetCurrentSyncedBlockResult)
getCurrentSyncedBlockHandler _ = do
  indexer <- view $ sidechainIndexersEnv . sidechainAddressUtxoIndexer
  liftIO $ first toRpcErr <$> Q.Utxo.queryCurrentSyncedBlock indexer

-- | Handler for retrieving UTXOs by Address
getAddressUtxoHandler
  :: GetUtxosFromAddressParams
  -- ^ Bech32 addressCredential and a slotNumber
  -> ReaderHandler (SidechainEnv ann) (Either (JsonRpcErr String) GetUtxosFromAddressResult)
getAddressUtxoHandler query = do
  indexer <- view $ sidechainIndexersEnv . sidechainAddressUtxoIndexer
  liftIO $
    first toRpcErr <$> do
      Q.Utxo.findByBech32AddressAtSlot
        indexer
        (pack $ queryAddress query)
        (querySearchInterval query)

-- | Handler for retrieving Txs by Minting Policy Hash.
getMintingPolicyHashTxHandler
  :: GetBurnTokenEventsParams
  -> ReaderHandler (SidechainEnv ann) (Either (JsonRpcErr String) GetBurnTokenEventsResult)
getMintingPolicyHashTxHandler query = do
  env <- ask
  liftIO $
    bimap toRpcErr GetBurnTokenEventsResult <$> do
      Q.Mint.queryByAssetIdAtSlot
        (env ^. sidechainQueryEnv . sidechainQueryEnvSecurityParam)
        (env ^. sidechainIndexersEnv)
        (policyId query)
        (assetName query)
        (beforeSlotNo query)
        (afterTx query)

-- | Handler for retrieving stake pool delegation per epoch
getEpochStakePoolDelegationHandler
  :: Word64
  -- ^ EpochNo
  -> ReaderHandler
      (SidechainEnv ann)
      (Either (JsonRpcErr String) GetEpochActiveStakePoolDelegationResult)
getEpochStakePoolDelegationHandler epochNo = do
  env <- ask
  liftIO $
    first toRpcErr
      <$> EpochState.queryActiveSDDByEpochNo env epochNo

-- | Handler for retrieving stake pool delegation per epoch
getEpochNonceHandler
  :: Word64
  -- ^ EpochNo
  -> ReaderHandler (SidechainEnv ann) (Either (JsonRpcErr String) GetEpochNonceResult)
getEpochNonceHandler epochNo = do
  env <- ask
  liftIO $
    first toRpcErr
      <$> EpochState.queryNonceByEpochNo env epochNo

-- | Convert to JSON-RPC protocol error.
toRpcErr
  :: QueryExceptions
  -> JsonRpcErr String
toRpcErr (QueryError err) =
  mkJsonRpcInvalidParamsErr $ Just $ unpack err
toRpcErr (IndexerInternalError err) =
  mkJsonRpcInternalErr $ Just $ unpack err
toRpcErr (UnexpectedQueryResult q) =
  mkJsonRpcInternalErr $ Just $ "Unexpected result obtained for query '" <> show q <> "'"
toRpcErr (UntrackedPolicy _ _) =
  mkJsonRpcInvalidParamsErr $
    Just
      "The 'policyId' and 'assetName' param values must belong to the provided target 'AssetIds'"
