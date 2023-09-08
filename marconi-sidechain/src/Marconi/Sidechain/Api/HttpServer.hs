{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Marconi.Sidechain.Api.HttpServer where

import Cardano.Api ()
import Cardano.BM.Trace (logDebug)
import Control.Lens (view, (^.))
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Bifunctor (Bifunctor (bimap), first)
import Data.ByteString qualified as BS
import Data.Default (def)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Word (Word64)
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
import Network.JsonRpc.Server.Types ()
import Network.JsonRpc.Types (
  JsonRpcErr (JsonRpcErr),
  UnusedRequestParams,
  mkJsonRpcParseErr,
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
import Prometheus qualified as P
import Servant.API ((:<|>) ((:<|>)))
import Servant.Server (Application, Handler (Handler), ServerError, ServerT, hoistServer, serve)
import System.Log.FastLogger (fromLogStr)

-- | Bootstraps the HTTP server
runHttpServer :: ReaderT SidechainEnv IO ()
runHttpServer = do
  env <- ask
  requestLogger <-
    lift $
      mkRequestLogger
        def
          { destination = Callback $ logDebug (env ^. sidechainTrace) . Text.decodeUtf8 . fromLogStr
          , outputFormat = DetailedWithSettings def{mPrelogRequests = True}
          }
  lift
    $ runSettings
      (env ^. sidechainQueryEnv . sidechainQueryEnvHttpSettings)
    $ requestLogger
    $ marconiApp env

marconiApp :: SidechainEnv -> Application
marconiApp env =
  serve (Proxy @API) $
    hoistServer (Proxy @API) (hoistHandler env) httpRpcServer

type ReaderHandler env = ExceptT ServerError (ReaderT env IO)
type ReaderServer env api = ServerT api (ReaderHandler env)

hoistHandler :: env -> ReaderHandler env a -> Handler a
hoistHandler env = Handler . ExceptT . flip runReaderT env . runExceptT

jsonRpcServer :: ReaderServer SidechainEnv JsonRpcAPI
jsonRpcServer =
  echo
    :<|> getTargetAddressesQueryHandler
    :<|> getCurrentSyncedBlockHandler
    :<|> getAddressUtxoHandler
    :<|> getMintingPolicyHashTxHandler
    :<|> getEpochStakePoolDelegationHandler
    :<|> getEpochNonceHandler

restApiServer :: ReaderServer SidechainEnv RestAPI
restApiServer =
  getTimeHandler
    :<|> getParamsHandler
    :<|> getTargetAddressesHandler
    :<|> getMetricsHandler

httpRpcServer :: ReaderServer SidechainEnv API
httpRpcServer = jsonRpcServer :<|> restApiServer

-- | Echoes message back as a Jsonrpc response. Used for testing the server.
echo
  :: String
  -> ReaderHandler SidechainEnv (Either (JsonRpcErr String) String)
echo = return . Right

{- | Echos current time as REST response. Used for testing the http server outside of jsonrpc
 protocol.
-}
getTimeHandler :: ReaderHandler SidechainEnv String
getTimeHandler = timeString <$> liftIO getCurrentTime
  where
    timeString = formatTime defaultTimeLocale "%T"

-- | Returns params given through CLI as a JSON REST response.
getParamsHandler :: ReaderHandler SidechainEnv CliArgs
getParamsHandler = view sidechainCliArgs

-- | Prints TargetAddresses Bech32 representation to the console
getTargetAddressesHandler :: ReaderHandler SidechainEnv [Text]
getTargetAddressesHandler = do
  indexer <- view $ sidechainIndexersEnv . sidechainAddressUtxoIndexer
  pure $ Q.Utxo.reportBech32Addresses indexer

getMetricsHandler :: ReaderHandler SidechainEnv Text
getMetricsHandler = liftIO $ Text.decodeUtf8 . BS.toStrict <$> P.exportMetricsAsText

-- | Prints TargetAddresses Bech32 representation as thru JsonRpc
getTargetAddressesQueryHandler
  :: UnusedRequestParams
  -- ^ Will be an empty string, empty object, or null, as we are ignoring this param, and returning everything
  -> ReaderHandler SidechainEnv (Either (JsonRpcErr String) [Text])
getTargetAddressesQueryHandler _ = do
  indexer <- view $ sidechainIndexersEnv . sidechainAddressUtxoIndexer
  pure $ Right $ Q.Utxo.reportBech32Addresses indexer

-- | Handler for retrieving current synced chain point.
getCurrentSyncedBlockHandler
  :: UnusedRequestParams
  -- ^ Will be an empty string, empty object, or null, as we are ignoring this param, and returning everything
  -> ReaderHandler SidechainEnv (Either (JsonRpcErr String) GetCurrentSyncedBlockResult)
getCurrentSyncedBlockHandler _ = do
  indexer <- view $ sidechainIndexersEnv . sidechainAddressUtxoIndexer
  liftIO $ first toRpcErr <$> Q.Utxo.queryCurrentSyncedBlock indexer

-- | Handler for retrieving UTXOs by Address
getAddressUtxoHandler
  :: GetUtxosFromAddressParams
  -- ^ Bech32 addressCredential and a slotNumber
  -> ReaderHandler SidechainEnv (Either (JsonRpcErr String) GetUtxosFromAddressResult)
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
  -> ReaderHandler SidechainEnv (Either (JsonRpcErr String) GetBurnTokenEventsResult)
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
  -> ReaderHandler SidechainEnv (Either (JsonRpcErr String) GetEpochActiveStakePoolDelegationResult)
getEpochStakePoolDelegationHandler epochNo = do
  env <- ask
  liftIO $
    first toRpcErr
      <$> EpochState.queryActiveSDDByEpochNo env epochNo

-- | Handler for retrieving stake pool delegation per epoch
getEpochNonceHandler
  :: Word64
  -- ^ EpochNo
  -> ReaderHandler SidechainEnv (Either (JsonRpcErr String) GetEpochNonceResult)
getEpochNonceHandler epochNo = do
  env <- ask
  liftIO $
    first toRpcErr
      <$> EpochState.queryNonceByEpochNo env epochNo

-- | Convert to JSON-RPC protocol error.
toRpcErr
  :: QueryExceptions
  -> JsonRpcErr String
toRpcErr (QueryError e) =
  -- TODO Change to specific code and message
  mkJsonRpcParseErr $ Just $ unpack e
toRpcErr (UnexpectedQueryResult e) =
  -- TODO Change to specific code and message
  mkJsonRpcParseErr $ Just $ show e
toRpcErr (UntrackedPolicy _ _) =
  mkJsonRpcParseErr $
    Just
      "The 'policyId' and 'assetName' param values must belong to the provided target 'AssetIds'."
toRpcErr (IndexerInternalError err) =
  JsonRpcErr (-32001) (Text.unpack err) Nothing
