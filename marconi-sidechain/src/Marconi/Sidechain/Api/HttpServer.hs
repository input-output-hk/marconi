{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Marconi.Sidechain.Api.HttpServer where

import Cardano.Api ()
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, lift)
import Data.Bifunctor (Bifunctor (bimap), first)
import Data.ByteString qualified as BS
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
  GetCurrentSyncedBlockParams,
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
 )
import Marconi.Sidechain.Error (
  QueryExceptions (IndexerInternalError, QueryError, UnexpectedQueryResult, UntrackedPolicy),
 )
import Network.JsonRpc.Server.Types ()
import Network.JsonRpc.Types (
  JsonRpcErr (JsonRpcErr),
  mkJsonRpcParseErr,
 )
import Network.Wai.Handler.Warp (runSettings)
import Prometheus qualified as P
import Servant.API ((:<|>) ((:<|>)))
import Servant.Server (Application, Handler, Server, serve)

-- | Bootstraps the HTTP server
runHttpServer :: ReaderT SidechainEnv IO ()
runHttpServer = do
  env <- ask
  lift $
    runSettings
      (env ^. sidechainQueryEnv . sidechainQueryEnvHttpSettings)
      (marconiApp env)

marconiApp :: SidechainEnv -> Application
marconiApp env = serve (Proxy @API) (httpRpcServer env)

jsonRpcServer
  :: SidechainEnv
  -- ^  Utxo Environment to access Utxo Storage running on the marconi thread
  -> Server JsonRpcAPI
jsonRpcServer env =
  echo
    :<|> getTargetAddressesQueryHandler env
    :<|> getCurrentSyncedBlockHandler env
    :<|> getAddressUtxoHandler env
    :<|> getMintingPolicyHashTxHandler env
    :<|> getEpochStakePoolDelegationHandler env
    :<|> getEpochNonceHandler env

restApiServer
  :: SidechainEnv
  -> Server RestAPI
restApiServer env =
  getTimeHandler
    :<|> getParamsHandler env
    :<|> getTargetAddressesHandler env
    :<|> getMetricsHandler

httpRpcServer
  :: SidechainEnv
  -- ^  Utxo Environment to access Utxo Storage running on the marconi thread
  -> Server API
httpRpcServer env = jsonRpcServer env :<|> restApiServer env

-- | Echos message back as a Jsonrpc response. Used for testing the server.
echo
  :: String
  -> Handler (Either (JsonRpcErr String) String)
echo = return . Right

{- | Echos current time as REST response. Used for testing the http server outside of jsonrpc
 protocol.
-}
getTimeHandler :: Handler String
getTimeHandler = timeString <$> liftIO getCurrentTime
  where
    timeString = formatTime defaultTimeLocale "%T"

-- | Returns params given through CLI as a JSON REST response.
getParamsHandler
  :: SidechainEnv
  -> Handler CliArgs
getParamsHandler env = pure $ env ^. sidechainCliArgs

-- | Prints TargetAddresses Bech32 representation to the console
getTargetAddressesHandler
  :: SidechainEnv
  -- ^  Utxo Environment to access Utxo Storage running on the marconi thread
  -> Handler [Text]
getTargetAddressesHandler env =
  pure $
    Q.Utxo.reportBech32Addresses $
      env ^. sidechainIndexersEnv . sidechainAddressUtxoIndexer

getMetricsHandler :: Handler Text
getMetricsHandler = liftIO $ Text.decodeUtf8 . BS.toStrict <$> P.exportMetricsAsText

-- | Prints TargetAddresses Bech32 representation as thru JsonRpc
getTargetAddressesQueryHandler
  :: SidechainEnv
  -- ^ database configuration
  -> String
  -- ^ Will always be an empty string as we are ignoring this param, and returning everything
  -> Handler (Either (JsonRpcErr String) [Text])
getTargetAddressesQueryHandler env _ =
  pure $
    Right $
      Q.Utxo.reportBech32Addresses (env ^. sidechainIndexersEnv . sidechainAddressUtxoIndexer)

-- | Handler for retrieving current synced chain point.
getCurrentSyncedBlockHandler
  :: SidechainEnv
  -- ^ Utxo Environment to access Utxo Storage running on the marconi thread
  -> GetCurrentSyncedBlockParams
  -- ^ Will always be an empty string as we are ignoring this param, and returning everything
  -> Handler (Either (JsonRpcErr String) GetCurrentSyncedBlockResult)
getCurrentSyncedBlockHandler env _ =
  liftIO $
    first toRpcErr
      <$> Q.Utxo.queryCurrentSyncedBlock
        (env ^. sidechainIndexersEnv . sidechainAddressUtxoIndexer)

-- | Handler for retrieving UTXOs by Address
getAddressUtxoHandler
  :: SidechainEnv
  -- ^ Utxo Environment to access Utxo Storage running on the marconi thread
  -> GetUtxosFromAddressParams
  -- ^ Bech32 addressCredential and a slotNumber
  -> Handler (Either (JsonRpcErr String) GetUtxosFromAddressResult)
getAddressUtxoHandler env query =
  liftIO $
    first toRpcErr <$> do
      Q.Utxo.findByBech32AddressAtSlot
        (env ^. sidechainIndexersEnv . sidechainAddressUtxoIndexer)
        (pack $ queryAddress query)
        (querySearchInterval query)

-- | Handler for retrieving Txs by Minting Policy Hash.
getMintingPolicyHashTxHandler
  :: SidechainEnv
  -- ^ Utxo Environment to access Utxo Storage running on the marconi thread
  -> GetBurnTokenEventsParams
  -> Handler (Either (JsonRpcErr String) GetBurnTokenEventsResult)
getMintingPolicyHashTxHandler env query =
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
  :: SidechainEnv
  -- ^ Utxo Environment to access Utxo Storage running on the marconi thread
  -> Word64
  -- ^ EpochNo
  -> Handler (Either (JsonRpcErr String) GetEpochActiveStakePoolDelegationResult)
getEpochStakePoolDelegationHandler env epochNo =
  liftIO $
    first toRpcErr
      <$> EpochState.queryActiveSDDByEpochNo env epochNo

-- | Handler for retrieving stake pool delegation per epoch
getEpochNonceHandler
  :: SidechainEnv
  -- ^ Utxo Environment to access Utxo Storage running on the marconi thread
  -> Word64
  -- ^ EpochNo
  -> Handler (Either (JsonRpcErr String) GetEpochNonceResult)
getEpochNonceHandler env epochNo =
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
