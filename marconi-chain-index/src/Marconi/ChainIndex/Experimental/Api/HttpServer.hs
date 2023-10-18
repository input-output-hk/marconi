{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.ChainIndex.Experimental.Api.HttpServer where

import Cardano.Api (AddressAny, serialiseAddress)
import Cardano.Api qualified as C
import Cardano.BM.Trace (Trace)
import Control.Lens (makeLenses, view, (^.), (^?))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, lift)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Marconi.ChainIndex.Experimental.Api.Routes (
  API,
  BurnTokenEventResult (BurnTokenEventResult),
  GetBurnTokenEventsParams (GetBurnTokenEventsParams),
  GetBurnTokenEventsResult (GetBurnTokenEventsResult),
  JsonRpcAPI,
  RestAPI,
 )
import Marconi.ChainIndex.Experimental.Indexers (
  MarconiChainIndexQueryables,
  queryableEpochState,
  queryableMintToken,
  -- queryableUtxo,
 )
import Marconi.ChainIndex.Experimental.Indexers.EpochState qualified as EpochState
import Marconi.ChainIndex.Experimental.Indexers.MintTokenEvent (
  mintAssetAssetName,
  mintAssetQuantity,
  mintAssetRedeemer,
  mintAssetRedeemerData,
  mintAssetRedeemerHash,
  mintTokenEventAsset,
  mintTokenEventBlockNo,
  mintTokenEventLocation,
  mintTokenEventTxId,
 )
import Marconi.ChainIndex.Experimental.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.ChainIndex.Types (SecurityParam)
import Marconi.Core qualified as Core
import Marconi.Core.JsonRpc (
  ReaderHandler,
  ReaderServer,
  catchHttpHandlerExceptions,
  dimapHandler,
  hoistReaderHandler,
  mkHttpRequestTracer,
  queryHttpReaderHandler,
 )
import Network.JsonRpc.Server.Types ()
import Network.JsonRpc.Types (JsonRpcErr, UnusedRequestParams)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Prometheus qualified as P
import Servant.API ((:<|>) ((:<|>)))
import Servant.Server (
  Application,
  Handler,
  hoistServer,
  serve,
 )

data HttpServerConfig = HttpServerConfig
  { _configTrace :: !(Trace IO Text)
  , _configPort :: !Int
  , _configSecurityParam :: !SecurityParam
  , _configTrackedAddresses :: ![AddressAny]
  , _configParams :: !Aeson.Value
  , _configQueryables :: !MarconiChainIndexQueryables
  }

makeLenses 'HttpServerConfig

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

------------------
-- JSON-RPC API --
------------------

jsonRpcServer :: ReaderServer HttpServerConfig JsonRpcAPI
jsonRpcServer =
  echo
    :<|> getTargetAddressesQueryHandler
    :<|> getEpochStakePoolDelegationHandler
    :<|> getEpochNonceHandler
    :<|> getBurnTokenEventsHandler

-- | Echo a message back as a JSON-RPC response. Used for testing the server.
echo
  :: String
  -> ReaderHandler HttpServerConfig (Either (JsonRpcErr String) String)
echo = return . Right

-- | Return the list of TargetAddresses in Bech32 representation.
getTargetAddressesQueryHandler
  :: UnusedRequestParams
  -- ^ Will be an empty string, empty object, or null, as we are ignoring this param, and returning everything
  -> ReaderHandler HttpServerConfig (Either (JsonRpcErr String) [Text])
getTargetAddressesQueryHandler _ = do
  addresses <- view configTrackedAddresses
  pure $ Right $ serialiseAddress <$> addresses

-- | Return the stake pool delegation per epoch
getEpochStakePoolDelegationHandler
  :: EpochState.ActiveSDDByEpochNoQuery
  -> ReaderHandler
      HttpServerConfig
      (Either (JsonRpcErr String) (Core.Result EpochState.ActiveSDDByEpochNoQuery))
getEpochStakePoolDelegationHandler = queryHttpReaderHandler (configQueryables . queryableEpochState)

-- | Return an epoch nonce
getEpochNonceHandler
  :: EpochState.NonceByEpochNoQuery
  -> ReaderHandler
      HttpServerConfig
      (Either (JsonRpcErr String) (Core.Result EpochState.NonceByEpochNoQuery))
getEpochNonceHandler = queryHttpReaderHandler (configQueryables . queryableEpochState)

getMintingPolicyHashTxHandler
  :: MintTokenEvent.QueryByAssetId MintTokenEvent.MintTokenBlockEvents
  -> ReaderHandler
      HttpServerConfig
      ( Either
          (JsonRpcErr String)
          ( Core.Result
              ( Core.WithStability
                  (MintTokenEvent.QueryByAssetId MintTokenEvent.MintTokenBlockEvents)
              )
          )
      )
getMintingPolicyHashTxHandler =
  queryHttpReaderHandler
    (configQueryables . queryableMintToken)
    . Core.WithStability

getBurnTokenEventsHandler
  :: GetBurnTokenEventsParams
  -> ReaderHandler HttpServerConfig (Either (JsonRpcErr String) GetBurnTokenEventsResult)
getBurnTokenEventsHandler = dimapHandler mapQuery mapResults getMintingPolicyHashTxHandler
  where
    mapQuery
      :: GetBurnTokenEventsParams
      -> MintTokenEvent.QueryByAssetId MintTokenEvent.MintTokenBlockEvents
    mapQuery (GetBurnTokenEventsParams policyId assetName beforeSlotNo afterTx) =
      MintTokenEvent.QueryByAssetId
        policyId
        assetName
        (Just MintTokenEvent.BurnEventType)
        beforeSlotNo
        afterTx
    mapResults
      :: [ Core.Stability
            ( Core.Timed
                (Core.Point MintTokenEvent.MintTokenBlockEvents)
                MintTokenEvent.MintTokenBlockEvents
            )
         ]
      -> GetBurnTokenEventsResult
    mapResults events = GetBurnTokenEventsResult $ mapResult =<< events
    mapResult
      :: Core.Stability
          ( Core.Timed
              C.ChainPoint
              MintTokenEvent.MintTokenBlockEvents
          )
      -> [BurnTokenEventResult]
    mapResult res = case res of
      Core.Stable x -> mapTimed True x
      Core.Volatile x -> mapTimed False x
    mapTimed
      :: Bool
      -> Core.Timed (Core.Point MintTokenEvent.MintTokenBlockEvents) MintTokenEvent.MintTokenBlockEvents
      -> [BurnTokenEventResult]
    mapTimed isStable (Core.Timed point (MintTokenEvent.MintTokenBlockEvents events)) =
      mapEvent point isStable <$> NonEmpty.toList events
    mapEvent :: C.ChainPoint -> Bool -> MintTokenEvent.MintTokenEvent -> BurnTokenEventResult
    mapEvent chainPoint isStable event =
      let (slotNo, headerHash) = case chainPoint of
            (C.ChainPoint sn hh) -> (sn, hh)
            C.ChainPointAtGenesis -> (0, "genesis")
       in BurnTokenEventResult
            slotNo
            headerHash
            (event ^. mintTokenEventLocation . mintTokenEventBlockNo)
            (event ^. mintTokenEventLocation . mintTokenEventTxId)
            (event ^? mintTokenEventAsset . mintAssetRedeemer . traverse . mintAssetRedeemerHash)
            (event ^? mintTokenEventAsset . mintAssetRedeemer . traverse . mintAssetRedeemerData)
            (event ^. mintTokenEventAsset . mintAssetAssetName)
            (abs $ event ^. mintTokenEventAsset . mintAssetQuantity)
            isStable

--------------
-- REST API --
--------------

restApiServer :: ReaderServer HttpServerConfig RestAPI
restApiServer =
  getTimeHandler
    :<|> getParamsHandler
    :<|> getTargetAddressesHandler
    :<|> getMetricsHandler

{- | Returns current time as REST response. Used for testing the http server outside of jsonrpc
 protocol.
-}
getTimeHandler :: ReaderHandler HttpServerConfig String
getTimeHandler = timeString <$> liftIO getCurrentTime
  where
    timeString = formatTime defaultTimeLocale "%T"

-- | Returns params given through CLI as a JSON REST response.
getParamsHandler :: ReaderHandler HttpServerConfig Aeson.Value
getParamsHandler = view configParams

-- | Prints TargetAddresses Bech32 representation to the console
getTargetAddressesHandler :: ReaderHandler HttpServerConfig [Text]
getTargetAddressesHandler = do
  addresses <- view configTrackedAddresses
  pure $ serialiseAddress <$> addresses

getMetricsHandler :: ReaderHandler HttpServerConfig Text
getMetricsHandler = liftIO $ Text.decodeUtf8 . BS.toStrict <$> P.exportMetricsAsText
