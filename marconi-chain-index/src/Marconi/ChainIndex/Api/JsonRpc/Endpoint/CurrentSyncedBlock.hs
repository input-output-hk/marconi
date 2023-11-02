{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Api.JsonRpc.Endpoint.CurrentSyncedBlock (
  GetCurrentSyncedBlockResult (GetCurrentSyncedBlockResult),
  RpcGetCurrentSyncedBlock,
  getCurrentSyncedBlockHandler,
) where

import Cardano.Api qualified as C
import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Aeson (ToJSON (toJSON), (.=))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Word (Word64)
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.CurrentSyncedBlock.Tip (
  Tip,
  fromChainTip,
 )
import Marconi.ChainIndex.Api.Types (HttpServerConfig, configQueryables)
import Marconi.ChainIndex.Indexers (queryableCurrentSyncPoint)
import Marconi.ChainIndex.Indexers.BlockInfo (BlockInfo)
import Marconi.ChainIndex.Indexers.BlockInfo qualified as BlockInfo
import Marconi.ChainIndex.Indexers.CurrentSyncPointQuery qualified as CurrentSyncPoint
import Marconi.Core qualified as Core
import Marconi.Core.JsonRpc (ReaderHandler, queryErrToRpcErr)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr, UnusedRequestParams, mkJsonRpcInternalErr)
import Servant (Handler, runHandler)

type RpcGetCurrentSyncedBlock =
  JsonRpc
    "getCurrentSyncedBlock"
    UnusedRequestParams
    String
    GetCurrentSyncedBlockResult

data GetCurrentSyncedBlockResult = GetCurrentSyncedBlockResult
  { blockNo :: Maybe C.BlockNo
  , blockHeaderHash :: Maybe (C.Hash C.BlockHeader)
  , slotNo :: Maybe C.SlotNo
  , nodeTip :: Maybe Tip
  , blockTimestamp :: Maybe Word64
  , blockEpochNo :: Maybe C.EpochNo
  }
  deriving (Eq, Ord, Show)

instance ToJSON GetCurrentSyncedBlockResult where
  toJSON (GetCurrentSyncedBlockResult (Just bn) (Just bh) (Just sn) tip (Just ts) (Just en)) =
    let jsonNodeTip =
          case tip of
            Nothing -> []
            Just tip' -> ["nodeTip" .= toJSON tip']
     in Aeson.object $
          [ "blockNo" .= bn
          , "blockTimestamp" .= ts
          , "blockHeaderHash" .= bh
          , "slotNo" .= sn
          , "epochNo" .= en
          ]
            <> jsonNodeTip
  toJSON (GetCurrentSyncedBlockResult _ _ _ tip _ _) =
    Aeson.object $
      case tip of
        Nothing -> []
        Just tip' -> ["nodeTip" .= toJSON tip']

getCurrentSyncPointHandler
  :: ReaderHandler
      HttpServerConfig
      ( Either
          (JsonRpcErr String)
          (Core.Result CurrentSyncPoint.CurrentSyncPointQuery)
      )
getCurrentSyncPointHandler = do
  indexer <- Lens.view (configQueryables . queryableCurrentSyncPoint)
  lastPointM <- hoistHttpHandler $ runExceptT $ Core.lastSyncPoint indexer
  case lastPointM of
    Left err ->
      pure $
        Left $
          mkJsonRpcInternalErr $
            Just $
              "Can't resolve last point in getCurrentSyncedBlock: " <> show err
    Right lastPoint ->
      hoistHttpHandler $
        liftIO $
          first queryErrToRpcErr <$> Core.queryEither lastPoint CurrentSyncPoint.CurrentSyncPointQuery indexer

getCurrentSyncedBlockHandler
  :: UnusedRequestParams
  -> ReaderHandler
      HttpServerConfig
      ( Either
          (JsonRpcErr String)
          GetCurrentSyncedBlockResult
      )
getCurrentSyncedBlockHandler =
  let processCurrentSync :: C.ChainPoint -> BlockInfo -> Maybe Tip -> GetCurrentSyncedBlockResult
      processCurrentSync C.ChainPointAtGenesis _blockInfo tip =
        GetCurrentSyncedBlockResult Nothing Nothing Nothing tip Nothing Nothing
      processCurrentSync (C.ChainPoint slotNo' hash) blockInfo tip =
        GetCurrentSyncedBlockResult
          (Just $ blockInfo ^. BlockInfo.blockNo)
          (Just hash)
          (Just slotNo')
          tip
          (Just $ blockInfo ^. BlockInfo.timestamp)
          (Just $ blockInfo ^. BlockInfo.epochNo)
      mapResult :: Core.Result CurrentSyncPoint.CurrentSyncPointQuery -> GetCurrentSyncedBlockResult
      mapResult (Core.Timed point (CurrentSyncPoint.CurrentSyncPointResult blockInfo tip)) =
        processCurrentSync point blockInfo $ fromChainTip tip
   in const $ fmap mapResult <$> getCurrentSyncPointHandler

hoistHttpHandler :: Handler a -> ReaderHandler env a
hoistHttpHandler = ExceptT . ReaderT . const . runHandler
