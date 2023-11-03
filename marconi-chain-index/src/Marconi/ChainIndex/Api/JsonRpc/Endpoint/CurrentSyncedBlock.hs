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
import Control.Monad.Except (runExceptT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import GHC.Generics (Generic)
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
import Marconi.Core.JsonRpc (ReaderHandler, hoistHttpHandler, queryErrToRpcErr)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr, UnusedRequestParams, mkJsonRpcInternalErr)

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
  }
  deriving (Eq, Ord, Generic, Show, FromJSON, ToJSON)

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
      processCurrentSync C.ChainPointAtGenesis _blockInfo tip = GetCurrentSyncedBlockResult Nothing Nothing Nothing tip
      processCurrentSync (C.ChainPoint slotNo' hash) blockInfo tip =
        GetCurrentSyncedBlockResult (Just $ blockInfo ^. BlockInfo.blockNo) (Just hash) (Just slotNo') tip
      mapResult :: Core.Result CurrentSyncPoint.CurrentSyncPointQuery -> GetCurrentSyncedBlockResult
      mapResult (Core.Timed point (CurrentSyncPoint.CurrentSyncPointResult blockInfo tip)) =
        processCurrentSync point blockInfo $ fromChainTip tip
   in const $ fmap mapResult <$> getCurrentSyncPointHandler
