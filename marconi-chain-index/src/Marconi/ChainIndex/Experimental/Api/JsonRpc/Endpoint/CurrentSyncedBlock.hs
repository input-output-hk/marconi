{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Experimental.Api.JsonRpc.Endpoint.CurrentSyncedBlock (
  RpcGetCurrentSyncedBlock,
  getCurrentSyncedBlockHandler,
) where

import Cardano.Api qualified as C
import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Experimental.Api.JsonRpc.Endpoint.CurrentSyncedBlock.Tip (
  Tip,
  fromChainTip,
 )
import Marconi.ChainIndex.Experimental.Api.Types (HttpServerConfig, configQueryables)
import Marconi.ChainIndex.Experimental.Indexers (queryableCurrentSyncPoint)
import Marconi.ChainIndex.Experimental.Indexers.BlockInfo (BlockInfo)
import Marconi.ChainIndex.Experimental.Indexers.BlockInfo qualified as BlockInfo
import Marconi.ChainIndex.Experimental.Indexers.CurrentSyncPointQuery qualified as CurrentSyncPoint
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
    Left _err -> pure $ Left $ mkJsonRpcInternalErr $ Just "Can't resolve last point it getCurrentSyncedPoint"
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

hoistHttpHandler :: Handler a -> ReaderHandler env a
hoistHttpHandler = ExceptT . ReaderT . const . runHandler
