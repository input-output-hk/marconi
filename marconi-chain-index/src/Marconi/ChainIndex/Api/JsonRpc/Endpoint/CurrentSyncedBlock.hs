{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Marconi.ChainIndex.Api.JsonRpc.Endpoint.CurrentSyncedBlock (
  GetCurrentSyncedBlockResult (GetCurrentSyncedBlockResult),
  RpcGetCurrentSyncedBlock,
  getCurrentSyncedBlockHandler,
) where

import Cardano.Api qualified as C
import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader ()
import Data.Aeson ()
import Data.Aeson.TH (Options (omitNothingFields), defaultOptions, deriveJSON, fieldLabelModifier)
import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.Word (Word64)
import Marconi.Cardano.Indexers (queryableCurrentSyncPoint)
import Marconi.Cardano.Indexers.BlockInfo (BlockInfo)
import Marconi.Cardano.Indexers.BlockInfo qualified as BlockInfo
import Marconi.Cardano.Indexers.CurrentSyncPointQuery qualified as CurrentSyncPoint
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.CurrentSyncedBlock.Tip (
  Tip,
  fromChainTip,
 )
import Marconi.ChainIndex.Api.Types (HttpServerConfig, configQueryables)
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
  { currentSyncedBlockResultBlockNo :: Maybe C.BlockNo
  , currentSyncedBlockResultBlockTimestamp :: Maybe Word64
  , currentSyncedBlockResultBlockHeaderHash :: Maybe (C.Hash C.BlockHeader)
  , currentSyncedBlockResultSlotNo :: Maybe C.SlotNo
  , currentSyncedBlockResultEpochNo :: Maybe C.EpochNo
  , currentSyncedBlockResultNodeTip :: Maybe Tip
  }
  deriving (Eq, Ord, Show)

$( deriveJSON
    defaultOptions
      { fieldLabelModifier = \str ->
          case drop 24 str of
            c : rest -> toLower c : rest
            _ -> error "Malformed label in JSON type GetCurrentSyncedBlockResult."
      , omitNothingFields = True
      }
    ''GetCurrentSyncedBlockResult
 )

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
        GetCurrentSyncedBlockResult Nothing Nothing Nothing Nothing Nothing tip
      processCurrentSync (C.ChainPoint slotNo' hash) blockInfo tip =
        GetCurrentSyncedBlockResult
          (Just $ blockInfo ^. BlockInfo.blockNo)
          (Just $ blockInfo ^. BlockInfo.timestamp)
          (Just hash)
          (Just slotNo')
          (Just $ blockInfo ^. BlockInfo.epochNo)
          tip
      mapResult :: Core.Result CurrentSyncPoint.CurrentSyncPointQuery -> GetCurrentSyncedBlockResult
      mapResult (Core.Timed point (CurrentSyncPoint.CurrentSyncPointResult blockInfo tip)) =
        processCurrentSync point blockInfo $ fromChainTip tip
   in const $ fmap mapResult <$> getCurrentSyncPointHandler
