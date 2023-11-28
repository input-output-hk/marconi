{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Gather several indexers to expose a query that can provide you block information about the
current last synced block and the chain tip.
-}
module Marconi.ChainIndex.Indexers.CurrentSyncPointQuery (
  CurrentSyncPointQueryIndexer (..),
  CurrentSyncPointQuery (..),
  CurrentSyncPointResult (..),
) where

import Cardano.Api qualified as C
import Control.Concurrent qualified as Con
import Control.Lens ((^.))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as Text
import Marconi.ChainIndex.Indexers.BlockInfo (BlockInfo)
import Marconi.ChainIndex.Indexers.ChainTip ()
import Marconi.Core qualified as Core

data CurrentSyncPointEvent

type instance Core.Point CurrentSyncPointEvent = C.ChainPoint

data CurrentSyncPointQueryIndexer event = forall
    coordinator
    blockInfoIndexer
    blockInfoEvent
    chainTipIndexer
    chainTipEvent.
  ( Core.IsSync
      (ExceptT Core.IndexerError IO)
      event
      coordinator
  , Core.IsSync
      (ExceptT (Core.QueryError (Core.GetLastQuery C.ChainTip)) IO)
      chainTipEvent
      chainTipIndexer
  , Core.Queryable
      (ExceptT (Core.QueryError (Core.LatestEventsQuery BlockInfo)) IO)
      blockInfoEvent
      (Core.LatestEventsQuery BlockInfo)
      blockInfoIndexer
  , Core.Queryable
      (ExceptT (Core.QueryError (Core.GetLastQuery C.ChainTip)) IO)
      chainTipEvent
      (Core.GetLastQuery C.ChainTip)
      chainTipIndexer
  , Core.Point blockInfoEvent ~ C.ChainPoint
  , Core.Point chainTipEvent ~ C.ChainPoint
  ) =>
  CurrentSyncPointQueryIndexer
  { lastPointCoordinator :: coordinator event
  , blockInfoIndexer :: Con.MVar (blockInfoIndexer blockInfoEvent)
  , chainTipIndexer :: Con.MVar (chainTipIndexer chainTipEvent)
  }

data CurrentSyncPointResult = CurrentSyncPointResult
  { currentBlockInfo :: BlockInfo
  , currentTip :: C.ChainTip
  }
  deriving (Show)

data CurrentSyncPointQuery = CurrentSyncPointQuery
type instance Core.Result CurrentSyncPointQuery = Core.Timed C.ChainPoint CurrentSyncPointResult

instance
  (MonadIO m, MonadError (Core.QueryError CurrentSyncPointQuery) m)
  => Core.IsSync m event CurrentSyncPointQueryIndexer
  where
  lastSyncPoint (CurrentSyncPointQueryIndexer _coordinator _ _) = do
    mRes <- liftIO $ runExceptT @Core.IndexerError $ Core.lastSyncPoint _coordinator
    case mRes of
      Left _err -> throwError $ Core.IndexerQueryError "Can't get last synced point from the coordinator"
      Right r -> pure r
  lastStablePoint (CurrentSyncPointQueryIndexer _coordinator _ _) = do
    mRes <- liftIO $ runExceptT @Core.IndexerError $ Core.lastStablePoint _coordinator
    case mRes of
      Left _err -> throwError $ Core.IndexerQueryError "Can't get last stable point from the coordinator"
      Right r -> pure r

instance
  (MonadIO m, MonadError (Core.QueryError CurrentSyncPointQuery) m, Core.Point event ~ C.ChainPoint)
  => Core.Queryable
      m
      event
      CurrentSyncPointQuery
      CurrentSyncPointQueryIndexer
  where
  query point _ (CurrentSyncPointQueryIndexer _coordinator blockInfoIndexerMVar chainTipIndexerMvar) = do
    blockInfoIndexer' <- liftIO $ Con.readMVar blockInfoIndexerMVar
    blockInfos <-
      toSyncPointError $
        runExceptT @(Core.QueryError (Core.LatestEventsQuery BlockInfo)) $
          Core.query point Core.latestEvent blockInfoIndexer'
    chainTipIndexer' <- liftIO $ Con.readMVar chainTipIndexerMvar
    tips <- toSyncPointError $ runExceptT $ Core.queryLatest Core.GetLastQuery chainTipIndexer'
    case (blockInfos, tips) of
      ([timedBlockInfo], Just tip) -> do
        let blockInfo = timedBlockInfo ^. Core.event
        pure $ Core.Timed point $ CurrentSyncPointResult blockInfo tip
      ([_blockInfo], Nothing) -> throwError $ Core.IndexerQueryError "Tip not Found"
      ([], _tip) -> throwError $ Core.IndexerQueryError "Block info not found"
      (_blockInfo, _tip) -> throwError $ Core.IndexerQueryError "Too many blockInfo"

toSyncPointError
  :: ( Show err
     , MonadIO m
     , MonadError (Core.QueryError b) m
     )
  => IO (Either err a)
  -> m a
toSyncPointError mres = do
  res <- liftIO mres
  case res of
    Left _err ->
      throwError $
        Core.IndexerQueryError $
          Text.pack $
            "Can't resolve a part of CurrentSyncPointQuery: " <> show _err
    Right r -> pure r
