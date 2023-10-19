{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Experimental.Indexers.CurrentSyncPointQuery (
  CurrentSyncPointQueryIndexer (CurrentSyncPointQueryIndexer),
  CurrentSyncPointQuery (..),
  CurrentSyncPointResult (..),
) where

import qualified Cardano.Api as C
import qualified Control.Concurrent as Con
import Control.Lens ((^.))
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Marconi.ChainIndex.Experimental.Indexers.BlockInfo (BlockInfo)
import Marconi.ChainIndex.Experimental.Indexers.ChainTip ()
import qualified Marconi.Core as Core

data CurrentSyncPointEvent

type instance Core.Point CurrentSyncPointEvent = C.ChainPoint

data CurrentSyncPointQueryIndexer event = forall
    blockInfoIndexer
    blockInfoEvent
    chainTipIndexer
    chainTipEvent.
  ( Core.IsSync
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
  { _blockInfoIndexer :: Con.MVar (blockInfoIndexer blockInfoEvent)
  , _chainTipIndexer :: Con.MVar (chainTipIndexer chainTipEvent)
  }

data CurrentSyncPointResult = CurrentSyncPointResult
  { currentBlockInfo :: BlockInfo
  , currentTip :: C.ChainTip
  }

data CurrentSyncPointQuery = CurrentSyncPointQuery
type instance Core.Result CurrentSyncPointQuery = Core.Timed C.ChainPoint CurrentSyncPointResult

instance
  (MonadIO m, MonadError (Core.QueryError CurrentSyncPointQuery) m)
  => Core.Queryable
      m
      CurrentSyncPointEvent
      CurrentSyncPointQuery
      CurrentSyncPointQueryIndexer
  where
  query point _ (CurrentSyncPointQueryIndexer blockInfoIndexerMVar chainTipIndexerMvar) = do
    blockInfoIndexer <- liftIO $ Con.readMVar blockInfoIndexerMVar
    blockInfos <-
      toSyncPointError $
        runExceptT @(Core.QueryError (Core.LatestEventsQuery BlockInfo)) $
          Core.query point Core.latestEvent blockInfoIndexer
    chainTipIndexer <- liftIO $ Con.readMVar chainTipIndexerMvar
    tips <- toSyncPointError $ runExceptT $ Core.queryLatest Core.GetLastQuery chainTipIndexer
    case (blockInfos, tips) of
      ([timedBlockInfo], Just tip) -> do
        let blockInfo = timedBlockInfo ^. Core.event
        pure $ Core.Timed point $ CurrentSyncPointResult blockInfo tip
      _other -> throwError Core.NotStoredAnymore

toSyncPointError
  :: ( MonadIO m
     , MonadError (Core.QueryError b) m
     )
  => IO (Either err a)
  -> m a
toSyncPointError mres = do
  res <- liftIO mres
  case res of
    Left _ -> throwError $ Core.IndexerQueryError "Can't resolve a part of CurrentSyncPointQuery"
    Right r -> pure r
