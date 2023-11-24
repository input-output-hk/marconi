{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Marconi.Cardano.Core.Transformer.WithSyncLog (
  -- * Transformer
  WithSyncStats,
  withSyncStats,

  -- * Stats
  LastSyncStats (LastSyncStats),
  syncStatsNumBlocks,
  syncStatsNumRollbacks,
  syncStatsChainSyncPoint,
  syncStatsNodeTip,
  syncStatsLastMessageTime,
  emptyLastSyncStats,

  -- * Backend
  LoggingBackend (LoggingBackend),
  loggingBackendAction,
  loggingBackendTimeBetweenActions,
  loggingBackendState,
) where

import Cardano.Api qualified as C
import Control.Lens (Lens', makeLenses, traverseOf, (&), (+~), (.~), (?~), (^.))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (isJust)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Types (TipAndBlock (TipAndBlock))
import Marconi.Core qualified as Core
import Marconi.Core.Class (
  Closeable,
  IsIndex (index, rollback, setLastStablePoint),
  IsSync,
  Queryable,
 )
import Marconi.Core.Transformer.Class (IndexerTrans, unwrap)
import Marconi.Core.Transformer.IndexTransformer (
  IndexTransformer (IndexTransformer),
  indexVia,
  rollbackVia,
  setLastStablePointVia,
  wrappedIndexer,
  wrapperConfig,
 )

-- | Chain synchronisation statistics measured starting from previously measured 'LastSyncStats'.
data LastSyncStats = LastSyncStats
  { _syncStatsNumBlocks :: !Word64
  -- ^ Number of applied blocks since last message
  , _syncStatsNumRollbacks :: !Word64
  -- ^ Number of rollbacks since last message
  , _syncStatsChainSyncPoint :: C.ChainPoint
  -- ^ Chain index syncing point
  , _syncStatsNodeTip :: C.ChainTip
  -- ^ Current node tip
  , _syncStatsLastMessageTime :: !(Maybe UTCTime)
  -- ^ Timestamp of last printed message
  }
  deriving (Eq, Show)

emptyLastSyncStats :: LastSyncStats
emptyLastSyncStats = LastSyncStats 0 0 C.ChainPointAtGenesis C.ChainTipAtGenesis Nothing

newtype WithSyncStatsConfig event = WithSyncStatsConfig
  { _withSyncStatsConfigBackends :: [LoggingBackend]
  }

-- | The data needed to log to an arbitrary backend
data LoggingBackend = LoggingBackend
  { _loggingBackendAction :: LastSyncStats -> IO ()
  -- ^ Logging action
  , _loggingBackendTimeBetweenActions :: NominalDiffTime
  -- ^ How much time should elapse between calls to the logging action
  , _loggingBackendState :: LastSyncStats
  -- ^ The state of the stats for this backend
  }

-- | A logging modifier that adds stats logging to the indexer
newtype WithSyncStats indexer event = WithSyncStats
  { _syncStatsWrapper :: IndexTransformer WithSyncStatsConfig indexer event
  }

makeLenses 'LastSyncStats
makeLenses 'WithSyncStats
makeLenses 'WithSyncStatsConfig
makeLenses 'LoggingBackend

deriving via
  (IndexTransformer WithSyncStatsConfig indexer)
  instance
    (IsSync m event indexer) => IsSync m event (WithSyncStats indexer)

deriving via
  (IndexTransformer WithSyncStatsConfig indexer)
  instance
    (Queryable m event query indexer) => Queryable m event query (WithSyncStats indexer)

deriving via
  (IndexTransformer WithSyncStatsConfig indexer)
  instance
    (Closeable m indexer) => Closeable m (WithSyncStats indexer)

-- | A smart constructor for @WithSyncStats@
withSyncStats
  :: [LoggingBackend]
  -> indexer event
  -> WithSyncStats indexer event
withSyncStats backends = WithSyncStats . IndexTransformer (WithSyncStatsConfig backends)

instance IndexerTrans WithSyncStats where
  unwrap = syncStatsWrapper . wrappedIndexer

instance
  (MonadIO m, MonadError Core.IndexerError m, IsIndex m TipAndBlock indexer)
  => IsIndex m TipAndBlock (WithSyncStats indexer)
  where
  index timedEvent indexer = do
    let event = timedEvent ^. Core.event
        p = timedEvent ^. Core.point
    res <- case event of
      Just (TipAndBlock tip block) -> do
        res <- indexVia unwrap timedEvent indexer
        let updatedTipIndexer =
              res
                & syncStatsWrapper
                  . wrapperConfig
                  . withSyncStatsConfigBackends
                  . traverse
                  . loggingBackendState
                  . syncStatsNodeTip
                  .~ tip
            updateIndexerBlocks idx cp =
              idx
                & incrementDirection syncStatsNumBlocks
                & setSyncStatsSyncPoint cp
        if isJust block
          then pure $ updateIndexerBlocks updatedTipIndexer p
          else pure updatedTipIndexer
      Nothing -> pure indexer
    liftIO $
      traverseOf
        (syncStatsWrapper . wrapperConfig . withSyncStatsConfigBackends . traverse)
        runAction
        res
  rollback cp indexer = do
    let
      updateIndexerRollbacks idx chainPoint =
        idx
          & incrementDirection syncStatsNumRollbacks
          & setSyncStatsSyncPoint chainPoint
    res <- rollbackVia unwrap cp indexer
    liftIO
      $ traverseOf
        (syncStatsWrapper . wrapperConfig . withSyncStatsConfigBackends . traverse)
        runAction
      $ updateIndexerRollbacks res cp
  setLastStablePoint = setLastStablePointVia unwrap

-- | Runs the action if enough time has elapsed that the @LoggingBackend@'s frequency is respected.
runAction :: LoggingBackend -> IO LoggingBackend
runAction
  backend@( LoggingBackend
              action
              timeBetweenActions
              lss@(LastSyncStats _ _ _ _ timeOfLastMsg)
            ) = do
    now <- getCurrentTime
    let shouldPrint =
          case timeOfLastMsg of
            Nothing -> True
            Just t
              | diffUTCTime now t > timeBetweenActions -> True
              | otherwise -> False
        resetStats sts =
          sts
            & syncStatsNumBlocks .~ 0
            & syncStatsNumRollbacks .~ 0
            & syncStatsLastMessageTime ?~ now

    if shouldPrint
      then do
        action lss
        pure $ backend & loggingBackendState .~ resetStats lss
      else pure backend

{- | Takes a @ChainPoint@ and a @WithSyncStats@ indexer.

    Sets the sync point of the indexer's stats.
-}
setSyncStatsSyncPoint :: C.ChainPoint -> WithSyncStats indexer event -> WithSyncStats indexer event
setSyncStatsSyncPoint cp =
  syncStatsWrapper
    . wrapperConfig
    . withSyncStatsConfigBackends
    . traverse
    . loggingBackendState
    . syncStatsChainSyncPoint
    .~ cp

{- | Takes a lens intended to be to either @syncStatsNumBlocks@ or @syncStatsNumRollbacks@ and
    a @WithSyncStats@ indexer, and increments the value of the lens by one.
-}
incrementDirection
  :: Lens' LastSyncStats Word64
  -> WithSyncStats indexer event
  -> WithSyncStats indexer event
incrementDirection direction =
  syncStatsWrapper
    . wrapperConfig
    . withSyncStatsConfigBackends
    . traverse
    . loggingBackendState
    . direction
    +~ 1
