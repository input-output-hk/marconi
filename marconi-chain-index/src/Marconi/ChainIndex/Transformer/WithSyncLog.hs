{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Marconi.ChainIndex.Transformer.WithSyncLog (
  WithSyncStats (..),
  LastSyncLog (..),
  LastSyncStats (..),
  withSyncStats,
) where

import Cardano.Api qualified as C
import Cardano.BM.Trace (logInfo)
import Control.Lens (makeLenses, view, (^.))
import Control.Monad (when)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (MarconiTrace, TipAndBlock (TipAndBlock))
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
import Prettyprinter (Pretty, pretty, (<+>))
import Text.Printf (printf)

-- | Chain synchronisation statistics measured starting from previously measured 'LastSyncStats'.
data LastSyncStats = LastSyncStats
  { syncStatsNumBlocks :: !Word64
  -- ^ Number of applied blocks since last message
  , syncStatsNumRollbacks :: !Word64
  -- ^ Number of rollbacks since last message
  , syncStatsChainSyncPoint :: C.ChainPoint
  -- ^ Chain index syncing point
  , syncStatsNodeTip :: C.ChainTip
  -- ^ Current node tip
  , syncStatsLastMessageTime :: !(Maybe UTCTime)
  -- ^ Timestamp of last printed message
  }
  deriving (Eq, Show)

-- | Logging datatype for information that occured since the previous 'LastSyncLog'.
data LastSyncLog = LastSyncLog
  { syncStatsSyncLog :: LastSyncStats
  -- ^ Stats since the last syncing log message
  , timeSinceLastMsgSyncLog :: Maybe NominalDiffTime
  -- ^ Time since last syncing log message.
  }
  deriving stock (Eq, Show, Generic)

-- | Logging datatype for information that occured since the previous 'LastSyncLog'.
data SyncLog event = SyncLog
  { _syncLogStats :: IORef LastSyncStats
  -- ^ Stats since the last syncing log message
  , _syncLogTracer :: MarconiTrace IO
  -- ^ The pretty-printing tracer
  }

makeLenses ''SyncLog

-- | A logging modifier that adds stats logging to the indexer
newtype WithSyncStats indexer event = WithSyncStats
  { _syncStatsWrapper :: IndexTransformer SyncLog indexer event
  }

makeLenses 'WithSyncStats

deriving via
  (IndexTransformer SyncLog indexer)
  instance
    (IsSync m event indexer) => IsSync m event (WithSyncStats indexer)

deriving via
  (IndexTransformer SyncLog indexer)
  instance
    (Queryable m event query indexer) => Queryable m event query (WithSyncStats indexer)

deriving via
  (IndexTransformer SyncLog indexer)
  instance
    (Closeable m indexer) => Closeable m (WithSyncStats indexer)

-- | A smart constructor for @WithSyncStats@
withSyncStats
  :: MarconiTrace IO
  -> indexer event
  -> IO (WithSyncStats indexer event)
withSyncStats tr idx = do
  stats <- newIORef (LastSyncStats 0 0 C.ChainPointAtGenesis C.ChainTipAtGenesis Nothing)
  pure $ WithSyncStats . IndexTransformer (SyncLog stats tr) $ idx

instance IndexerTrans WithSyncStats where
  unwrap = syncStatsWrapper . wrappedIndexer

instance
  (MonadIO m, MonadError Core.IndexerError m, IsIndex m TipAndBlock indexer)
  => IsIndex m TipAndBlock (WithSyncStats indexer)
  where
  index timedEvent indexer = do
    let stats = indexer ^. syncStatsWrapper . wrapperConfig . syncLogStats
        tracer = indexer ^. syncStatsWrapper . wrapperConfig . syncLogTracer
        event = timedEvent ^. Core.event
    res <- case event of
      Just (TipAndBlock tip block) -> do
        {- The order here is important.

          We must index the block before the tip, but update the stats with the tip before the
          block. -}
        res <- indexVia unwrap timedEvent indexer
        liftIO $ chainTipUpdate stats tip
        case block of
          Just b -> liftIO $ runUpdate stats b
          Nothing -> pure ()
        pure res
      Nothing -> pure indexer
    liftIO $ printMessage tracer stats
    pure res
    where
      runUpdate
        :: IORef LastSyncStats
        -> Core.ProcessedInput C.ChainPoint a
        -> IO ()
      runUpdate stats = \case
        Core.Index (Core.Timed cp _) -> indexUpdate stats cp
        Core.IndexAllDescending es -> traverse_ (indexUpdate stats . view Core.point) es
        Core.Rollback cp -> rollbackUpdate stats cp
        _ -> pure ()

  rollback cp indexer = do
    let stats = indexer ^. syncStatsWrapper . wrapperConfig . syncLogStats
        tracer = indexer ^. syncStatsWrapper . wrapperConfig . syncLogTracer
    res <- rollbackVia unwrap cp indexer
    liftIO $ rollbackUpdate stats cp
    liftIO $ printMessage tracer stats
    pure res
  setLastStablePoint = setLastStablePointVia unwrap

chainTipUpdate :: IORef LastSyncStats -> C.ChainTip -> IO ()
chainTipUpdate statsRef ct =
  modifyIORef' statsRef $
    \stats ->
      stats
        { syncStatsNodeTip = ct
        }

indexUpdate :: IORef LastSyncStats -> C.ChainPoint -> IO ()
indexUpdate statsRef cp =
  modifyIORef' statsRef $
    \stats ->
      stats
        { syncStatsNumBlocks = syncStatsNumBlocks stats + 1
        , syncStatsChainSyncPoint = cp
        }

rollbackUpdate :: IORef LastSyncStats -> C.ChainPoint -> IO ()
rollbackUpdate statsRef cp = do
  modifyIORef' statsRef $ \stats ->
    stats
      { syncStatsNumRollbacks = syncStatsNumRollbacks stats + 1
      , syncStatsChainSyncPoint = cp
      }

printMessage :: MarconiTrace IO -> IORef LastSyncStats -> IO ()
printMessage tracer statsRef = do
  syncStats@LastSyncStats{syncStatsLastMessageTime} <- readIORef statsRef
  now <- getCurrentTime
  let minSecondsBetweenMsg :: NominalDiffTime
      minSecondsBetweenMsg = 10
  let timeSinceLastMsg = diffUTCTime now <$> syncStatsLastMessageTime
  -- Should only log if we never logged before and if at least 'minSecondsBetweenMsg' have
  -- passed after last log message.
  let shouldPrint = case timeSinceLastMsg of
        Nothing -> True
        Just t
          | t > minSecondsBetweenMsg -> True
          | otherwise -> False
  when shouldPrint $ do
    logInfo tracer $ pretty (LastSyncLog syncStats timeSinceLastMsg)
    modifyIORef' statsRef $ \stats ->
      stats
        { syncStatsNumBlocks = 0
        , syncStatsNumRollbacks = 0
        , syncStatsLastMessageTime = Just now
        }

instance Pretty LastSyncLog where
  pretty = \case
    LastSyncLog (LastSyncStats numRollForward numRollBackwards cp nt _) timeSinceLastMsgM ->
      let currentTipMsg Nothing = ""
          currentTipMsg (Just _) =
            "Current synced point is"
              <+> pretty cp
              <+> "and current node tip is"
              <+> pretty nt
              <> "."

          processingSummaryMsg timeSinceLastMsg =
            "Processed"
              <+> pretty numRollForward
              <+> "blocks and"
              <+> pretty numRollBackwards
              <+> "rollbacks in the last"
              <+> pretty (formatTime defaultTimeLocale "%s" timeSinceLastMsg)
              <> "s"
       in case (timeSinceLastMsgM, cp, nt) of
            (Nothing, _, _) ->
              "Starting from"
                <+> pretty cp
                <> "."
                <+> currentTipMsg timeSinceLastMsgM
            (Just _, _, C.ChainTipAtGenesis) ->
              "Not syncing. Node tip is at Genesis"
            -- This case statement should never happen.
            (Just timeSinceLastMsg, C.ChainPointAtGenesis, C.ChainTip{}) ->
              "Synchronising (0%)."
                <+> currentTipMsg timeSinceLastMsgM
                <+> processingSummaryMsg timeSinceLastMsg
                <> "."
            ( Just timeSinceLastMsg
              , C.ChainPoint (C.SlotNo chainSyncSlot) _
              , C.ChainTip (C.SlotNo nodeTipSlot) _ _
              )
                | nodeTipSlot == chainSyncSlot ->
                    "Fully synchronised."
                      <+> currentTipMsg timeSinceLastMsgM
                      <+> processingSummaryMsg timeSinceLastMsg
                      <> "."
            ( Just timeSinceLastMsg
              , C.ChainPoint (C.SlotNo chainSyncSlot) _
              , C.ChainTip (C.SlotNo nodeTipSlot) _ _
              ) ->
                let pct = ((100 :: Double) * fromIntegral chainSyncSlot) / fromIntegral nodeTipSlot
                    rate = fromIntegral numRollForward / realToFrac timeSinceLastMsg :: Double
                    {- If the percentage will be rounded up to 100, we want to avoid logging that
                      as it's not very user friendly (it falsely implies full synchronisation) -}
                    progressStr =
                      if pct < 99.995
                        then pretty (printf "%.2f" pct :: String) <> "%"
                        else "almost synced"
                 in "Synchronising ("
                      <> progressStr
                      <> ")."
                      <+> currentTipMsg timeSinceLastMsgM
                      <+> processingSummaryMsg timeSinceLastMsg
                      <+> pretty (printf "(%.0f blocks/s)." rate :: String)
