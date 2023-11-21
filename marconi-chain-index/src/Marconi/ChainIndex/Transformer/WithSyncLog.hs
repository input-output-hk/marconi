{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Marconi.ChainIndex.Transformer.WithSyncLog (
  WithSyncStats (..),
  LastSyncStats (..),
  SyncLog (..),
  withSyncStats,
) where

import Cardano.Api qualified as C
import Cardano.BM.Trace (logInfo)
import Control.Lens (Lens', makeLenses, traverseOf, (&), (+~), (.~), (?~), (^.))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (isJust)
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
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

-- | Logging datatype for information that occurred since the previous 'LastSyncLog'.
data SyncLog event = SyncLog
  { _syncLogStats :: LastSyncStats
  -- ^ Stats since the last syncing log message
  , _syncLogTracer :: MarconiTrace IO
  -- ^ The pretty-printing tracer
  , _syncLogTimeSinceLastMsg :: Maybe NominalDiffTime
  -- ^ Time since last syncing log message.
  }

-- | A logging modifier that adds stats logging to the indexer
newtype WithSyncStats indexer event = WithSyncStats
  { _syncStatsWrapper :: IndexTransformer SyncLog indexer event
  }

makeLenses 'LastSyncStats
makeLenses 'SyncLog
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
  -> WithSyncStats indexer event
withSyncStats tr idx =
  let stats = LastSyncStats 0 0 C.ChainPointAtGenesis C.ChainTipAtGenesis Nothing
   in WithSyncStats . IndexTransformer (SyncLog stats tr Nothing) $ idx

instance IndexerTrans WithSyncStats where
  unwrap = syncStatsWrapper . wrappedIndexer

instance
  (MonadIO m, MonadError Core.IndexerError m, IsIndex m TipAndBlock indexer)
  => IsIndex m TipAndBlock (WithSyncStats indexer)
  where
  index timedEvent indexer = do
    let tracer = indexer ^. syncStatsWrapper . wrapperConfig . syncLogTracer
        event = timedEvent ^. Core.event
        p = timedEvent ^. Core.point
    res <- case event of
      Just (TipAndBlock tip block) -> do
        res <- indexVia unwrap timedEvent indexer
        let updatedIndexer = chainTipUpdate res tip
        if isJust block
          then pure $ indexUpdate updatedIndexer p
          else pure updatedIndexer
      Nothing -> pure indexer
    printStats tracer res
  rollback cp indexer = do
    let tracer = indexer ^. syncStatsWrapper . wrapperConfig . syncLogTracer
    res <- rollbackVia unwrap cp indexer
    rollbackUpdate res cp & printStats tracer
  setLastStablePoint = setLastStablePointVia unwrap

printStats
  :: (MonadIO m)
  => MarconiTrace IO
  -> WithSyncStats indexer event
  -> m (WithSyncStats indexer event)
printStats tracer idx =
  liftIO $
    traverseOf (syncStatsWrapper . wrapperConfig) (printMessage tracer) idx

chainTipUpdate :: WithSyncStats indexer event -> C.ChainTip -> WithSyncStats indexer event
chainTipUpdate indexer ct =
  indexer
    & syncStatsWrapper
      . wrapperConfig
      . syncLogStats
      . syncStatsNodeTip
      .~ ct

indexUpdate :: WithSyncStats indexer event -> C.ChainPoint -> WithSyncStats indexer event
indexUpdate indexer cp =
  indexer
    & incrementDirection syncStatsNumBlocks
    & setChainPoint cp

rollbackUpdate :: WithSyncStats indexer event -> C.ChainPoint -> WithSyncStats indexer event
rollbackUpdate indexer cp =
  indexer
    & incrementDirection syncStatsNumRollbacks
    & setChainPoint cp

setChainPoint :: C.ChainPoint -> WithSyncStats indexer event -> WithSyncStats indexer event
setChainPoint cp =
  syncStatsWrapper
    . wrapperConfig
    . syncLogStats
    . syncStatsChainSyncPoint
    .~ cp

incrementDirection
  :: Lens' LastSyncStats Word64
  -> WithSyncStats indexer event
  -> WithSyncStats indexer event
incrementDirection direction =
  syncStatsWrapper
    . wrapperConfig
    . syncLogStats
    . direction
    +~ 1

printMessage :: MarconiTrace IO -> SyncLog event -> IO (SyncLog event)
printMessage tracer stats = do
  now <- getCurrentTime
  let minSecondsBetweenMsg :: NominalDiffTime
      minSecondsBetweenMsg = 10
  let timeSinceLastMsg = diffUTCTime now <$> (stats ^. syncLogStats . syncStatsLastMessageTime)
  -- Should only log if we never logged before and if at least 'minSecondsBetweenMsg' have
  -- passed after last log message.
  let shouldPrint = case timeSinceLastMsg of
        Nothing -> True
        Just t
          | t > minSecondsBetweenMsg -> True
          | otherwise -> False
  let resetStats sts =
        sts
          & syncLogStats . syncStatsNumBlocks .~ 0
          & syncLogStats . syncStatsNumRollbacks .~ 0
          & syncLogStats . syncStatsLastMessageTime ?~ now
  if shouldPrint
    then do
      let timeUpdatedStats = stats & syncLogTimeSinceLastMsg .~ timeSinceLastMsg
      logInfo tracer $ pretty timeUpdatedStats
      pure $ resetStats timeUpdatedStats
    else pure stats

instance Pretty (SyncLog event) where
  pretty = \case
    SyncLog (LastSyncStats numRollForward numRollBackwards cp nt _) _ timeSinceLastMsgM ->
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
