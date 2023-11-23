{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Marconi.Cardano.Core.Transformer.WithSyncLog (
  WithSyncStats (..),
  LastSyncStats (..),
  withSyncStats,
  mkPrintBackend,
  mkPrometheusBackend,
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
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Types (MarconiTrace, TipAndBlock (TipAndBlock))
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
import Prometheus qualified as P
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

emptyLastSyncStats :: LastSyncStats
emptyLastSyncStats = LastSyncStats 0 0 C.ChainPointAtGenesis C.ChainTipAtGenesis Nothing

newtype WithSyncStatsConfig event = WithSyncStatsConfig
  { _withSyncStatsConfigBackends :: [LoggingBackend]
  }

-- SyncLog -> IO ()
-- SyncLog -> IO SyncLog

data LoggingBackend = LoggingBackend
  { _loggingBackendTracer :: LastSyncStats -> IO () -- Tracer IO LastSyncStats
  , _loggingBackendTimeBetweenMessages :: NominalDiffTime
  , _loggingBackendState :: LastSyncStats
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
                & setChainPoint cp
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
          & setChainPoint chainPoint
    res <- rollbackVia unwrap cp indexer
    liftIO
      $ traverseOf
        (syncStatsWrapper . wrapperConfig . withSyncStatsConfigBackends . traverse)
        runAction
      $ updateIndexerRollbacks res cp
  setLastStablePoint = setLastStablePointVia unwrap

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

setChainPoint :: C.ChainPoint -> WithSyncStats indexer event -> WithSyncStats indexer event
setChainPoint cp =
  syncStatsWrapper
    . wrapperConfig
    . withSyncStatsConfigBackends
    . traverse
    . loggingBackendState
    . syncStatsChainSyncPoint
    .~ cp

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

mkPrometheusBackend :: IO LoggingBackend
mkPrometheusBackend = do
  processedBlocksCounter <-
    P.register $
      P.gauge (P.Info "processed_blocks_counter" "Number of processed blocks")
  processedRollbacksCounter <-
    P.register $
      P.gauge (P.Info "processed_rollbacks_counter" "Number of processed rollbacks")
  blocksPerSecondCounter <-
    P.register $
      P.gauge (P.Info "processed_blocks_per_second_counter" "Average of processed blocks per second")
  pure $
    LoggingBackend
      (updateMetrics processedBlocksCounter processedRollbacksCounter blocksPerSecondCounter)
      60
      emptyLastSyncStats
  where
    updateMetrics :: P.Gauge -> P.Gauge -> P.Gauge -> LastSyncStats -> IO ()
    updateMetrics rollforwards rollbacks blocksPerSecondGauge (LastSyncStats fw bw _ _ lastTime) = do
      now <- getCurrentTime
      let timeSinceLastMsg = diffUTCTime now <$> lastTime
          blocksThisUpdate = fromInteger . toInteger $ fw
          blocksPerSecondThisUpdate =
            maybe
              0
              (((blocksThisUpdate * 1000000000000) `div`) . fromEnum)
              timeSinceLastMsg
      _ <- P.setGauge rollforwards (fromInteger . toInteger $ fw)
      _ <- P.setGauge rollbacks (fromInteger . toInteger $ bw)
      _ <- P.setGauge blocksPerSecondGauge (toEnum blocksPerSecondThisUpdate)
      pure ()

mkPrintBackend :: MarconiTrace IO -> LoggingBackend
mkPrintBackend tracer = LoggingBackend (printMessage tracer) 10 emptyLastSyncStats

printMessage :: MarconiTrace IO -> LastSyncStats -> IO ()
printMessage tracer stats = do
  now <- getCurrentTime
  let timeSinceLastMsg = diffUTCTime now <$> (stats ^. syncStatsLastMessageTime)

  logInfo tracer $ pretty $ LastSyncStatsOutput stats timeSinceLastMsg

data LastSyncStatsOutput = LastSyncStatsOutput LastSyncStats (Maybe NominalDiffTime)

instance Pretty LastSyncStatsOutput where
  pretty = \case
    LastSyncStatsOutput (LastSyncStats numRollForward numRollBackwards cp nt _) timeSinceLastMsgM ->
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
