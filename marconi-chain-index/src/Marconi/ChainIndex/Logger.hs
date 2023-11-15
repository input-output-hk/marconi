{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Common loggers for marconi
module Marconi.ChainIndex.Logger (
  BM.nullTracer,
  defaultStdOutLogger,
  mkMarconiTrace,
  chainSyncEventStreamLogging,
  LastSyncLog (..),
  LastSyncStats (..),
  marconiFormatting,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (
  BlockEvent (BlockEvent),
  ChainSyncEvent (RollBackward, RollForward),
 )
import Cardano.BM.Backend.Switchboard qualified as BM
import Cardano.BM.Configuration qualified as BM
import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Setup qualified as BM
import Cardano.BM.Trace (logInfo)
import Cardano.BM.Tracing (contramap)
import Cardano.BM.Tracing qualified as BM
import Control.Arrow ((&&&))
import Control.Concurrent (modifyMVar_, newMVar, readMVar)
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Time (
  NominalDiffTime,
  UTCTime,
  defaultTimeLocale,
  diffUTCTime,
  formatTime,
  getCurrentTime,
 )
import Data.Word (Word64)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (MarconiTrace)
import Marconi.Core qualified as Core
import Prettyprinter (Pretty (pretty), (<+>))
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Text qualified as Pretty
import Streaming (MonadIO (liftIO), Of, Stream, effect)
import Streaming.Prelude qualified as S
import Text.Printf (printf)

-- | StdOut logger, only log stuff above the Info level
defaultStdOutLogger :: Text -> IO (Trace IO Text, BM.Switchboard Text)
defaultStdOutLogger appName = do
  cfg <- BM.defaultConfigStdout
  BM.setMinSeverity cfg BM.Info
  BM.setupTrace_ cfg appName

-- | Builds a 'MarconiTrace' from a base tracer.
mkMarconiTrace :: Trace m Text -> MarconiTrace m
mkMarconiTrace =
  contramap . fmap . fmap $ marconiFormatting

marconiFormatting :: Pretty.Doc ann -> Text
marconiFormatting =
  Pretty.renderStrict
    . Pretty.layoutPretty Pretty.defaultLayoutOptions

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
                | nodeTipSlot - chainSyncSlot < 100 ->
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

-- | Adds logging to the event stream
chainSyncEventStreamLogging
  :: MarconiTrace IO
  -> Stream (Of (ChainSyncEvent BlockEvent)) IO r
  -> Stream (Of (Core.ProcessQueueItem C.ChainPoint (ChainSyncEvent BlockEvent))) IO r
chainSyncEventStreamLogging tracer s = effect $ do
  stats <- newIORef (LastSyncStats 0 0 C.ChainPointAtGenesis C.ChainTipAtGenesis Nothing)
  ref <- liftIO $ newMVar mempty
  let memo a act = do
        m <- readMVar ref
        case Map.lookup a m of
          Just _ -> pure ()
          Nothing -> do
            act
            modifyMVar_ ref (pure . Map.union (Map.singleton a ()))
  pure $ S.map (uncurry Core.ProcessQueueItem . (update memo stats &&& id)) s
  where
    minSecondsBetweenMsg :: NominalDiffTime
    minSecondsBetweenMsg = 10

    {- We need to memoise our actions because we can get multiple events at the same ChainPoint,
    results in, for example, a higher @syncStatsNumBlocks@ than we'd expect -}
    runUpdate :: (C.ChainPoint -> IO () -> IO ()) -> C.ChainPoint -> C.ChainPoint -> IO () -> IO ()
    runUpdate memo cp cp' = when (cp' == cp) . memo cp'

    update
      :: (C.ChainPoint -> IO () -> IO ())
      -> IORef LastSyncStats
      -> ChainSyncEvent BlockEvent
      -> (C.ChainPoint -> IO ())
    update memo statsRef = do
      \case
        (RollForward (BlockEvent bim _epochNo _posixTime) ct) -> do
          let cp = case bim of
                (C.BlockInMode (C.Block (C.BlockHeader slotNo hash _blockNo) _txs) _eim) ->
                  C.ChainPoint slotNo hash
          \cp' -> runUpdate memo cp cp' $
            do
              modifyIORef' statsRef $ \stats ->
                stats
                  { syncStatsNumBlocks = syncStatsNumBlocks stats + 1
                  , syncStatsChainSyncPoint = cp
                  , syncStatsNodeTip = ct
                  }
              printMessage statsRef
        (RollBackward cp ct) -> \cp' -> runUpdate memo cp cp' $ do
          modifyIORef' statsRef $ \stats ->
            stats
              { syncStatsNumRollbacks = syncStatsNumRollbacks stats + 1
              , syncStatsChainSyncPoint = cp
              , syncStatsNodeTip = ct
              }
          printMessage statsRef

    printMessage :: IORef LastSyncStats -> IO ()
    printMessage statsRef = do
      syncStats@LastSyncStats{syncStatsLastMessageTime} <- readIORef statsRef

      now <- getCurrentTime

      let timeSinceLastMsg = diffUTCTime now <$> syncStatsLastMessageTime

      -- Should only log if we never logged before and if at least 'minSecondsBetweenMsg' have
      -- passed after last log message.
      let shouldPrint = case timeSinceLastMsg of
            Nothing -> True
            Just t
              | t > minSecondsBetweenMsg -> True
              | otherwise -> False

      if shouldPrint
        then do
          liftIO $ modifyIORef' statsRef $ \stats ->
            stats
              { syncStatsNumBlocks = 0
              , syncStatsNumRollbacks = 0
              , syncStatsLastMessageTime = Just now
              }
          logInfo tracer $ pretty (LastSyncLog syncStats timeSinceLastMsg)
        else pure mempty
