{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Logging (
  LastSyncStats (..),
  LastSyncLog (..),
  chainSyncEventStreamLogging,
  MarconiTrace,
  mkMarconiTrace,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (
  BlockEvent (BlockEvent),
  ChainSyncEvent (RollBackward, RollForward),
 )
import Cardano.BM.Trace (Trace, logInfo)
import Cardano.BM.Tracing (contramap)
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
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
import Prettyprinter (Pretty (pretty), (<+>))
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Text qualified as Pretty
import Streaming (Of, Stream, effect)
import Streaming.Prelude qualified as S
import Text.Printf (printf)

-- | Builds a 'MarconiTrace' from a base tracer.
mkMarconiTrace :: Trace m Text -> MarconiTrace m ann
mkMarconiTrace =
  contramap
    ( (fmap . fmap)
        ( Pretty.renderStrict
            . Pretty.layoutPretty Pretty.defaultLayoutOptions
        )
    )

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
                 in "Synchronising ("
                      <> pretty (printf "%.2f" pct :: String)
                      <> "%)."
                      <+> currentTipMsg timeSinceLastMsgM
                      <+> processingSummaryMsg timeSinceLastMsg
                      <+> pretty (printf "(%.0f blocks/s)." rate :: String)

chainSyncEventStreamLogging
  :: MarconiTrace IO ann
  -> Stream (Of (ChainSyncEvent BlockEvent)) IO r
  -> Stream (Of (ChainSyncEvent BlockEvent)) IO r
chainSyncEventStreamLogging tracer s = effect $ do
  stats <- newIORef (LastSyncStats 0 0 C.ChainPointAtGenesis C.ChainTipAtGenesis Nothing)
  return $ S.chain (update stats) s
  where
    minSecondsBetweenMsg :: NominalDiffTime
    minSecondsBetweenMsg = 10

    update
      :: IORef LastSyncStats
      -> ChainSyncEvent BlockEvent
      -> IO ()
    update statsRef (RollForward (BlockEvent bim _epochNo _posixTime) ct) = do
      let cp = case bim of
            (C.BlockInMode (C.Block (C.BlockHeader slotNo hash _blockNo) _txs) _eim) ->
              C.ChainPoint slotNo hash
      modifyIORef' statsRef $ \stats ->
        stats
          { syncStatsNumBlocks = syncStatsNumBlocks stats + 1
          , syncStatsChainSyncPoint = cp
          , syncStatsNodeTip = ct
          }
      printMessage statsRef
    update statsRef (RollBackward cp ct) = do
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

      when shouldPrint $ do
        logInfo tracer $ pretty (LastSyncLog syncStats timeSinceLastMsg)
        modifyIORef' statsRef $ \stats ->
          stats
            { syncStatsNumBlocks = 0
            , syncStatsNumRollbacks = 0
            , syncStatsLastMessageTime = Just now
            }
