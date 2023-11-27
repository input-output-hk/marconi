{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Marconi.Cardano.Core.Transformer.WithSyncStats.Backend.Printer (
  LastSyncStatsOutput (..),
  mkLogBackend,
) where

import Cardano.Api qualified as C
import Cardano.BM.Trace (logInfo)
import Control.Lens ((^.))
import Data.Time (NominalDiffTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Marconi.Cardano.Core.Transformer.WithSyncStats (
  LastSyncStats (LastSyncStats),
  StatsBackend (StatsBackend),
  emptyLastSyncStats,
  syncStatsLastMessageTime,
 )
import Marconi.Cardano.Core.Types (MarconiTrace)
import Prettyprinter (Pretty (pretty), (<+>))
import Text.Printf (printf)

{- | Creates a simple pretty-printing backend which prints the number of processed blocks and
		the number of rollbacks to a given tracer.

		Takes a @NominalDiffTime@ which determines how frequently we send stats to Prometheus and a
		@MarconiTrace IO@ with which it performs the tracing.
-}
mkLogBackend :: MarconiTrace IO -> NominalDiffTime -> StatsBackend
mkLogBackend tracer timeBetween =
  StatsBackend
    (logLastSyncStats tracer)
    timeBetween
    emptyLastSyncStats

logLastSyncStats :: MarconiTrace IO -> LastSyncStats -> IO ()
logLastSyncStats tracer stats = do
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
