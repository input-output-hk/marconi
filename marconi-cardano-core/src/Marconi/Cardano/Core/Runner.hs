{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Allow the execution of indexers on a Cardano node using the chain sync protocol
module Marconi.Cardano.Core.Runner (
  -- * Runner
  runChainSyncIndexer,
  runSnapshotIndexer,

  -- ** Runner Config
  RunIndexerConfig (RunIndexerConfig),
  runIndexerPreprocessEvent,
  runIndexerExtractBlockNo,
  runIndexerExtractTipDistance,
  RunIndexerEventPreprocessing (RunIndexerEventPreprocessing),
  runIndexerConfigTrace,
  runIndexerConfigEventProcessing,
  runIndexerConfigRetryConfig,
  runIndexerConfigSecurityParam,
  runIndexerConfigNetworkId,
  runIndexerConfigChainPoint,
  runIndexerConfigSocketPath,
  RunSnapshotIndexerConfig (RunSnapshotIndexerConfig),

  -- * Process chainSync events
  withNoPreprocessor,
  withDistancePreprocessor,
  withDistanceAndTipPreprocessor,
  withNoPreprocessorOnSnapshot,
  withDistancePreprocessorOnSnapshot,
  withDistanceAndTipPreprocessorOnSnapshot,

  -- * Event types
) where

import Cardano.Api.Extended qualified as C
import Cardano.Api.Extended.Streaming (
  BlockEvent (BlockEvent),
  ChainSyncEvent (RollBackward, RollForward),
  ChainSyncEventException (NoIntersectionFound),
  withChainSyncBlockEventStream,
 )
import Cardano.BM.Trace qualified as Trace
import Control.Concurrent qualified as Concurrent
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM qualified as STM
import Control.Exception (catch)
import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad.Except (ExceptT)
import Control.Monad.State.Strict (MonadState (put), State, gets)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map qualified as Map
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance, getEvent)
import Marconi.Cardano.Core.Extract.WithDistance qualified as Distance
import Marconi.Cardano.Core.Logger ()
import Marconi.Cardano.Core.Node.Client.Retry (withNodeConnectRetry)
import Marconi.Cardano.Core.Orphans qualified ()
import Marconi.Cardano.Core.Types (
  BlockEvent (blockInMode),
  MarconiTrace,
  RetryConfig,
  SecurityParam,
  TipAndBlock (TipAndBlock),
 )
import Marconi.Core qualified as Core
import Prettyprinter (pretty)
import Prettyprinter qualified as PP
import Streaming qualified as S
import Streaming.Prelude qualified as S

-- | Runner pre-processing
data RunIndexerEventPreprocessing rawEvent event = RunIndexerEventPreprocessing
  { _runIndexerPreprocessEvent :: rawEvent -> [Core.ProcessedInput C.ChainPoint event]
  , _runIndexerExtractBlockNo :: event -> Maybe C.BlockNo
  , _runIndexerExtractTipDistance :: event -> Maybe Word
  }

Lens.makeLenses ''RunIndexerEventPreprocessing

-- | Common configuration required to run indexers
data RunIndexerConfig rawEvent event = RunIndexerConfig
  { _runIndexerConfigTrace :: MarconiTrace IO
  , _runIndexerConfigEventProcessing :: RunIndexerEventPreprocessing rawEvent event
  , _runIndexerConfigRetryConfig :: RetryConfig
  , _runIndexerConfigSecurityParam :: SecurityParam
  , _runIndexerConfigNetworkId :: C.NetworkId
  , _runIndexerConfigChainPoint :: C.ChainPoint
  , _runIndexerConfigSocketPath :: FilePath
  }

Lens.makeLenses ''RunIndexerConfig

data RunSnapshotIndexerConfig rawEvent event = RunSnapshotIndexerConfig
  { _runSnapshotIndexerConfigEventProcessing :: RunIndexerEventPreprocessing rawEvent event
  , _runSnapshotIndexerConfigSecurityParam :: SecurityParam
  }

Lens.makeLenses ''RunSnapshotIndexerConfig

-- | Wraps as a datatype log message emitted by the 'runIndexer' et al. functions.
data RunIndexerLog
  = -- | The last sync points of all indexers that will be used to start the chain-sync protocol.
    StartingPointLog C.ChainPoint
  | NoIntersectionFoundLog

instance PP.Pretty RunIndexerLog where
  pretty (StartingPointLog lastSyncPoint) =
    "The starting point for the chain-sync protocol is" PP.<+> PP.pretty lastSyncPoint
  pretty NoIntersectionFoundLog = "No intersection found"

type instance Core.Point BlockEvent = C.ChainPoint

{- | Connect to the given socket to start a chain sync protocol and start indexing it with the
given indexer.
-}
runChainSyncIndexer
  :: ( Core.IsIndex (ExceptT Core.IndexerError IO) event indexer
     , Core.Closeable IO indexer
     , Core.Point event ~ C.ChainPoint
     )
  => RunIndexerConfig (ChainSyncEvent BlockEvent) event
  -> indexer event
  -> IO ()
runChainSyncIndexer config indexer = do
  _ <- runEmitterAndConsumer securityParam eventPreprocessing (chainSyncEventEmitter config indexer)
  return ()
  where
    securityParam = Lens.view runIndexerConfigSecurityParam config
    eventPreprocessing = Lens.view runIndexerConfigEventProcessing config

runSnapshotIndexer
  :: ( Core.IsIndex (ExceptT Core.IndexerError IO) event indexer
     , Core.Closeable IO indexer
     , Core.Point event ~ C.ChainPoint
     )
  => RunSnapshotIndexerConfig BlockEvent event
  -> indexer event
  -> S.Stream (S.Of BlockEvent) IO ()
  -> IO (Concurrent.MVar (indexer event))
runSnapshotIndexer config indexer stream =
  runEmitterAndConsumer
    securityParam
    eventPreprocessing
    (streamBlockEventEmitter config indexer stream)
  where
    securityParam = Lens.view runSnapshotIndexerConfigSecurityParam config
    eventPreprocessing = Lens.view runSnapshotIndexerConfigEventProcessing config

data EventEmitter indexer event a = EventEmitter
  { queue :: STM.TBQueue (Core.ProcessedInput (Core.Point event) event)
  , indexerMVar :: Concurrent.MVar (indexer event)
  , emitEvents :: IO a
  }

runEmitterAndConsumer
  :: ( Core.Point event ~ C.ChainPoint
     , -- , Ord (Core.Point event)
       Core.IsIndex (ExceptT Core.IndexerError IO) event indexer
     , Core.Closeable IO indexer
     )
  => SecurityParam
  -> RunIndexerEventPreprocessing rawEvent event
  -> IO (EventEmitter indexer event a)
  -> IO (Concurrent.MVar (indexer event))
runEmitterAndConsumer
  securityParam
  eventPreprocessing
  eventEmitter =
    do
      EventEmitter{queue, indexerMVar, emitEvents} <- eventEmitter
      emitEvents
        `race_` Core.processQueue
          (stablePointComputation securityParam eventPreprocessing)
          Map.empty
          queue
          indexerMVar
      return indexerMVar

chainSyncEventEmitter
  :: (Core.Point event ~ C.ChainPoint)
  => RunIndexerConfig (ChainSyncEvent BlockEvent) event
  -> indexer event
  -> IO (EventEmitter indexer event ())
chainSyncEventEmitter
  ( RunIndexerConfig
      trace
      eventProcessing
      retryConfig
      securityParam
      networkId
      startingPoint
      socketPath
    )
  indexer =
    withNodeConnectRetry trace retryConfig socketPath $ do
      Trace.logInfo trace $
        PP.pretty $
          StartingPointLog startingPoint
      eventQueue <- STM.newTBQueueIO $ fromIntegral securityParam
      cBox <- Concurrent.newMVar indexer
      let processEvent = eventProcessing ^. runIndexerPreprocessEvent
          runChainSyncStream =
            withChainSyncBlockEventStream
              socketPath
              networkId
              [startingPoint]
              (mkEventStream processEvent eventQueue)
          whenNoIntersectionFound NoIntersectionFound =
            Trace.logError trace $
              PP.pretty NoIntersectionFoundLog
          eventEmitter =
            withNodeConnectRetry trace retryConfig socketPath $
              runChainSyncStream `catch` whenNoIntersectionFound
      return (EventEmitter eventQueue cBox eventEmitter)

streamBlockEventEmitter
  :: (Core.Point event ~ C.ChainPoint)
  => RunSnapshotIndexerConfig BlockEvent event
  -> indexer event
  -> S.Stream (S.Of BlockEvent) IO ()
  -> IO (EventEmitter indexer event ())
streamBlockEventEmitter config indexer stream = do
  queue <- STM.newTBQueueIO $ fromIntegral securityParam
  indexerMVar <- Concurrent.newMVar indexer
  let processEvent = eventProcessing ^. runIndexerPreprocessEvent
      emitEvents = mkEventStream processEvent queue stream
  return EventEmitter{queue, indexerMVar, emitEvents}
  where
    securityParam = Lens.view runSnapshotIndexerConfigSecurityParam config
    eventProcessing = Lens.view runSnapshotIndexerConfigEventProcessing config

stablePointComputation
  :: SecurityParam
  -> RunIndexerEventPreprocessing rawEvent event
  -> Core.Timed C.ChainPoint (Maybe event)
  -> State (Map C.BlockNo C.ChainPoint) (Maybe C.ChainPoint)
stablePointComputation securityParam preprocessing (Core.Timed point event) = do
  let distanceM = preprocessing ^. runIndexerExtractTipDistance =<< event
      blockNoM = preprocessing ^. runIndexerExtractBlockNo =<< event
  case (distanceM, blockNoM) of
    (Just distance, Just blockNo) ->
      if distance > fromIntegral securityParam
        then do
          put mempty
          pure $ Just point
        else do
          let lastVolatileBlock = blockNo + fromIntegral distance - fromIntegral securityParam
          (immutable, volatile) <- gets (Map.spanAntitone (< lastVolatileBlock))
          put (Map.insert blockNo point volatile)
          pure $ case Map.elems immutable of
            [] -> Nothing
            xs -> Just $ last xs
    _otherCases -> pure Nothing

getBlockNo :: C.BlockInMode C.CardanoMode -> C.BlockNo
getBlockNo (C.BlockInMode block _eraInMode) =
  case C.getBlockHeader block of C.BlockHeader _ _ b -> b

-- | Event preprocessing, to ease the coordinator work
mkEventStream
  :: (inputEvent -> [Core.ProcessedInput (Core.Point inputEvent) outputEvent])
  -> STM.TBQueue (Core.ProcessedInput (Core.Point inputEvent) outputEvent)
  -> S.Stream (S.Of inputEvent) IO r
  -> IO r
mkEventStream processEvent q =
  S.mapM_ $ STM.atomically . traverse_ (STM.writeTBQueue q) . processEvent

withDistanceAndTipPreprocessor
  :: RunIndexerEventPreprocessing (ChainSyncEvent BlockEvent) TipAndBlock
withDistanceAndTipPreprocessor =
  let extractChainTipAndAddDistance
        :: ChainSyncEvent BlockEvent
        -> [Core.ProcessedInput C.ChainPoint TipAndBlock]
      extractChainTipAndAddDistance (RollForward x tip) =
        let point = blockEventPoint x
            blockWithDistance
              :: BlockEvent
              -> WithDistance BlockEvent
            blockWithDistance (BlockEvent b@(C.BlockInMode block _) epochNo' bt) =
              let (C.Block (C.BlockHeader _slotNo _hsh currentBlockNo) _) = block
                  withDistance = Distance.attachDistance currentBlockNo tip (BlockEvent b epochNo' bt)
               in withDistance
         in [Core.Index $ Core.Timed point $ Just $ TipAndBlock tip $ Just $ blockWithDistance x]
      extractChainTipAndAddDistance (RollBackward x tip) =
        [ Core.Rollback x
        , Core.Index $ Core.Timed x $ Just $ TipAndBlock tip Nothing
        ]
      getDistance (TipAndBlock _ (Just event)) = Just . fromIntegral $ Distance.chainDistance event
      getDistance _ = Nothing
      blockNoFromBlockEvent (TipAndBlock _ (Just event)) = Just . getBlockNo . blockInMode $ getEvent event
      blockNoFromBlockEvent _ = Nothing
   in RunIndexerEventPreprocessing extractChainTipAndAddDistance blockNoFromBlockEvent getDistance

withDistanceAndTipPreprocessorOnSnapshot
  :: C.ChainTip -> RunIndexerEventPreprocessing BlockEvent TipAndBlock
withDistanceAndTipPreprocessorOnSnapshot tip =
  let extractChainTipAndAddDistance
        :: BlockEvent
        -> [Core.ProcessedInput C.ChainPoint TipAndBlock]
      extractChainTipAndAddDistance x =
        let point = blockEventPoint x
            blockWithDistance
              :: BlockEvent
              -> WithDistance BlockEvent
            blockWithDistance (BlockEvent b@(C.BlockInMode block _) epochNo' bt) =
              let (C.Block (C.BlockHeader _slotNo _hsh currentBlockNo) _) = block
                  withDistance = Distance.attachDistance currentBlockNo tip (BlockEvent b epochNo' bt)
               in withDistance
         in [Core.Index $ Core.Timed point $ Just $ TipAndBlock tip $ Just $ blockWithDistance x]
      getDistance (TipAndBlock _ (Just event)) = Just . fromIntegral $ Distance.chainDistance event
      getDistance _ = Nothing
      blockNoFromBlockEvent (TipAndBlock _ (Just event)) = Just . getBlockNo . blockInMode $ getEvent event
      blockNoFromBlockEvent _ = Nothing
   in RunIndexerEventPreprocessing extractChainTipAndAddDistance blockNoFromBlockEvent getDistance

withNoPreprocessor :: RunIndexerEventPreprocessing (ChainSyncEvent BlockEvent) BlockEvent
withNoPreprocessor =
  let eventToProcessedInput
        :: ChainSyncEvent BlockEvent
        -> [Core.ProcessedInput C.ChainPoint BlockEvent]
      eventToProcessedInput (RollForward event _) =
        let point = blockEventPoint event
            timedEvent = Core.Timed point event
         in [Core.Index $ Just <$> timedEvent]
      eventToProcessedInput (RollBackward point _tip) = [Core.Rollback point]
      blockNoFromBlockEvent = Just . getBlockNo . blockInMode
   in RunIndexerEventPreprocessing eventToProcessedInput blockNoFromBlockEvent (const Nothing)

withNoPreprocessorOnSnapshot :: RunIndexerEventPreprocessing BlockEvent BlockEvent
withNoPreprocessorOnSnapshot =
  let eventToProcessedInput
        :: BlockEvent
        -> [Core.ProcessedInput C.ChainPoint BlockEvent]
      eventToProcessedInput event =
        let point = blockEventPoint event
            timedEvent = Core.Timed point event
         in [Core.Index $ Just <$> timedEvent]
      blockNoFromBlockEvent = Just . getBlockNo . blockInMode
   in RunIndexerEventPreprocessing eventToProcessedInput blockNoFromBlockEvent (const Nothing)

withDistancePreprocessorOnSnapshot
  :: C.ChainTip
  -> RunIndexerEventPreprocessing BlockEvent (WithDistance BlockEvent)
withDistancePreprocessorOnSnapshot tip =
  let addDistance
        :: BlockEvent
        -> [Core.ProcessedInput C.ChainPoint (WithDistance BlockEvent)]
      addDistance x =
        let point = blockEventPoint x
            blockWithDistance
              :: BlockEvent
              -> Core.Timed C.ChainPoint (WithDistance BlockEvent)
            blockWithDistance (BlockEvent b@(C.BlockInMode block _) epochNo' bt) =
              let (C.Block (C.BlockHeader _slotNo _hsh currentBlockNo) _) = block
                  withDistance = Distance.attachDistance currentBlockNo tip (BlockEvent b epochNo' bt)
               in Core.Timed point withDistance
         in [Core.Index $ Just <$> blockWithDistance x]
      getDistance = Just . fromIntegral . Distance.chainDistance
      blockNoFromBlockEvent = Just . getBlockNo . blockInMode . getEvent
   in RunIndexerEventPreprocessing addDistance blockNoFromBlockEvent getDistance

withDistancePreprocessor
  :: RunIndexerEventPreprocessing (ChainSyncEvent BlockEvent) (WithDistance BlockEvent)
withDistancePreprocessor =
  let addDistance
        :: ChainSyncEvent BlockEvent
        -> [Core.ProcessedInput C.ChainPoint (WithDistance BlockEvent)]
      addDistance (RollForward x tip) =
        let point = blockEventPoint x
            blockWithDistance
              :: BlockEvent
              -> Core.Timed C.ChainPoint (WithDistance BlockEvent)
            blockWithDistance (BlockEvent b@(C.BlockInMode block _) epochNo' bt) =
              let (C.Block (C.BlockHeader _slotNo _hsh currentBlockNo) _) = block
                  withDistance = Distance.attachDistance currentBlockNo tip (BlockEvent b epochNo' bt)
               in Core.Timed point withDistance
         in [Core.Index $ Just <$> blockWithDistance x]
      addDistance (RollBackward x _tip) = [Core.Rollback x]
      getDistance = Just . fromIntegral . Distance.chainDistance
      blockNoFromBlockEvent = Just . getBlockNo . blockInMode . getEvent
   in RunIndexerEventPreprocessing addDistance blockNoFromBlockEvent getDistance

blockEventPoint :: BlockEvent -> C.ChainPoint
blockEventPoint (BlockEvent (C.BlockInMode block _) _epochNo' _bt) =
  let (C.Block (C.BlockHeader slotNo hsh _currentBlockNo) _) = block
   in C.ChainPoint slotNo hsh
