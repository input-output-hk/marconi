{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Allow the execution of indexers on a Cardano node using the chain sync protocol
module Marconi.Cardano.Core.Runner (
  -- * Runner
  runIndexerOnChainSync,
  runIndexerOnSnapshot,
  runEmitterAndConsumer,

  -- ** Runner Config
  RunIndexerConfig (RunIndexerConfig),
  runIndexerPreprocessEvent,
  runIndexerExtractBlockNo,
  runIndexerExtractTipDistance,
  RunIndexerEventPreprocessing (RunIndexerEventPreprocessing),
  RunIndexerEventPreprocessingPure,
  runIndexerConfigTrace,
  runIndexerConfigEventProcessing,
  runIndexerConfigRetryConfig,
  runIndexerConfigSecurityParam,
  runIndexerConfigNetworkId,
  runIndexerConfigChainPoint,
  runIndexerConfigSocketPath,
  RunIndexerOnSnapshotConfig (RunIndexerOnSnapshotConfig),

  -- * Process chainSync events
  withNoPreprocessor,
  withDistancePreprocessor,
  withDistanceAndTipPreprocessor,
  withNoPreprocessorOnSnapshot,

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
import Control.Monad (void)
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
data RunIndexerEventPreprocessing m rawEvent event = RunIndexerEventPreprocessing
  { _runIndexerPreprocessEvent :: rawEvent -> m (Core.ProcessedInput C.ChainPoint event)
  , _runIndexerExtractBlockNo :: event -> Maybe C.BlockNo
  , _runIndexerExtractTipDistance :: event -> Maybe Word
  }

type RunIndexerEventPreprocessingPure rawEvent event =
  RunIndexerEventPreprocessing [] rawEvent event

Lens.makeLenses ''RunIndexerEventPreprocessing

-- | Common configuration required to run indexers
data RunIndexerConfig rawEvent event = RunIndexerConfig
  { _runIndexerConfigTrace :: MarconiTrace IO
  , _runIndexerConfigEventProcessing :: RunIndexerEventPreprocessingPure rawEvent event
  , _runIndexerConfigRetryConfig :: RetryConfig
  , _runIndexerConfigSecurityParam :: SecurityParam
  , _runIndexerConfigNetworkId :: C.NetworkId
  , _runIndexerConfigChainPoint :: C.ChainPoint
  , _runIndexerConfigSocketPath :: FilePath
  }

Lens.makeLenses ''RunIndexerConfig

data RunIndexerOnSnapshotConfig rawEvent event = RunIndexerOnSnapshotConfig
  { _runIndexerOnSnapshotConfigEventProcessing :: RunIndexerEventPreprocessingPure rawEvent event
  , _runIndexerOnSnapshotConfigSecurityParam :: SecurityParam
  }

Lens.makeLenses ''RunIndexerOnSnapshotConfig

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
runIndexerOnChainSync
  :: ( Core.IsIndex (ExceptT Core.IndexerError IO) event indexer
     , Core.Closeable IO indexer
     , Core.Point event ~ C.ChainPoint
     )
  => RunIndexerConfig (ChainSyncEvent BlockEvent) event
  -> indexer event
  -> IO ()
runIndexerOnChainSync config indexer = do
  void $
    runEmitterAndConsumer
      securityParam
      eventPreprocessing
      (chainSyncEventEmitter config indexer)
  where
    securityParam = Lens.view runIndexerConfigSecurityParam config
    eventPreprocessing = Lens.view runIndexerConfigEventProcessing config

{- | Run an indexer directly from a stream of 'BlockEvent's. Useful for running indexers from
"snapshots" of the blockchain, i.e. serialised parts of the chain which were stored on disk.
-}
runIndexerOnSnapshot
  :: ( Core.IsIndex (ExceptT Core.IndexerError IO) event indexer
     , Core.Closeable IO indexer
     , Core.Point event ~ C.ChainPoint
     )
  => RunIndexerOnSnapshotConfig BlockEvent event
  -> indexer event
  -> S.Stream (S.Of BlockEvent) IO ()
  -> IO (Concurrent.MVar (indexer event))
runIndexerOnSnapshot config indexer stream =
  runEmitterAndConsumer
    securityParam
    eventPreprocessing
    (streamBlockEventEmitter config indexer stream)
  where
    securityParam = Lens.view runIndexerOnSnapshotConfigSecurityParam config
    eventPreprocessing = Lens.view runIndexerOnSnapshotConfigEventProcessing config

-- | The result of an asynchronous procedure which emits events to be consumed.
data EventEmitter indexer event a = EventEmitter
  { queue :: STM.TBQueue (Core.ProcessedInput (Core.Point event) event)
  , indexerMVar :: Concurrent.MVar (indexer event)
  , emitEvents :: IO a
  }

{- | Races two threads, one which emits events and the other which consumes them.
The indexer receives the consumed events. Returns the 'MVar' in which the indexer
resides, for inspection.
-}
runEmitterAndConsumer
  :: ( Core.Point event ~ C.ChainPoint
     , Core.IsIndex (ExceptT Core.IndexerError IO) event indexer
     , Core.Closeable IO indexer
     )
  => SecurityParam
  -> RunIndexerEventPreprocessingPure rawEvent event
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
      pure indexerMVar

-- | Emits events from a local running Cardano node via the chain sync protocol.
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
      pure (EventEmitter eventQueue cBox eventEmitter)

{- | Emits events from a stream. There is a level of indirection here, since
instead of consuming the stream a caller will consume the created queue.
The reason behind this is to provide the same interface as 'chainSyncEventEmitter'.
-}
streamBlockEventEmitter
  :: (Core.Point event ~ C.ChainPoint)
  => RunIndexerOnSnapshotConfig BlockEvent event
  -> indexer event
  -> S.Stream (S.Of BlockEvent) IO ()
  -> IO (EventEmitter indexer event ())
streamBlockEventEmitter config indexer stream = do
  queue <- STM.newTBQueueIO $ fromIntegral securityParam
  indexerMVar <- Concurrent.newMVar indexer
  let processEvent = eventProcessing ^. runIndexerPreprocessEvent
      emitEvents = mkEventStream processEvent queue stream
  pure EventEmitter{queue, indexerMVar, emitEvents}
  where
    securityParam = Lens.view runIndexerOnSnapshotConfigSecurityParam config
    eventProcessing = Lens.view runIndexerOnSnapshotConfigEventProcessing config

stablePointComputation
  :: SecurityParam
  -> RunIndexerEventPreprocessingPure rawEvent event
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
  :: RunIndexerEventPreprocessingPure (ChainSyncEvent BlockEvent) TipAndBlock
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

withNoPreprocessor :: RunIndexerEventPreprocessingPure (ChainSyncEvent BlockEvent) BlockEvent
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

withNoPreprocessorOnSnapshot :: RunIndexerEventPreprocessingPure BlockEvent BlockEvent
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

withDistancePreprocessor
  :: RunIndexerEventPreprocessingPure (ChainSyncEvent BlockEvent) (WithDistance BlockEvent)
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
