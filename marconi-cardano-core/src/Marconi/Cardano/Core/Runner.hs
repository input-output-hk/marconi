{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Allow the execution of indexers on a Cardano node using the chain sync protocol
module Marconi.Cardano.Core.Runner (
  -- * Runner
  runIndexer,

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

  -- * Process chainSync events
  withNoPreprocessor,
  withDistancePreprocessor,
  withDistanceAndTipPreprocessor,

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
import Control.Concurrent.STM (TBQueue)
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
data RunIndexerEventPreprocessing event1 event2 = RunIndexerEventPreprocessing
  { _runIndexerPreprocessEvent :: event1 -> [Core.ProcessedInput C.ChainPoint event2]
  , _runIndexerExtractBlockNo :: event2 -> Maybe C.BlockNo
  , _runIndexerExtractTipDistance :: event2 -> Maybe Word
  }

Lens.makeLenses ''RunIndexerEventPreprocessing

-- | Common configuration required to run indexers
data RunIndexerConfig event1 event2 = RunIndexerConfig
  { _runIndexerConfigTrace :: MarconiTrace IO
  , _runIndexerConfigEventProcessing :: RunIndexerEventPreprocessing event1 event2
  , _runIndexerConfigRetryConfig :: RetryConfig
  , _runIndexerConfigSecurityParam :: SecurityParam
  , _runIndexerConfigNetworkId :: C.NetworkId
  , _runIndexerConfigChainPoint :: C.ChainPoint
  , _runIndexerConfigSocketPath :: FilePath
  }

Lens.makeLenses ''RunIndexerConfig

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

If you want to start several indexers, use @runIndexers@.
-}
runIndexer
  :: ( Core.IsIndex (ExceptT Core.IndexerError IO) event2 indexer
     , Core.Closeable IO indexer
     , Core.Point event2 ~ C.ChainPoint
     )
  => RunIndexerConfig event1 event2
  -> indexer event2
  -> ((event1 -> [Core.ProcessedInput C.ChainPoint event2]) -> IO ())
  -> IO ()
runIndexer
  ( RunIndexerConfig
      trace
      eventProcessing
      retryConfig
      securityParam
      _networkId
      startingPoint
      socketPath
    )
  indexer
  streamRunner = do
    withNodeConnectRetry trace retryConfig socketPath $ do
      Trace.logInfo trace $
        PP.pretty $
          StartingPointLog startingPoint
      eventQueue <- STM.newTBQueueIO $ fromIntegral securityParam
      cBox <- Concurrent.newMVar indexer
      let processEvent = eventProcessing ^. runIndexerPreprocessEvent
          whenNoIntersectionFound NoIntersectionFound =
            Trace.logError trace $
              PP.pretty NoIntersectionFoundLog
      void $ Concurrent.forkIO $ streamRunner processEvent `catch` whenNoIntersectionFound
      Core.processQueue
        -- TODO: this is not ok, we need to assume that the chain is stable
        (stablePointComputation securityParam eventProcessing)
        Map.empty
        eventQueue
        cBox

runChainSyncStream socketPath networkId startingPoint eventQueue processEvent =
  withChainSyncBlockEventStream
    socketPath
    networkId
    [startingPoint]
    (mkEventStream processEvent eventQueue)

stablePointComputation
  :: SecurityParam
  -> RunIndexerEventPreprocessing event1 event2
  -> Core.Timed C.ChainPoint (Maybe event2)
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
  :: (event -> [Core.ProcessedInput C.ChainPoint a])
  -> STM.TBQueue (Core.ProcessedInput C.ChainPoint a)
  -> S.Stream (S.Of event) IO r
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
