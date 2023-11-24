{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | A coordinator that also maintains the @ExtLedgerState@.
module Marconi.ChainIndex.Indexers.ExtLedgerStateCoordinator (
  -- * Types
  ExtLedgerState,
  ExtLedgerStateEvent (..),
  ExtLedgerConfig,
  ExtLedgerStateCoordinator (ExtLedgerStateCoordinator),
  ExtLedgerStateCoordinatorConfig (ExtLedgerStateCoordinatorConfig),
  ExtLedgerStateWorkerConfig (..),

  -- * Constructors
  mkExtLedgerStateCoordinator,
  extLedgerStateWorker,

  -- * Utils
  newEpochPreprocessor,
  readGenesisFile,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended qualified as CE
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Trace qualified as BM
import Cardano.BM.Tracing qualified as BM
import Cardano.Ledger.Shelley.API qualified as Ledger
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Arrow ((<<<))
import Control.Concurrent qualified as Con
import Control.Exception (throw)
import Control.Lens (Lens', (%~), (&), (-~), (.=), (.~), (^.))
import Control.Lens qualified as Lens
import Control.Monad (foldM, (<=<))
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadTrans (lift), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Strict (MonadState (put), gets)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Short qualified as BS.Short
import Data.Data (Proxy (Proxy))
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance, chainDistance)
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Types (BlockEvent, SecurityParam (SecurityParam), blockInMode)
import Marconi.Core (IsSync (lastStablePoint))
import Marconi.Core qualified as Core
import Marconi.Core.Preprocessor qualified as Core
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Config qualified as O
import Ouroboros.Consensus.Ledger.Extended qualified as O
import Ouroboros.Consensus.Node.NetworkProtocolVersion qualified as O
import Ouroboros.Consensus.Node.Serialisation qualified as O
import Ouroboros.Consensus.Shelley.Ledger qualified as O
import Ouroboros.Consensus.Storage.Serialisation qualified as O
import Prettyprinter (Pretty (pretty))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Read qualified as Text

type ExtLedgerState = O.ExtLedgerState (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
type ExtLedgerConfig = O.ExtLedgerCfg (O.HardForkBlock (O.CardanoEras O.StandardCrypto))

type instance Core.Point ExtLedgerState = C.ChainPoint
type instance Core.Point (C.BlockInMode C.CardanoMode) = C.ChainPoint

data ExtLedgerStateCoordinatorConfig input = ExtLedgerStateCoordinatorConfig
  { _configExtractBlockEvent :: input -> BlockEvent
  -- ^ extract block event from the input
  , _configGenesisConfig :: C.GenesisConfig
  -- ^ used to bootstrap the ledger state
  , _configSnapshotInterval :: Word
  -- ^ how often we save the ledger state saving often is expensive but make rollbacks faster
  , _configSecurityParam :: SecurityParam
  -- ^ we use the security param to decide when we trigger the saves, on stable data, we save at
  -- each epoch only
  }

Lens.makeLenses ''ExtLedgerStateCoordinatorConfig

-- | Base event used to store the 'ExtLedgerState'
data ExtLedgerStateEvent = ExtLedgerStateEvent {extLedgerState :: ExtLedgerState, blockNo :: C.BlockNo}

type instance Core.Point ExtLedgerStateEvent = C.ChainPoint

data ExtLedgerStateCoordinatorState = ExtLedgerStateCoordinatorState
  { _stateBlocksToNextSnapshot :: Word
  -- ^ Number of block before the next snapshot
  , _stateEpochNo :: Maybe C.EpochNo
  -- ^ LastEpochNo saved
  , _stateSnapshots :: [C.ChainPoint]
  -- ^ Track the previous snapshot, used for resuming
  }

Lens.makeLenses ''ExtLedgerStateCoordinatorState

-- | Metadata used to create 'ExtLedgerStateEventIndexer' filenames
data EpochMetadata = EpochMetadata
  { metadataBlockNo :: Maybe C.BlockNo
  , metadataChainpoint :: C.ChainPoint
  }
  deriving (Show)

type ExtLedgerStateFileIndexer = Core.FileIndexer EpochMetadata ExtLedgerStateEvent

type BlockFileIndexer = Core.FileIndexer EpochMetadata (C.BlockInMode C.CardanoMode)

data ExtLedgerStateCoordinator input = ExtLedgerStateCoordinator
  { _extLedgerStateCoordinatorConfig :: ExtLedgerStateCoordinatorConfig input
  -- ^ The configuration of the coordinator
  , _extLedgerStateCoordinatorState :: ExtLedgerStateCoordinatorState
  -- ^ The current state of the coordinator
  , _extLedgerStateIndexer :: ExtLedgerStateFileIndexer
  -- ^ The indexer that manage the ledger state snapshot to handle backup and resuming
  , _blockIndexer :: BlockFileIndexer
  -- ^ The indexer that serialise blocks to handle resuming
  , _coordinator :: Core.Coordinator input
  -- ^ A standard coordinator, which handles the workers
  }

Lens.makeLenses ''ExtLedgerStateCoordinator

-- | The state maintained by the indexer to decide how to handle incoming events
data WorkerState input = WorkerState
  { _lastLedgerState :: ExtLedgerStateEvent
  -- ^ The last computed ledger state
  , _accessToIndexer
      :: Con.MVar
          ( Core.WithTrace
              IO
              ExtLedgerStateCoordinator
              (ExtLedgerStateEvent, WithDistance input)
          )
  -- ^ allow query to the indexer
  }

Lens.makeLenses ''WorkerState

genesisConfig :: Lens.Getter (ExtLedgerStateCoordinator input) C.GenesisConfig
genesisConfig = extLedgerStateCoordinatorConfig . configGenesisConfig

extLedgerConfig :: Lens.Getter (ExtLedgerStateCoordinator input) ExtLedgerConfig
extLedgerConfig = genesisConfig . Lens.to CE.mkExtLedgerConfig

snapshotInterval :: Lens.Getter (ExtLedgerStateCoordinator input) Word
snapshotInterval = extLedgerStateCoordinatorConfig . configSnapshotInterval

securityParam :: Lens.Getter (ExtLedgerStateCoordinator input) SecurityParam
securityParam = extLedgerStateCoordinatorConfig . configSecurityParam

extractBlockEvent :: Lens.Getter (ExtLedgerStateCoordinator input) (input -> BlockEvent)
extractBlockEvent = extLedgerStateCoordinatorConfig . configExtractBlockEvent

blocksToNextSnapshot :: Lens' (ExtLedgerStateCoordinator input) Word
blocksToNextSnapshot = extLedgerStateCoordinatorState . stateBlocksToNextSnapshot

snapshots :: Lens' (ExtLedgerStateCoordinator input) [C.ChainPoint]
snapshots = extLedgerStateCoordinatorState . stateSnapshots

lastEpochNo :: Lens' (ExtLedgerStateCoordinator input) (Maybe C.EpochNo)
lastEpochNo = extLedgerStateCoordinatorState . stateEpochNo

mkExtLedgerStateCoordinator
  :: (MonadIO m, MonadError Core.IndexerError m, Core.Point (ExtLedgerStateEvent, input) ~ C.ChainPoint)
  => ExtLedgerStateCoordinatorConfig (ExtLedgerStateEvent, input)
  -> FilePath
  -> [Core.Worker (ExtLedgerStateEvent, input) C.ChainPoint]
  -> m (ExtLedgerStateCoordinator (ExtLedgerStateEvent, input))
mkExtLedgerStateCoordinator config rootDir workers = do
  let genesisCfg = config ^. configGenesisConfig
      securityParam' = config ^. configSecurityParam
      extLedgerCfg = CE.mkExtLedgerConfig genesisCfg
      configCodec = O.configCodec . O.getExtLedgerCfg $ extLedgerCfg
  liftIO $ createDirectoryIfMissing True rootDir
  epochStateIndexer' <-
    buildExtLedgerStateEventIndexer
      configCodec
      securityParam'
      (rootDir </> "epochState")
  epochBlocksIndexer <-
    buildBlockIndexer
      configCodec
      securityParam'
      (rootDir </> "epochBlocks")
  coordinator' <- liftIO $ Core.mkCoordinator workers
  let extLedgerStateCoordinatorState' =
        ExtLedgerStateCoordinatorState
          (config ^. configSnapshotInterval)
          Nothing
          []
  pure $
    ExtLedgerStateCoordinator
      config
      extLedgerStateCoordinatorState'
      epochStateIndexer'
      epochBlocksIndexer
      coordinator'

data ExtLedgerStateWorkerConfig m input = ExtLedgerStateWorkerConfig
  { workerEventExtractor :: input -> BlockEvent
  , workerLogger :: BM.Trace m Text
  , workerNodeConfigPath :: FilePath
  , workerSnapshotInterval :: Word
  , workerSecurityParam :: SecurityParam
  }

{-
  p <- max <$> Core.lastStablePoint epochStateIndexer' <*> Core.lastStablePoint epochBlocksIndexer
  ledgerState <-
    either
      (const $ throwError $ Core.IndexerInternalError "Can't resolve the last stable point")
      pure
      =<< runExceptT (restoreLedgerStateFromIndexers genesisCfg p epochStateIndexer' epochBlocksIndexer)
-}

extLedgerStateWorker
  :: forall m input
   . ( MonadIO m
     , MonadError Core.IndexerError m
     , Core.Point (ExtLedgerStateEvent, WithDistance input) ~ C.ChainPoint
     )
  => ExtLedgerStateWorkerConfig IO (WithDistance input)
  -> [Core.Worker (ExtLedgerStateEvent, WithDistance input) C.ChainPoint]
  -> FilePath
  -> m
      ( Core.WorkerIndexer
          IO
          (WithDistance input)
          (ExtLedgerStateEvent, WithDistance input)
          (Core.WithTrace IO ExtLedgerStateCoordinator)
      )
extLedgerStateWorker config workers path = do
  genesisCfg <- readGenesisFile $ workerNodeConfigPath config
  let extLedgerCfg = CE.mkExtLedgerConfig genesisCfg
      extract = workerEventExtractor config
      indexerName = "ExtLedgerStateEvent"
      indexerEventLogger = BM.contramap (fmap $ fmap $ Text.pack . show) $ workerLogger config
      cfg =
        ExtLedgerStateCoordinatorConfig
          (extract . snd)
          genesisCfg
          (workerSnapshotInterval config)
          (workerSecurityParam config)

      mapOneEvent
        :: (MonadState (WorkerState input) n, MonadError Core.IndexerError n, MonadIO n)
        => Maybe (WithDistance input)
        -> n (Maybe (ExtLedgerStateEvent, WithDistance input))
      mapOneEvent Nothing = pure Nothing
      mapOneEvent (Just evt) = runMaybeT $ do
        let applyBlock = CE.applyBlockExtLedgerState extLedgerCfg C.QuickValidation
            block = extract evt
            blockInMode' = blockInMode block
        let newBlockNo = getBlockNo blockInMode'
        ExtLedgerStateEvent currentLedgerState' _block <- Lens.use lastLedgerState
        case applyBlock blockInMode' currentLedgerState' of
          Left err -> throwError . Core.IndexerInternalError . Text.pack . show $ err
          Right res -> do
            let ledgerStateEvent = ExtLedgerStateEvent res newBlockNo
            lastLedgerState .= ledgerStateEvent
            pure (ledgerStateEvent, evt)

      processAsEpochState
        :: ExceptT Core.IndexerError IO (WorkerState input)
        -> Core.Preprocessor
            (ExceptT Core.IndexerError IO)
            C.ChainPoint
            (WithDistance input)
            (ExtLedgerStateEvent, WithDistance input)
      processAsEpochState = do
        Core.preprocessorM $ \case
          Core.Index x -> pure . Core.Index <$> traverse mapOneEvent x
          Core.IndexAllDescending xs -> pure . Core.IndexAllDescending <$> traverse (traverse mapOneEvent) xs
          Core.Rollback p -> do
            lastIndexerM <- Lens.use accessToIndexer
            lastIndexer <- liftIO $ Con.readMVar lastIndexerM
            queryResult <- lift $ runExceptT $ Core.query p Core.EventAtQuery lastIndexer
            case queryResult of
              Left (Core.IndexerQueryError err) ->
                throwError $
                  Core.IndexerInternalError $
                    "Can't rollback to the given epoch:" <> Text.pack (show err)
              Left _err -> throwError $ Core.IndexerInternalError "Can't rollback to the given epoch"
              Right Nothing -> throwError $ Core.IndexerInternalError "Can't rollback to the given epoch: no event found"
              Right (Just res) -> do
                lastLedgerState .= res
                pure . pure $ Core.Rollback p
          Core.StableAt p -> pure . pure $ Core.StableAt p
          Core.Stop -> pure $ pure Core.Stop

  coordinator' <-
    Core.withTrace indexerEventLogger
      <$> mkExtLedgerStateCoordinator cfg (path </> "epochState") workers
  workerState <- liftIO $ Con.newMVar coordinator'
  lastStable <- Core.lastStablePoint coordinator'
  ledgerStateE <- runExceptT $ restoreLedgerState (Just lastStable) (coordinator' ^. Core.unwrap)
  extLedgerState' <- case ledgerStateE of
    Left _err -> throwError $ Core.IndexerInternalError "can't restore ledger state"
    Right res -> pure res
  let initialState = pure $ WorkerState extLedgerState' workerState
      eventPreprocessing = processAsEpochState initialState <<< Core.withResume lastStable
  pure $
    Core.WorkerIndexer workerState $
      Core.Worker indexerName workerState eventPreprocessing id

instance
  ( MonadIO m
  , MonadError Core.IndexerError m
  , Core.Point (ExtLedgerStateEvent, WithDistance input) ~ C.ChainPoint
  )
  => Core.IsIndex m (ExtLedgerStateEvent, WithDistance input) ExtLedgerStateCoordinator
  where
  index (Core.Timed _point Nothing) indexer = pure indexer
  index timedEvent@(Core.Timed point (Just input@(newExtLedgerStateEvent, evt))) indexer = do
    let indexer' = indexer & blocksToNextSnapshot -~ 1
        extract = indexer ^. extractBlockEvent
        block = extract input
        blockInMode' = blockInMode block
        snapshotTime = (indexer' ^. blocksToNextSnapshot) == 0
        thisEpochNo = getEpochNo (extLedgerState newExtLedgerStateEvent)
        isNewEpoch = thisEpochNo /= indexer ^. lastEpochNo
        isVolatile = SecurityParam (chainDistance evt) < (indexer' ^. securityParam)
        snapshotEpoch = (snapshotTime && isVolatile) || isNewEpoch
        resetOnSnapshot =
          if snapshotTime || isNewEpoch
            then
              (blocksToNextSnapshot .~ indexer' ^. snapshotInterval)
                . (lastEpochNo .~ thisEpochNo)
            else id
        snapshotLedgerState =
          if snapshotEpoch
            then
              fmap (snapshots %~ (point :))
                . Core.indexVia extLedgerStateIndexer (Core.Timed point $ Just newExtLedgerStateEvent)
            else pure
        snapshotBlock =
          if isVolatile
            then blockIndexer (Core.index $ Core.Timed point $ Just blockInMode')
            else pure
    indexer'' <-
      if snapshotTime || isNewEpoch
        then
          pure . resetOnSnapshot
            <=< snapshotLedgerState
            <=< snapshotBlock
            $ indexer'
        else pure indexer'
    Core.indexVia coordinator timedEvent indexer''

  rollback point =
    extLedgerStateIndexer (Core.rollback point)
      <=< blockIndexer (Core.rollback point)
      <=< coordinator (Core.rollback point)

  setLastStablePoint point indexer =
    let (volatile, immutable) = span (> point) $ indexer ^. snapshots
        p' = listToMaybe immutable
        indexer' = indexer & snapshots .~ volatile
        setStablePointOnIndexers p =
          Core.setLastStablePointVia extLedgerStateIndexer p
            <=< Core.setLastStablePointVia blockIndexer p
            <=< Core.setLastStablePointVia coordinator p
     in maybe pure setStablePointOnIndexers p' indexer'

-- | A preprocessor that filters in events when they are the first of their epoch
newEpochPreprocessor :: (Monad m) => (a -> C.EpochNo) -> Core.Preprocessor m C.ChainPoint a a
newEpochPreprocessor f =
  let filterNewEpoch x = do
        let newEpochNo = Just $ f x
        isNewEpoch <- gets (newEpochNo /=)
        if isNewEpoch
          then do
            put newEpochNo
            pure $ Just x
          else pure Nothing
   in Core.scanMaybeEvent filterNewEpoch Nothing

instance
  ( MonadIO m
  , MonadError Core.IndexerError m
  , Core.Point event ~ C.ChainPoint
  )
  => Core.IsSync m event ExtLedgerStateCoordinator
  where
  lastSyncPoint indexer = do
    lastBlock <- Core.lastSyncPointVia blockIndexer indexer
    lastLedgerState' <- Core.lastSyncPointVia extLedgerStateIndexer indexer
    lastCoordinator <- Core.lastSyncPointVia coordinator indexer
    pure $ minimum [lastBlock, lastLedgerState', lastCoordinator]
  lastStablePoint = Core.lastStablePointVia extLedgerStateIndexer

instance
  (MonadIO m, MonadError Core.IndexerError m)
  => Core.Closeable m ExtLedgerStateCoordinator
  where
  close indexer = do
    Core.closeVia blockIndexer indexer
    Core.closeVia extLedgerStateIndexer indexer
    Core.closeVia coordinator indexer

instance
  ( MonadIO m
  , MonadError (Core.QueryError (Core.EventAtQuery ExtLedgerStateEvent)) m
  , Core.Point event ~ C.ChainPoint
  , Core.Point (ExtLedgerStateEvent, event) ~ C.ChainPoint
  )
  => Core.Queryable
      m
      (ExtLedgerStateEvent, event)
      (Core.EventAtQuery ExtLedgerStateEvent)
      ExtLedgerStateCoordinator
  where
  query point = const $ fmap Just . restoreLedgerState (Just point)

-- Restoration of the LedgerState

restoreLedgerState
  :: ( MonadIO m
     , MonadError (Core.QueryError (Core.EventAtQuery ExtLedgerStateEvent)) m
     , Core.Point (ExtLedgerStateEvent, event) ~ C.ChainPoint
     , Core.Point event ~ C.ChainPoint
     )
  => Maybe C.ChainPoint
  -> ExtLedgerStateCoordinator (ExtLedgerStateEvent, event)
  -> m ExtLedgerStateEvent
restoreLedgerState p indexer = do
  let applyBlocksUpToGivenSlot epochStatePoint closestLedgerState = do
        blocks <- getBlocksFrom epochStatePoint p (indexer ^. blockIndexer)
        liftIO $
          foldM
            (buildNextExtLedgerStateEvent $ indexer ^. extLedgerConfig)
            closestLedgerState
            blocks
  Core.Timed epochStatePoint closestLedgerState <-
    getLatestNonEmpty
      p
      (Lens.views genesisConfig initialExtLedgerStateEvent indexer)
      (indexer ^. extLedgerStateIndexer)
  mlast <- runExceptT @Core.IndexerError $ Core.lastSyncPoint indexer
  last' <- either (const $ throwError $ Core.IndexerQueryError "can't find lastpoint") pure mlast
  if epochStatePoint == fromMaybe last' p
    then pure closestLedgerState
    else applyBlocksUpToGivenSlot epochStatePoint closestLedgerState

getLatestNonEmpty
  :: (MonadIO m, MonadError (Core.QueryError (Core.EventAtQuery ExtLedgerStateEvent)) m)
  => Maybe C.ChainPoint
  -> ExtLedgerStateEvent
  -> ExtLedgerStateFileIndexer
  -> m (Core.Timed C.ChainPoint ExtLedgerStateEvent)
getLatestNonEmpty p firstExtLedgerStateEvent indexer = do
  let query = maybe Core.queryLatest Core.query
      getLatest [] = Core.Timed Core.genesis firstExtLedgerStateEvent
      getLatest (x : _) = x
  result <- runExceptT $ query p Core.latestEvent indexer
  case result of
    Right xs -> pure $ getLatest xs
    Left (Core.AheadOfLastSync partialResult) -> case partialResult of
      Nothing ->
        throwError $
          Core.IndexerQueryError $
            "Cant resolve last epochState: No previous result: " <> Text.pack (show $ pretty p)
      Just xs -> pure $ getLatest xs
    Left (Core.IndexerQueryError err) ->
      throwError $
        Core.IndexerQueryError $
          "Cant resolve last epochState: " <> Text.pack (show err)
    Left Core.NotStoredAnymore ->
      throwError $
        Core.IndexerQueryError
          "Cant resolve last epochState: Not stored anymore"
    Left (Core.SlotNoBoundsInvalid _) ->
      throwError $ Core.IndexerQueryError "Invalid bounds"

getBlocksFrom
  :: (MonadIO m, MonadError (Core.QueryError (Core.EventAtQuery ExtLedgerStateEvent)) m)
  => C.ChainPoint
  -> Maybe C.ChainPoint
  -> BlockFileIndexer
  -> m [C.BlockInMode C.CardanoMode]
getBlocksFrom from to indexer = do
  let query = maybe Core.queryLatest Core.query
      extractResult = fmap $ Lens.view Core.event
  result <- runExceptT $ query to (Core.EventsFromQuery from) indexer
  case result of
    Right xs -> pure $ extractResult xs
    Left (Core.AheadOfLastSync partialResult) -> case partialResult of
      Nothing -> do
        lastSync <- Core.lastSyncPoint indexer
        throwError $
          Core.IndexerQueryError $
            "Cant resolve last blocks: No result - ahead of sync - No previous result: "
              <> Text.pack (show $ pretty to)
              <> " head is: "
              <> Text.pack (show $ pretty lastSync)
      Just xs -> do
        lastSync <- Core.lastSyncPoint indexer
        throwError $
          Core.IndexerQueryError $
            "Cant resolve last blocks: No result - ahead of sync - Latest results: "
              <> Text.pack (show (Lens.view Core.point <$> xs))
              <> " Head is: "
              <> Text.pack (show $ pretty lastSync)
              <> " Expecting: "
              <> Text.pack (show to)
    Left (Core.IndexerQueryError err) ->
      throwError $
        Core.IndexerQueryError $
          "Cant resolve last blocks: " <> Text.pack (show err)
    Left Core.NotStoredAnymore ->
      throwError $
        Core.IndexerQueryError
          "Cant resolve last blocks: Not stored anymore"
    Left (Core.SlotNoBoundsInvalid _) ->
      throwError $ Core.IndexerQueryError "Invalid bounds"

initialExtLedgerStateEvent :: C.GenesisConfig -> ExtLedgerStateEvent
initialExtLedgerStateEvent = flip ExtLedgerStateEvent 0 . CE.mkInitExtLedgerState

buildNextExtLedgerStateEvent
  :: ExtLedgerConfig -> ExtLedgerStateEvent -> C.BlockInMode C.CardanoMode -> IO ExtLedgerStateEvent
buildNextExtLedgerStateEvent extLedgerCfg currentState block =
  let currentLedgerState' = extLedgerState currentState
      applyBlock = CE.applyBlockExtLedgerState extLedgerCfg C.QuickValidation
   in do
        case applyBlock block currentLedgerState' of
          Left err -> throw . Core.IndexerInternalError . Text.pack . show $ err
          Right res -> pure $ ExtLedgerStateEvent res (getBlockNo block)

-- | File Indexer to save the @ExtLedgerState@
buildExtLedgerStateEventIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> SecurityParam
  -> FilePath
  -> m (Core.FileIndexer EpochMetadata ExtLedgerStateEvent)
buildExtLedgerStateEventIndexer codecConfig securityParam' path = do
  let serialiseLedgerState =
        CBOR.toStrictByteString
          . O.encodeExtLedgerState
            (O.encodeDisk codecConfig)
            (O.encodeDisk codecConfig)
            (O.encodeDisk codecConfig)
          . extLedgerState

      deserialiseLedgerState (EpochMetadata Nothing _) = const (Right Nothing)
      deserialiseLedgerState (EpochMetadata (Just blockNo') _) =
        bimap
          (Text.pack . show)
          (Just . flip ExtLedgerStateEvent blockNo' . snd)
          . CBOR.deserialiseFromBytes
            ( O.decodeExtLedgerState
                (O.decodeDisk codecConfig)
                (O.decodeDisk codecConfig)
                (O.decodeDisk codecConfig)
            )
          . BS.fromStrict
      blockNoAsText = maybe "" (Text.pack . show . (\(C.BlockNo b) -> b) . blockNo)
      metadataAsText (Core.Timed C.ChainPointAtGenesis evt) = [blockNoAsText evt]
      metadataAsText (Core.Timed chainPoint evt) =
        let chainPointTexts = case chainPoint of
              C.ChainPoint (C.SlotNo slotNo) blockHeaderHash ->
                [Text.pack $ show slotNo, C.serialiseToRawBytesHexText blockHeaderHash]
         in blockNoAsText evt : chainPointTexts
      immutableEpochs
        :: Core.Timed (Core.Point ExtLedgerStateEvent) (Maybe ExtLedgerStateEvent)
        -> [Core.EventInfo EpochMetadata]
        -> [Core.EventInfo EpochMetadata]
      immutableEpochs (Core.Timed _ Nothing) _eventsInfo = []
      immutableEpochs (Core.Timed _ (Just event)) eventsInfo = do
        let sortedEvents = sortOn (metadataBlockNo . Core.fileMetadata) eventsInfo
            lastBlockNo = blockNo event
            blockDepth = (\(C.BlockNo b) -> b) . (lastBlockNo -)
            isImmutable =
              maybe True ((> securityParam') . fromIntegral . blockDepth)
                . metadataBlockNo
                . Core.fileMetadata
            immutableEvents = takeWhile isImmutable sortedEvents
        case immutableEvents of
          [] -> []
          _ -> init immutableEvents
  Core.mkFileIndexer
    path
    (Just 180_000_000) -- Wait 180s for files to finish writing before terminating
    (Core.FileStorageConfig False immutableEpochs (comparing (Down . metadataBlockNo)))
    (Core.FileBuilder "epochState" "cbor" metadataAsText serialiseLedgerState serialisePoint)
    (Core.EventBuilder deserialiseMetadata metadataChainpoint deserialiseLedgerState deserialisePoint)

-- | File Indexer to save the @BlockInMode@
buildBlockIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> SecurityParam
  -> FilePath
  -> m (Core.FileIndexer EpochMetadata (C.BlockInMode C.CardanoMode))
buildBlockIndexer codecConfig securityParam' path = do
  let blockNoAsText = maybe "" (Text.pack . show . (\(C.BlockNo b) -> b) . getBlockNo)
      metadataAsText :: Core.Timed C.ChainPoint (Maybe (C.BlockInMode C.CardanoMode)) -> [Text]
      metadataAsText (Core.Timed C.ChainPointAtGenesis evt) = [blockNoAsText evt]
      metadataAsText (Core.Timed chainPoint evt) =
        let chainPointTexts = case chainPoint of
              C.ChainPoint (C.SlotNo slotNo) blockHeaderHash ->
                [Text.pack $ show slotNo, C.serialiseToRawBytesHexText blockHeaderHash]
         in blockNoAsText evt : chainPointTexts
      immutableBlocks
        :: Core.Timed (Core.Point ExtLedgerStateEvent) (Maybe (C.BlockInMode C.CardanoMode))
        -> [Core.EventInfo EpochMetadata]
        -> [Core.EventInfo EpochMetadata]
      immutableBlocks (Core.Timed _ Nothing) _eventsInfo = []
      immutableBlocks (Core.Timed _ (Just event)) eventsInfo = do
        let sortedEvents = sortOn (metadataBlockNo . Core.fileMetadata) eventsInfo
            lastBlockNo = getBlockNo event
            blockDepth = (\(C.BlockNo b) -> b) . (lastBlockNo -)
            isImmutable =
              maybe True ((> securityParam') . fromIntegral . blockDepth)
                . metadataBlockNo
                . Core.fileMetadata
            immutableEvents = takeWhile isImmutable sortedEvents
        case immutableEvents of
          [] -> []
          _ -> init immutableEvents
      blockNodeToNodeVersionM = do
        nodeToClientVersion <- snd $ O.latestReleasedNodeVersion (Proxy @(O.CardanoBlock O.StandardCrypto))
        Map.lookup nodeToClientVersion $
          O.supportedNodeToClientVersions (Proxy @(O.CardanoBlock O.StandardCrypto))
  blockNodeToNodeVersion <- case blockNodeToNodeVersionM of
    Nothing -> throwError $ Core.IndexerInternalError "Can't finde block to Node version"
    Just v -> pure v
  Core.mkFileIndexer
    path
    (Just 60_000_000) -- Wait 60s for files to finish writing before terminating
    (Core.FileStorageConfig False immutableBlocks (comparing metadataBlockNo))
    ( Core.FileBuilder
        "block"
        "cbor"
        metadataAsText
        (serialiseBlock codecConfig blockNodeToNodeVersion)
        serialisePoint
    )
    ( Core.EventBuilder
        deserialiseMetadata
        metadataChainpoint
        (deserialiseBlock codecConfig blockNodeToNodeVersion)
        deserialisePoint
    )

serialisePoint :: C.ChainPoint -> BS.ByteString
serialisePoint =
  let pointEncoding :: C.ChainPoint -> CBOR.Encoding
      pointEncoding C.ChainPointAtGenesis = CBOR.encodeBool False
      pointEncoding (C.ChainPoint (C.SlotNo s) (C.HeaderHash bhh)) =
        CBOR.encodeBool True <> CBOR.encodeWord64 s <> CBOR.encodeBytes (BS.Short.fromShort bhh)
   in CBOR.toStrictByteString . pointEncoding

deserialisePoint :: BS.ByteString -> Either Text C.ChainPoint
deserialisePoint bs =
  let pointDecoding = do
        b <- CBOR.decodeBool
        if b
          then do
            s <- C.SlotNo <$> CBOR.decodeWord64
            bhh <- C.HeaderHash . BS.Short.toShort <$> CBOR.decodeBytes
            pure $ C.ChainPoint s bhh
          else pure C.ChainPointAtGenesis
   in case CBOR.deserialiseFromBytes pointDecoding . BS.fromStrict $ bs of
        Right (remain, res) | BS.Lazy.null remain -> Right res
        _other -> Left "Can't read chainpoint"

deserialiseMetadata :: [Text] -> Maybe EpochMetadata
deserialiseMetadata [blockNoStr, slotNoStr, bhhStr] = do
  EpochMetadata
    <$> parseBlockNo blockNoStr
    <*> (C.ChainPoint <$> parseSlotNo slotNoStr <*> parseBlockHeaderHash bhhStr)
  where
    parseSlotNo = fmap C.SlotNo . Text.readMaybe . Text.unpack
    parseBlockHeaderHash bhhStr' = do
      bhhBs <- either (const Nothing) Just $ Base16.decode $ Text.encodeUtf8 bhhStr'
      either (const Nothing) Just $ C.deserialiseFromRawBytes (C.proxyToAsType Proxy) bhhBs
    parseBlockNo "" = pure Nothing
    parseBlockNo bno = Just . C.BlockNo <$> Text.readMaybe (Text.unpack bno)
deserialiseMetadata _ = Nothing

serialiseBlock
  :: O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> O.BlockNodeToClientVersion (O.CardanoBlock O.StandardCrypto)
  -> C.BlockInMode C.CardanoMode
  -> BS.ByteString
serialiseBlock codecConfig blockToNode =
  CBOR.toStrictByteString . O.encodeNodeToClient codecConfig blockToNode . C.toConsensusBlock

deserialiseBlock
  :: O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> O.BlockNodeToClientVersion (O.CardanoBlock O.StandardCrypto)
  -> EpochMetadata
  -> BS.ByteString
  -> Either Text (Maybe (C.BlockInMode C.CardanoMode))
deserialiseBlock _codecConfig _blockToNode (EpochMetadata Nothing _) = const (Right Nothing)
deserialiseBlock codecConfig blockToNode _metadata =
  bimap
    (Text.pack . show)
    (Just . C.fromConsensusBlock C.CardanoMode . snd)
    . CBOR.deserialiseFromBytes (O.decodeNodeToClient codecConfig blockToNode)
    . BS.fromStrict

-- Data extraction

getBlockNo :: C.BlockInMode C.CardanoMode -> C.BlockNo
getBlockNo (C.BlockInMode block _eraInMode) =
  case C.getBlockHeader block of C.BlockHeader _ _ b -> b

getEpochNo
  :: O.ExtLedgerState (O.CardanoBlock O.StandardCrypto)
  -> Maybe C.EpochNo
getEpochNo extLedgerState' = case O.ledgerState extLedgerState' of
  O.LedgerStateByron _st -> Nothing
  O.LedgerStateShelley st -> getEpochNoFromShelleyBlock st
  O.LedgerStateAllegra st -> getEpochNoFromShelleyBlock st
  O.LedgerStateMary st -> getEpochNoFromShelleyBlock st
  O.LedgerStateAlonzo st -> getEpochNoFromShelleyBlock st
  O.LedgerStateBabbage st -> getEpochNoFromShelleyBlock st
  O.LedgerStateConway st -> getEpochNoFromShelleyBlock st
  where
    getEpochNoFromShelleyBlock = Just . Ledger.nesEL . O.shelleyLedgerState

readGenesisFile
  :: (MonadIO m, MonadError Core.IndexerError m)
  => FilePath
  -> m C.GenesisConfig
readGenesisFile nodeConfigPath = do
  nodeCfgE <- liftIO $ runExceptT $ C.readNodeConfig (C.File nodeConfigPath)
  nodeCfg <- case nodeCfgE of
    Left err -> throwError . Core.IndexerInternalError . Text.pack . show $ err
    Right cfg -> pure cfg
  genesisConfigE <- liftIO $ runExceptT $ C.readCardanoGenesisConfig nodeCfg
  case genesisConfigE of
    Left err -> throwError . Core.IndexerInternalError . Text.pack . show . C.renderGenesisConfigError $ err
    Right cfg -> pure cfg
