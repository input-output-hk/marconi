{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- | A coordinator that also maintains the @ExtLedgerState@.

* It takes an @input@ computes the @ExtLedgerState@ out of it
  (in a preprocessor, we don't want to pass the ledger state if it isn't needed)
* volatile @ExtLedgerState@s are stored in memory
  (in a preprocessor as well).
* Compute the @output@ from there
* pass the @output@ to the underlying coordinator
* saves the last known stable ledger state systematically on close
-}
module Marconi.Cardano.Indexers.ExtLedgerStateCoordinator (
  -- * Types
  ExtLedgerState,
  ExtLedgerStateEvent (..),
  ExtLedgerConfig,
  ExtLedgerStateCoordinator (ExtLedgerStateCoordinator),
  ExtLedgerStateWorkerConfig (..),
  EpochMetadata,

  -- * Constructors
  mkExtLedgerStateCoordinator,
  extLedgerStateWorker,
  buildExtLedgerStateEventIndexer,

  -- * Utils
  getEpochNo,
  newEpochPreprocessor,
  readGenesisFile,

  -- * For testing
  deserialiseLedgerState,
  deserialiseMetadata,
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
import Control.Lens ((%=), (+=), (-=), (.=), (^.))
import Control.Lens qualified as Lens
import Control.Monad (guard, when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Strict (MonadState (put), gets)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Short qualified as BS.Short
import Data.Data (Proxy (Proxy))
import Data.Foldable (foldrM)
import Data.Functor (($>))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ord (Down (Down), comparing)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Types (BlockEvent, SecurityParam (SecurityParam), blockInMode)
import Marconi.Core qualified as Core
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Config qualified as O
import Ouroboros.Consensus.Ledger.Extended qualified as O
import Ouroboros.Consensus.Shelley.Ledger qualified as O
import Ouroboros.Consensus.Storage.Serialisation qualified as O
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Read qualified as Text

type ExtLedgerState = O.ExtLedgerState (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
type ExtLedgerConfig = O.ExtLedgerCfg (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
type CodecConfig = O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))

type instance Core.Point ExtLedgerState = C.ChainPoint
type instance Core.Point (C.BlockInMode C.CardanoMode) = C.ChainPoint

-- | Base event used to store the 'ExtLedgerState'
data ExtLedgerStateEvent = ExtLedgerStateEvent
  { extLedgerState :: ExtLedgerState
  , blockNo :: C.BlockNo
  }
  deriving (Show)

type instance Core.Point ExtLedgerStateEvent = C.ChainPoint

-- | Metadata used to create 'ExtLedgerStateEventIndexer' filenames
data EpochMetadata = EpochMetadata
  { metadataBlockNo :: Maybe C.BlockNo
  , metadataChainpoint :: C.ChainPoint
  }
  deriving (Show)

type ExtLedgerStateFileIndexer = Core.FileIndexer EpochMetadata ExtLedgerStateEvent

newtype ExtLedgerStateCoordinator output input = ExtLedgerStateCoordinator
  { _coordinator :: Core.Coordinator output
  -- ^ A standard coordinator, which handles the workers
  }

Lens.makeLenses ''ExtLedgerStateCoordinator

data ExtLedgerStatePersistConfig = ExtLedgerStatePersistConfig
  { _blocksToNextSnapshot :: Word64
  -- ^ Number of block before the next snapshot (must be at least 1)
  , _snapshotInterval :: Word64
  -- ^ Track the previous snapshot, used for resuming (can't start at 0)
  , _ledgerStateIndexer :: ExtLedgerStateFileIndexer
  }

Lens.makeLenses ''ExtLedgerStatePersistConfig

-- | The state maintained by the preprocessor of the indexer to decide how to handle incoming events
data PreprocessorState output input = PreprocessorState
  { _volatileLedgerStates :: Seq (Core.Timed C.ChainPoint ExtLedgerStateEvent)
  -- ^ storage for volatile ledger states
  , _lastLedgerState :: Core.Timed C.ChainPoint ExtLedgerStateEvent
  -- ^ The last computed ledger state
  , _currentLength :: Word64
  -- ^ The number of volatile ledger states that are stored
  , _securityParam :: Word64
  -- ^ how many of them do we keep in memory
  , _persistConfig :: ExtLedgerStatePersistConfig
  -- ^ the indexer that saves snapshots
  }

Lens.makeLenses ''PreprocessorState

{- | Used to maintain a list of `securityParam` ledger states in memory.
  When full, the oldest stored ledger state is pushed out of the queue
  and we check if it should be saved (we save every `snapshotInterval + 1` time)
-}
updateLatestLedgerState
  :: forall io output input
   . (MonadIO io, MonadState (PreprocessorState output input) io, MonadError Core.IndexerError io)
  => Bool
  -> Core.Timed C.ChainPoint ExtLedgerStateEvent
  -> io (Maybe C.ChainPoint)
updateLatestLedgerState isVolatile' event =
  let resetBlockCounterIfNeeded = do
        timeToNextSnapshot <- Lens.use $ persistConfig . blocksToNextSnapshot
        when (timeToNextSnapshot == 0) $ do
          snapshotInterval' <- Lens.use $ persistConfig . snapshotInterval
          persistConfig . blocksToNextSnapshot .= snapshotInterval'
      storeNewSnapshot :: io (Maybe (Core.Timed C.ChainPoint ExtLedgerStateEvent))
      storeNewSnapshot = do
        volatileLedgerStates' <- Lens.use volatileLedgerStates
        case volatileLedgerStates' of
          -- If we didn't start indexing volatile event yet
          Seq.Empty -> do
            eventToSave <- Lens.use lastLedgerState
            if isVolatile'
              then do
                -- we reached the first volatile event
                -- We always save the last stable event before we reach volatile ones
                persistConfig . blocksToNextSnapshot .= 0
                volatileLedgerStates %= (event Seq.<|)
              else do
                persistConfig . blocksToNextSnapshot -= 1
            timeToNextSnapshot <- Lens.use $ persistConfig . blocksToNextSnapshot
            lastLedgerState .= event
            pure $ guard (timeToNextSnapshot == 0) $> eventToSave
          -- If we're already indexing volatile event
          xs Seq.:|> oldestLedgerState -> do
            max' <- Lens.use securityParam
            current <- Lens.use currentLength
            let bufferIsFull = max' <= current
            if bufferIsFull
              then volatileLedgerStates .= (event Seq.<| xs)
              else do
                volatileLedgerStates %= (event Seq.<|)
                currentLength += 1
            timeToNextSnapshot <- Lens.use $ persistConfig . blocksToNextSnapshot
            lastLedgerState .= event
            pure $ guard (timeToNextSnapshot == 0 && bufferIsFull) $> oldestLedgerState
      saveSnapshot eventToSave = do
        let savePoint = eventToSave ^. Core.point
        indexer <- Lens.use $ persistConfig . ledgerStateIndexer
        indexer' <- Core.setLastStablePoint savePoint =<< Core.index (Just <$> eventToSave) indexer
        persistConfig . ledgerStateIndexer .= indexer'
        pure $ Just savePoint
   in do
        snapshotCandidate <- storeNewSnapshot
        resetBlockCounterIfNeeded
        case snapshotCandidate of
          Nothing -> pure Nothing
          Just eventToSave -> saveSnapshot eventToSave

-- | Configuration of the worker
data ExtLedgerStateWorkerConfig m output input = ExtLedgerStateWorkerConfig
  { workerEventExtractor :: input -> BlockEvent
  -- ^ Get the blockEvent (required to compute the ledger state) from the event
  , workerDistanceExtractor :: input -> Word64
  -- ^ Get the distance to the tip
  , workerLogger :: BM.Trace m Text
  -- ^ The logger
  , workerNodeConfigPath :: FilePath
  -- ^ Location of the node config, used to initialise the ledger state
  , workerSnapshotInterval :: Word64
  -- ^ Number of blocks before two snapshots of the ledger state
  , workerSecurityParam :: SecurityParam
  -- ^ Number of volatile blocks
  , workerOutputBuilder :: ExtLedgerStateEvent -> ExtLedgerStateEvent -> input -> Maybe output
  -- ^ How to create an @output@ from the previous ledger state, the current one and the current input
  }

{- | The preprocessor maintain the ledger state and pass the processed ledger state to the
coordinator.
-}
extLedgerStatePreprocessor
  :: C.GenesisConfig
  -> ExtLedgerStateWorkerConfig m output input
  -> PreprocessorState input output
  -> Core.Preprocessor (ExceptT Core.IndexerError IO) C.ChainPoint input output
extLedgerStatePreprocessor genesisCfg config initialState =
  let extLedgerCfg = CE.mkExtLedgerConfig genesisCfg
      extract = workerEventExtractor config
      applyBlock = CE.applyBlockExtLedgerState extLedgerCfg C.QuickValidation
   in flip Core.preprocessorM (pure initialState) $ \case
        Core.Rollback p -> do
          -- Remove the invalid ledger state from the list of ledger states
          (dropped, kept) <- Lens.uses volatileLedgerStates (Seq.spanl ((p <) . Lens.view Core.point))
          volatileLedgerStates .= kept
          case kept of
            Seq.Empty -> pure ()
            latest Seq.:<| _ -> lastLedgerState .= latest
          currentLength -= fromIntegral (length dropped)
          pure [Core.Rollback p]
        Core.Index (Core.Timed p Nothing) -> pure [Core.Index $ Core.Timed p Nothing]
        Core.Index (Core.Timed p (Just evt)) -> do
          currentLedgerState <- Lens.use $ lastLedgerState . Core.event
          let block = extract evt
              blockInMode' = blockInMode block
          case applyBlock blockInMode' (extLedgerState currentLedgerState) of
            Left err -> throwError . Core.IndexerInternalError . Text.pack . show $ err
            Right ledgerStateEvent -> do
              let ledgerState = ExtLedgerStateEvent ledgerStateEvent (getBlockNo blockInMode')
                  timedLedgerState = Core.Timed p ledgerState
                  distance = workerDistanceExtractor config evt
                  SecurityParam security = workerSecurityParam config
                  isVolatile' = distance <= security
                  outputEvent = workerOutputBuilder config currentLedgerState ledgerState evt
                  timedOutput = Core.Timed p outputEvent
              lastStablePoint' <- updateLatestLedgerState isVolatile' timedLedgerState
              let stableAtInput = case lastStablePoint' of
                    Nothing -> []
                    Just C.ChainPointAtGenesis -> []
                    Just p' -> [Core.StableAt p']
              pure $ Core.Index timedOutput : stableAtInput
        Core.IndexAllDescending evts -> do
          currentLedgerState <- Lens.use (lastLedgerState . Core.event)
          let go (Core.Timed _ Nothing) (l, acc) = pure (l, acc)
              go (Core.Timed p (Just evt)) (l, acc) = do
                let block = extract evt
                    blockInMode' = blockInMode block
                case applyBlock blockInMode' (extLedgerState l) of
                  Left err -> throwError . Core.IndexerInternalError . Text.pack . show $ err
                  Right ledgerStateEvent -> do
                    let ledgerState = ExtLedgerStateEvent ledgerStateEvent (getBlockNo blockInMode')
                        timedLedgerState = Core.Timed p ledgerState
                        distance = workerDistanceExtractor config evt
                        SecurityParam security = workerSecurityParam config
                        isVolatile' = distance <= security
                        outputEvent = workerOutputBuilder config currentLedgerState ledgerState evt
                        timedOutput = Core.Timed p outputEvent
                    lastStablePoint' <- updateLatestLedgerState isVolatile' timedLedgerState
                    pure (ledgerState, maybe id ((:) . Core.StableAt) lastStablePoint' $ Core.Index timedOutput : acc)
          outputs <- snd <$> foldrM go (currentLedgerState, []) (NonEmpty.toList evts)
          pure $ reverse outputs
        -- These are swallows as stability correspond to ledger state savepoints
        Core.StableAt _p -> pure []
        Core.Stop -> do
          -- TODO close the indexer in a separateThread
          indexer <- saveLastStable
          Core.close indexer
          pure [Core.Stop]

saveLastStable
  :: (MonadIO io, MonadState (PreprocessorState output input) io, MonadError Core.IndexerError io)
  => io ExtLedgerStateFileIndexer
saveLastStable = do
  ledgerStates' <- Lens.use volatileLedgerStates
  eventToSave <- case ledgerStates' of
    Seq.Empty ->
      Just <$> Lens.use lastLedgerState
    _xs Seq.:|> oldestLedgerState -> do
      max' <- Lens.use securityParam
      current <- Lens.use currentLength
      let bufferIsFull = max' <= current
      if bufferIsFull
        then pure $ Just oldestLedgerState
        else pure Nothing
  indexer <- Lens.use (persistConfig . ledgerStateIndexer)
  case eventToSave of
    Nothing -> pure indexer
    Just timedEvent ->
      Core.setLastStablePoint (timedEvent ^. Core.point) =<< Core.index (Just <$> timedEvent) indexer

{- | Create a naked coordinator.
| You would usually prefer to use a worker, as it will maintain the
| ledger state before sending it to the coordinator.
-}
mkExtLedgerStateCoordinator
  :: (MonadIO m, MonadError Core.IndexerError m, Core.Point output ~ C.ChainPoint)
  => [Core.Worker output C.ChainPoint]
  -> m (ExtLedgerStateCoordinator output input)
mkExtLedgerStateCoordinator workers = do
  coordinator' <- liftIO $ Core.mkCoordinator workers
  pure $ ExtLedgerStateCoordinator coordinator'

-- | Create a worker for the extLedgerState coordinator
extLedgerStateWorker
  :: forall m output input
   . ( MonadIO m
     , MonadError Core.IndexerError m
     , Core.Point output ~ C.ChainPoint
     , Core.Point input ~ C.ChainPoint
     )
  => ExtLedgerStateWorkerConfig IO output input
  -> [Core.Worker output C.ChainPoint]
  -> FilePath
  -> m
      ( Core.WorkerIndexer
          IO
          input
          output
          (Core.WithTrace IO (ExtLedgerStateCoordinator output))
      )
extLedgerStateWorker config workers path = do
  genesisCfg <- readGenesisFile $ workerNodeConfigPath config
  let SecurityParam securityParam' = workerSecurityParam config
      indexerName = "ExtLedgerStateEvent"
      indexerEventLogger = BM.contramap (fmap $ fmap $ Text.pack . show) $ workerLogger config
      rootDir = path </> "ledgerState"
      initialExtLedgerStateEvent' = initialExtLedgerStateEvent genesisCfg
      extLedgerCfg = CE.mkExtLedgerConfig genesisCfg
      configCodec = O.configCodec . O.getExtLedgerCfg $ extLedgerCfg
  liftIO $ createDirectoryIfMissing True rootDir
  ledgerStateIndexer' <- buildExtLedgerStateEventIndexer configCodec rootDir
  let snapshotInterval' = workerSnapshotInterval config
      persistConfig' = mkExtLedgerStatePersistConfig snapshotInterval' ledgerStateIndexer'
  coordinator' <-
    Core.withTrace indexerEventLogger
      <$> mkExtLedgerStateCoordinator workers
  workerState <- liftIO $ Con.newMVar coordinator'
  ledgerStateE <- runExceptT $ restoreLedgerState initialExtLedgerStateEvent' ledgerStateIndexer'
  extLedgerState' <- case ledgerStateE of
    Left _err -> throwError $ Core.IndexerInternalError "can't restore ledger state"
    Right res -> pure res
  let initialState = PreprocessorState Seq.empty extLedgerState' 0 securityParam' persistConfig'
      eventPreprocessing =
        extLedgerStatePreprocessor genesisCfg config initialState
          <<< Core.withResume (extLedgerState' ^. Core.point)
  pure $
    Core.WorkerIndexer workerState $
      Core.Worker indexerName workerState eventPreprocessing id

{- | Smart constructor for ExtLedgerStatePersistConfig, ensure that the time to next snapshot is at
least one block (saving each ledger state)
-}
mkExtLedgerStatePersistConfig
  :: Word64 -> Core.FileIndexer EpochMetadata ExtLedgerStateEvent -> ExtLedgerStatePersistConfig
mkExtLedgerStatePersistConfig snapshotInterval' =
  let curatedSnapshotInterval = max 1 snapshotInterval'
   in ExtLedgerStatePersistConfig curatedSnapshotInterval curatedSnapshotInterval

instance
  ( MonadIO m
  , MonadError Core.IndexerError m
  , Core.Point output ~ C.ChainPoint
  )
  => Core.IsIndex m output (ExtLedgerStateCoordinator output)
  where
  index = Core.indexVia coordinator
  rollback = Core.rollbackVia coordinator
  setLastStablePoint = Core.setLastStablePointVia coordinator

-- Restoration of the LedgerState

restoreLedgerState
  :: (MonadIO m, MonadError (Core.QueryError (Core.EventAtQuery ExtLedgerStateEvent)) m)
  => ExtLedgerStateEvent
  -> ExtLedgerStateFileIndexer
  -> m (Core.Timed C.ChainPoint ExtLedgerStateEvent)
restoreLedgerState firstExtLedgerStateEvent indexer = do
  let getLatest [] = Core.Timed Core.genesis firstExtLedgerStateEvent
      getLatest (x : _) = x
  result <- runExceptT $ Core.queryLatest Core.latestEvent indexer
  case result of
    Right xs -> pure $ getLatest xs
    Left (Core.AheadOfLastSync partialResult) -> case partialResult of
      Nothing ->
        throwError $
          Core.IndexerQueryError "Cant resolve last ledgerState: No previous result"
      Just _ ->
        throwError $
          Core.IndexerQueryError "Wrong ledger state is stored"
    Left (Core.IndexerQueryError err) ->
      throwError $
        Core.IndexerQueryError $
          "Cant resolve last ledgerState: " <> Text.pack (show err)
    Left Core.NotStoredAnymore ->
      throwError $
        Core.IndexerQueryError
          "Cant resolve last ledgerState: Not stored anymore"
    Left (Core.SlotNoBoundsInvalid _) ->
      throwError $ Core.IndexerQueryError "Invalid bounds"

instance
  ( MonadIO m
  , MonadError Core.IndexerError m
  , Core.Point event ~ Core.Point output
  , Core.Point output ~ C.ChainPoint
  , Ord (Core.Point output)
  )
  => Core.IsSync m event (ExtLedgerStateCoordinator output)
  where
  lastSyncPoint = Core.lastSyncPointVia coordinator
  lastStablePoint = Core.lastStablePointVia coordinator

instance
  (MonadIO m, MonadError Core.IndexerError m)
  => Core.Closeable m (ExtLedgerStateCoordinator output)
  where
  close = Core.closeVia coordinator

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

initialExtLedgerStateEvent :: C.GenesisConfig -> ExtLedgerStateEvent
initialExtLedgerStateEvent = flip ExtLedgerStateEvent 0 . CE.mkInitExtLedgerState

-- | File Indexer to save the @ExtLedgerState@
buildExtLedgerStateEventIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> FilePath
  -> m (Core.FileIndexer EpochMetadata ExtLedgerStateEvent)
buildExtLedgerStateEventIndexer codecConfig path = do
  let serialiseLedgerState =
        CBOR.toBuilder
          . O.encodeExtLedgerState
            (O.encodeDisk codecConfig)
            (O.encodeDisk codecConfig)
            (O.encodeDisk codecConfig)
          . extLedgerState

      blockNoAsText = maybe "" (Text.pack . show . (\(C.BlockNo b) -> b) . blockNo)
      metadataAsText (Core.Timed C.ChainPointAtGenesis evt) = [blockNoAsText evt]
      metadataAsText (Core.Timed chainPoint evt) =
        let chainPointTexts = case chainPoint of
              C.ChainPoint (C.SlotNo slotNo) blockHeaderHash ->
                [Text.pack $ show slotNo, C.serialiseToRawBytesHexText blockHeaderHash]
         in blockNoAsText evt : chainPointTexts
      keepLast [] = ([], [])
      keepLast (x : xs) = ([x], xs)
  Core.mkFileIndexer
    path
    Nothing -- (Just 180_000_000) -- Wait 180s for files to finish writing before terminating
    ( Core.FileStorageConfig
        False
        (Core.withPartition $ const keepLast)
        (comparing (Down . metadataBlockNo))
    )
    (Core.FileBuilder "ledgerState" "cbor" metadataAsText serialiseLedgerState serialisePoint)
    ( Core.EventBuilder
        deserialiseMetadata
        metadataChainpoint
        (deserialiseLedgerState codecConfig)
        deserialisePoint
    )

deserialiseLedgerState
  :: CodecConfig
  -> EpochMetadata
  -> BS.ByteString
  -> Either Text (Maybe ExtLedgerStateEvent)
deserialiseLedgerState _ (EpochMetadata Nothing _) = const (Right Nothing)
deserialiseLedgerState codecConfig (EpochMetadata (Just blockNo') _) =
  bimap
    (Text.pack . show)
    (Just . flip ExtLedgerStateEvent blockNo' . snd)
    . CBOR.deserialiseFromBytes
      ( O.decodeExtLedgerState
          (O.decodeDisk codecConfig)
          (O.decodeDisk codecConfig)
          (O.decodeDisk codecConfig)
      )

serialisePoint :: C.ChainPoint -> Builder
serialisePoint =
  let pointEncoding :: C.ChainPoint -> CBOR.Encoding
      pointEncoding C.ChainPointAtGenesis = CBOR.encodeBool False
      pointEncoding (C.ChainPoint (C.SlotNo s) (C.HeaderHash bhh)) =
        CBOR.encodeBool True <> CBOR.encodeWord64 s <> CBOR.encodeBytes (BS.Short.fromShort bhh)
   in CBOR.toBuilder . pointEncoding

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
   in case CBOR.deserialiseFromBytes pointDecoding bs of
        Right (remain, res) | BS.null remain -> Right res
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
