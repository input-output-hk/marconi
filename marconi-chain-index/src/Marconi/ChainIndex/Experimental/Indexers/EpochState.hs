{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Marconi.ChainIndex.Experimental.Indexers.EpochState (
  -- * Events
  EpochState (EpochState),
  extLedgerState,
  blockNo,
  EpochNonce (EpochNonce),
  nonceEpochNo,
  nonceNonce,
  nonceBlockNo,
  EpochSDD (EpochSDD),
  sddEpochNo,
  sddPoolId,
  sddLovelace,
  sddBlockNo,
  EpochMetadata (..),

  -- * Indexer and worker
  EpochStateIndexer,
  EpochStateConfig (..),
  StandardEpochStateIndexer,
  mkEpochStateIndexer,
  mkEpochStateWorker,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.ExtLedgerState qualified as CE
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Ledger.UMap qualified as Ledger
import Cardano.Protocol.TPraos.API qualified as Shelley
import Cardano.Protocol.TPraos.Rules.Tickn qualified as Shelley
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Exception (throw)
import Control.Lens (Lens')
import Control.Lens qualified as Lens
import Control.Lens.Operators ((&), (.~), (^.))
import Control.Monad (foldM, guard, (<=<))
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Data.Bifunctor (bimap)
import Data.ByteString.Base16 qualified as Base16
import Data.Coerce (coerce)
import Data.Data (Proxy (Proxy))
import Data.Foldable (Foldable (toList))
import Data.Functor (($>))
import Data.List (genericLength)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.VMap (VMap)
import Data.VMap qualified as VMap
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Marconi.ChainIndex.Experimental.Extract.WithDistance (
  WithDistance (WithDistance),
 )
import Marconi.ChainIndex.Experimental.Extract.WithDistance qualified as Distance
import Marconi.ChainIndex.Experimental.Indexers.Orphans ()
import Marconi.ChainIndex.Experimental.Indexers.SyncHelper qualified as Sync
import Marconi.ChainIndex.Experimental.Indexers.Worker (
  StandardIndexer,
  StandardWorkerConfig (securityParamConfig),
  eventExtractor,
  mkStandardIndexer,
  workerName,
 )
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (SecurityParam (SecurityParam))
import Marconi.Core.Experiment qualified as Core
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Config qualified as O
import Ouroboros.Consensus.HeaderValidation qualified as O
import Ouroboros.Consensus.Ledger.Extended qualified as O
import Ouroboros.Consensus.Protocol.Praos qualified as O
import Ouroboros.Consensus.Protocol.TPraos qualified as O
import Ouroboros.Consensus.Shelley.Ledger qualified as O
import Ouroboros.Consensus.Storage.Serialisation qualified as O
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Text.Read qualified as Text

type ExtLedgerState = O.ExtLedgerState (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
type ExtLedgerConfig = O.ExtLedgerCfg (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
type instance Core.Point (C.BlockInMode C.CardanoMode) = C.ChainPoint
type instance Core.Point EpochState = C.ChainPoint

-- | Base event used to store the 'ExtLedgerState'
data EpochState = EpochState {extLedgerState :: ExtLedgerState, blockNo :: C.BlockNo}

-- | Event for @Nonce@ storage
data EpochNonce = EpochNonce
  { _nonceEpochNo :: !C.EpochNo
  , _nonceNonce :: !Ledger.Nonce
  , _nonceBlockNo :: !C.BlockNo
  }
  deriving (Eq, Ord, Show, Generic, SQL.FromRow, SQL.ToRow)

instance SQL.ToRow (Core.Timed C.ChainPoint EpochNonce) where
  toRow epochNonce =
    SQL.toRow (epochNonce ^. Core.event)
      <> [SQL.toField $ epochNonce ^. Core.point . Lens.to C.chainPointToSlotNo]

type instance Core.Point EpochNonce = C.ChainPoint

Lens.makeLenses ''EpochNonce

-- | Event for @SDD@ storage
data EpochSDD = EpochSDD
  { _sddEpochNo :: !C.EpochNo
  , _sddPoolId :: !C.PoolId
  , _sddLovelace :: !C.Lovelace
  , _sddBlockNo :: !C.BlockNo
  }
  deriving (Eq, Ord, Show, Generic, SQL.FromRow, SQL.ToRow)

type instance Core.Point (NonEmpty EpochSDD) = C.ChainPoint

instance SQL.ToRow (Core.Timed C.ChainPoint EpochSDD) where
  toRow epochSDD =
    SQL.toRow (epochSDD ^. Core.event)
      <> [SQL.toField $ epochSDD ^. Core.point . Lens.to C.chainPointToSlotNo]

instance SQL.FromRow (Core.Timed C.ChainPoint EpochSDD) where
  fromRow = do
    sdd <- SQL.fromRow
    point <- SQL.fromRow
    pure $ Core.Timed point sdd

data EpochMetadata = EpochMetadata
  { metadataBlockNo :: Maybe C.BlockNo
  , metadataChainpoint :: C.ChainPoint
  }

type instance Core.Point EpochSDD = C.ChainPoint

Lens.makeLenses ''EpochSDD

type LedgerStateFileIndexer =
  Core.WithTransform
    (Core.FileIndexer EpochMetadata)
    EpochState
    (WithDistance EpochState)

type BlockFileIndexer =
  Core.WithTransform
    (Core.FileIndexer EpochMetadata)
    (C.BlockInMode C.CardanoMode)
    (WithDistance (C.BlockInMode C.CardanoMode))

-- | The inner state of the 'EpochStateIndexer'
data EpochStateIndexerState event = EpochStateIndexerState
  { _stateCurrentLedgerState :: EpochState
  , _stateTimeToNextSnapshot :: Word
  , _stateEpochStateIndexer :: LedgerStateFileIndexer
  , _stateBlockIndexer :: BlockFileIndexer
  , _stateEpochNonceIndexer :: StandardIndexer IO Core.SQLiteIndexer EpochNonce
  , _stateEpochSDDIndexer :: StandardIndexer IO Core.SQLiteIndexer (NonEmpty EpochSDD)
  }

Lens.makeLenses ''EpochStateIndexerState

-- | The configuration of the 'EpochStateIndexer'
data EpochStateIndexerConfig event = EpochStateIndexerConfig
  { _configSnapshotInterval :: Word
  , _configGenesisConfig :: C.GenesisConfig
  , _configSecurityParam :: SecurityParam
  }

Lens.makeLenses ''EpochStateIndexerConfig

data EpochStateIndexer event = EpochStateIndexer
  { _epochStateIndexerState :: EpochStateIndexerState event
  , _epochStateIndexerConfig :: EpochStateIndexerConfig event
  }

Lens.makeLenses ''EpochStateIndexer

type StandardEpochStateIndexer = EpochStateIndexer (WithDistance (C.BlockInMode C.CardanoMode))

currentLedgerState :: Lens' (EpochStateIndexer event) EpochState
currentLedgerState = epochStateIndexerState . stateCurrentLedgerState

currentEpoch :: EpochStateIndexer event -> Maybe C.EpochNo
currentEpoch = Lens.views currentLedgerState (getEpochNo . extLedgerState)

blocksBeforeNextSnapshot :: Lens' (EpochStateIndexer event) Word
blocksBeforeNextSnapshot = epochStateIndexerState . stateTimeToNextSnapshot

epochNonceIndexer
  :: Lens'
      (EpochStateIndexer event)
      (StandardIndexer IO Core.SQLiteIndexer EpochNonce)
epochNonceIndexer = epochStateIndexerState . stateEpochNonceIndexer

epochSDDIndexer
  :: Lens'
      (EpochStateIndexer event)
      (StandardIndexer IO Core.SQLiteIndexer (NonEmpty EpochSDD))
epochSDDIndexer = epochStateIndexerState . stateEpochSDDIndexer

epochStateIndexer :: Lens' (EpochStateIndexer event) LedgerStateFileIndexer
epochStateIndexer = epochStateIndexerState . stateEpochStateIndexer

blockIndexer :: Lens' (EpochStateIndexer event) BlockFileIndexer
blockIndexer = epochStateIndexerState . stateBlockIndexer

snapshotInterval :: Lens' (EpochStateIndexer event) Word
snapshotInterval = epochStateIndexerConfig . configSnapshotInterval

genesisConfig :: Lens' (EpochStateIndexer event) C.GenesisConfig
genesisConfig = epochStateIndexerConfig . configGenesisConfig

securityParam :: Lens' (EpochStateIndexer event) SecurityParam
securityParam = epochStateIndexerConfig . configSecurityParam

extLedgerConfig :: EpochStateIndexer event -> ExtLedgerConfig
extLedgerConfig = Lens.views genesisConfig CE.mkExtLedgerConfig

initialEpochState :: EpochStateIndexer event -> EpochState
initialEpochState = Lens.views genesisConfig (flip EpochState 0 . CE.mkInitExtLedgerState)

toEpochNonce :: WithDistance EpochState -> Maybe (WithDistance EpochNonce)
toEpochNonce (WithDistance d epochState) = do
  let ledgerState = extLedgerState epochState
  epochNo <- getEpochNo ledgerState
  pure $ WithDistance d $ EpochNonce epochNo (getEpochNonce ledgerState) (blockNo epochState)

toEpochSDD :: WithDistance EpochState -> Maybe (WithDistance (NonEmpty EpochSDD))
toEpochSDD (WithDistance d epochState) = fmap (WithDistance d) $ NonEmpty.nonEmpty $ do
  let ledgerState = extLedgerState epochState
  epochNo <- toList $ getEpochNo ledgerState
  (poolId, lovelace) <- Map.toList $ getStakeMap $ extLedgerState epochState
  pure $ EpochSDD epochNo poolId lovelace (blockNo epochState)

data EpochStateConfig = EpochStateConfig
  { nodeConfig :: FilePath
  -- ^ node config path
  , snapshotIntervalInBlocks :: Word
  -- ^ number of blocks between each snapshot
  }

mkEpochStateIndexer
  :: (MonadIO n, MonadError Core.IndexerError n)
  => StandardWorkerConfig IO a b
  -> EpochStateConfig
  -> FilePath
  -> n (Core.WithResume EpochStateIndexer (WithDistance (C.BlockInMode C.CardanoMode)))
mkEpochStateIndexer workerCfg cfg rootDir = do
  genesisCfg <- readGenesisFile (nodeConfig cfg)
  let securityParam' = securityParamConfig workerCfg
      config = EpochStateIndexerConfig (snapshotIntervalInBlocks cfg) genesisCfg securityParam'
      extLedgerCfg = CE.mkExtLedgerConfig genesisCfg
      configCodec = O.configCodec . O.getExtLedgerCfg $ extLedgerCfg
      addTransform
        :: (Core.Point a ~ C.ChainPoint)
        => indexer a
        -> Core.WithTransform indexer a (WithDistance a)
      addTransform = Core.withTransform id (Just . Distance.getEvent)
  liftIO $ createDirectoryIfMissing True rootDir
  epochSDDIndexer' <- mkStandardIndexer workerCfg =<< buildEpochSDDIndexer (rootDir </> "epochSDD.db")
  epochNonceIndexer' <-
    mkStandardIndexer workerCfg =<< buildEpochNonceIndexer (rootDir </> "epochNonce.db")
  epochStateIndexer' <-
    addTransform
      <$> buildEpochStateIndexer
        configCodec
        (securityParamConfig workerCfg)
        (snapshotIntervalInBlocks cfg)
        (rootDir </> "epochState")
  epochBlocksIndexer <-
    addTransform
      <$> buildBlockIndexer
        configCodec
        (securityParamConfig workerCfg)
        (rootDir </> "epochBlocks")
  let state =
        EpochStateIndexerState
          (EpochState (CE.mkInitExtLedgerState genesisCfg) 0)
          (snapshotIntervalInBlocks cfg)
          epochStateIndexer'
          epochBlocksIndexer
          epochNonceIndexer'
          epochSDDIndexer'

  let indexer = EpochStateIndexer state config
  latestLedgerState <- liftIO $ runExceptT $ restoreLedgerState indexer
  case latestLedgerState of
    Left err -> throwError err
    Right extLedgerState' ->
      Core.withResume Core.lastSyncPoints (fromIntegral (securityParamConfig workerCfg) + 1) $
        indexer & currentLedgerState .~ extLedgerState'

mkEpochStateWorker
  :: (MonadIO n, MonadError Core.IndexerError n)
  => StandardWorkerConfig IO input (C.BlockInMode C.CardanoMode)
  -- ^ General configuration of the indexer (mostly for logging purpose)
  -> EpochStateConfig
  -> FilePath
  -> n
      ( Core.WorkerIndexer
          IO
          (WithDistance input)
          (WithDistance (C.BlockInMode C.CardanoMode))
          (Core.WithResume EpochStateIndexer)
      )
mkEpochStateWorker workerConfig epochStateConfig rootDir = do
  indexer <- mkEpochStateIndexer workerConfig epochStateConfig rootDir
  let mapEventUnderDistance = fmap sequence . traverse (eventExtractor workerConfig)
  Core.createWorker (workerName workerConfig) mapEventUnderDistance indexer

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
    parseBlockNo bhh = Just . C.BlockNo <$> Text.readMaybe (Text.unpack bhh)
deserialiseMetadata _ = Nothing

directorySize :: (MonadIO m) => FilePath -> m Word
directorySize dir = liftIO $ do
  b <- doesDirectoryExist dir
  if b then genericLength <$> listDirectory dir else pure 0

buildEpochStateIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> SecurityParam
  -> Word
  -- ^ save frequency
  -> FilePath
  -> m (Core.FileIndexer EpochMetadata EpochState)
buildEpochStateIndexer codecConfig securityParam' saveFrequency path = do
  let serialiseLedgerState =
        CBOR.toLazyByteString
          . O.encodeExtLedgerState
            (O.encodeDisk codecConfig)
            (O.encodeDisk codecConfig)
            (O.encodeDisk codecConfig)
          . extLedgerState

      deserialiseLedgerState (EpochMetadata Nothing _) = const (Right Nothing)
      deserialiseLedgerState (EpochMetadata (Just blockNo') _) =
        bimap
          (Text.pack . show)
          (Just . flip EpochState blockNo' . snd)
          . CBOR.deserialiseFromBytes
            ( O.decodeExtLedgerState
                (O.decodeDisk codecConfig)
                (O.decodeDisk codecConfig)
                (O.decodeDisk codecConfig)
            )
      blockNoAsText = maybe "" (Text.pack . show . (\(C.BlockNo b) -> b) . blockNo)
      metadataAsText (Core.Timed C.ChainPointAtGenesis evt) = [blockNoAsText evt]
      metadataAsText (Core.Timed chainPoint evt) =
        let chainPointTexts = case chainPoint of
              C.ChainPoint (C.SlotNo slotNo) blockHeaderHash ->
                [Text.pack $ show slotNo, C.serialiseToRawBytesHexText blockHeaderHash]
         in blockNoAsText evt : chainPointTexts
      -- we always add one to get at least one stable ledger state
      storageSize = (fromIntegral securityParam' `div` saveFrequency) + 1
  currentSize <- directorySize path
  Core.mkFileIndexer
    path
    (Core.FileStorageConfig False (Just storageSize) (comparing metadataChainpoint) currentSize)
    (Core.FileBuilder "epochState" "cbor" metadataAsText serialiseLedgerState)
    (Core.EventBuilder deserialiseMetadata metadataChainpoint deserialiseLedgerState)

buildBlockIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> SecurityParam
  -> FilePath
  -> m (Core.FileIndexer EpochMetadata (C.BlockInMode C.CardanoMode))
buildBlockIndexer codecConfig securityParam' path = do
  let serialiseBlock =
        CBOR.toLazyByteString . O.encodeDisk codecConfig . C.toConsensusBlock
      deserialiseBlock (EpochMetadata Nothing _) = const (Right Nothing)
      deserialiseBlock _ =
        bimap
          (Text.pack . show)
          (Just . C.fromConsensusBlock C.CardanoMode)
          . fmap (\(bs', decode) -> decode bs')
          . CBOR.deserialiseFromBytes (O.decodeDisk codecConfig)
      blockNoAsText = maybe "" (Text.pack . show . (\(C.BlockNo b) -> b) . getBlockNo)
      metadataAsText (Core.Timed C.ChainPointAtGenesis evt) = [blockNoAsText evt]
      metadataAsText (Core.Timed chainPoint evt) =
        let chainPointTexts = case chainPoint of
              C.ChainPoint (C.SlotNo slotNo) blockHeaderHash ->
                [Text.pack $ show slotNo, C.serialiseToRawBytesHexText blockHeaderHash]
         in blockNoAsText evt : chainPointTexts
      storageSize = fromIntegral securityParam'
  currentSize <- directorySize path
  Core.mkFileIndexer
    path
    (Core.FileStorageConfig True (Just storageSize) (comparing metadataChainpoint) currentSize)
    (Core.FileBuilder "block" "cbor" metadataAsText serialiseBlock)
    (Core.EventBuilder deserialiseMetadata metadataChainpoint deserialiseBlock)

buildEpochSDDIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => FilePath
  -> m (Core.SQLiteIndexer (NonEmpty EpochSDD))
buildEpochSDDIndexer path = do
  let createSDD =
        [sql|CREATE TABLE IF NOT EXISTS epoch_sdd
              ( epochNo INT NOT NULL
              , poolId BLOB NOT NULL
              , lovelace INT NOT NULL
              , blockNo INT NOT NULL
              , slotNo INT NOT NULL
              )|]
      sddInsertQuery =
        [sql|INSERT INTO epoch_sdd
                ( epochNo
                , poolId
                , lovelace
                , blockNo
                , slotNo
                ) VALUES (?, ?, ?, ?, ?)|]
      insertEvent = [Core.SQLInsertPlan (traverse NonEmpty.toList) sddInsertQuery]
  Core.mkSqliteIndexer
    path
    [Sync.syncTableCreation, createSDD]
    [insertEvent]
    (Just Sync.syncInsertPlan)
    [ Core.SQLRollbackPlan "epoch_sdd" "slotNo" C.chainPointToSlotNo
    , Sync.syncRollbackPlan
    ]
    Sync.syncLastPointsQuery

buildEpochNonceIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => FilePath
  -> m (Core.SQLiteIndexer EpochNonce)
buildEpochNonceIndexer path = do
  let createNonce =
        [sql|CREATE TABLE IF NOT EXISTS epoch_nonce
              ( epochNo INT NOT NULL
              , nonce BLOB NOT NULL
              , blockNo INT NOT NULL
              , slotNo INT NOT NULL
              )|]
      nonceInsertQuery =
        [sql|INSERT INTO epoch_nonce
                ( epochNo
                , nonce
                , blockNo
                , slotNo
                ) VALUES (?, ?, ?, ?)|]
      insertEvent = [Core.SQLInsertPlan pure nonceInsertQuery]
  Core.mkSqliteIndexer
    path
    [Sync.syncTableCreation, createNonce]
    [insertEvent]
    (Just Sync.syncInsertPlan)
    [ Core.SQLRollbackPlan "epoch_nonce" "slotNo" C.chainPointToSlotNo
    , Sync.syncRollbackPlan
    ]
    Sync.syncLastPointsQuery

updateEpochState
  :: (MonadIO m) => C.BlockInMode C.CardanoMode -> EpochStateIndexer event -> m EpochState
updateEpochState block indexer = do
  let nextLedgerState = buildNextEpochState (extLedgerConfig indexer)
      currentState = indexer ^. currentLedgerState
  liftIO $ nextLedgerState currentState block

performSnapshots
  :: Word
  -> Core.Timed C.ChainPoint (Maybe (WithDistance (C.BlockInMode C.CardanoMode)))
  -> EpochStateIndexer event
  -> ExceptT Core.IndexerError IO (EpochStateIndexer event)
performSnapshots newBlocksBeforeNextSnapshot bim indexer = do
  let epochState = indexer ^. currentLedgerState
      (evt, blocksBeforeNextSnapshot') =
        if newBlocksBeforeNextSnapshot == 0
          then (Core.Timed (bim ^. Core.point) (Just epochState), indexer ^. snapshotInterval)
          else (Core.Timed (bim ^. Core.point) Nothing, newBlocksBeforeNextSnapshot)
      SecurityParam s = indexer ^. securityParam
      distanceToTip = Distance.chainDistance <$> bim ^. Core.event
      isStable = maybe True (s <) distanceToTip
      attachDistance e = do
        d <- distanceToTip
        e' <- e
        pure $ WithDistance d e'
      dEvt = attachDistance <$> evt
      snapshotEpochState = epochStateIndexer $ Core.index dEvt
      snapshotBlock =
        if isStable
          then pure
          else blockIndexer $ Core.index bim
      indexer' = indexer & blocksBeforeNextSnapshot .~ blocksBeforeNextSnapshot'
  snapshotEpochState <=< snapshotBlock $ indexer'

storeEmptyEpochStateRelatedInfo
  :: C.ChainPoint
  -> EpochStateIndexer event
  -> ExceptT Core.IndexerError IO (EpochStateIndexer event)
storeEmptyEpochStateRelatedInfo p indexer = do
  let indexNonce = epochNonceIndexer $ Core.index $ Core.Timed p Nothing
      indexSDD = epochSDDIndexer $ Core.index $ Core.Timed p Nothing
  indexNonce <=< indexSDD $ indexer

storeEpochStateRelatedInfo
  :: Core.Timed C.ChainPoint (Maybe (WithDistance EpochState))
  -> EpochStateIndexer event
  -> ExceptT Core.IndexerError IO (EpochStateIndexer event)
storeEpochStateRelatedInfo ledgerState indexer = do
  let indexNonce = epochNonceIndexer $ Core.index $ (>>= toEpochNonce) <$> ledgerState
      indexSDD = epochSDDIndexer $ Core.index $ (>>= toEpochSDD) <$> ledgerState
  indexNonce <=< indexSDD $ indexer

getLatestNonEmpty
  :: EpochState
  -> LedgerStateFileIndexer
  -> ExceptT Core.IndexerError IO (Core.Timed C.ChainPoint EpochState)
getLatestNonEmpty firstEpochState indexer = do
  result <- runExceptT $ Core.queryLatest Core.latestEvent indexer
  case result of
    Left _err -> throwError $ Core.IndexerInternalError "Cant resolve last epochState"
    Right [] -> pure $ Core.Timed Core.genesis firstEpochState
    Right (x : _) -> pure x

getBlocksFrom
  :: C.ChainPoint
  -> BlockFileIndexer
  -> ExceptT Core.IndexerError IO [C.BlockInMode C.CardanoMode]
getBlocksFrom from indexer = do
  result <- runExceptT $ Core.queryLatest (Core.EventsFromQuery from) indexer
  case result of
    Left _err -> throwError $ Core.IndexerInternalError "Cant resolve last epochState"
    Right xs -> pure $ Lens.view Core.event <$> xs

restoreLedgerState :: EpochStateIndexer event -> ExceptT Core.IndexerError IO EpochState
restoreLedgerState indexer = do
  Core.Timed epochStatePoint closestLedgerState <-
    getLatestNonEmpty (initialEpochState indexer) (indexer ^. epochStateIndexer)
  blocks <- getBlocksFrom epochStatePoint (indexer ^. blockIndexer)
  liftIO $
    foldM
      (buildNextEpochState $ extLedgerConfig indexer)
      closestLedgerState
      blocks

instance
  Core.IsIndex
    (ExceptT Core.IndexerError IO)
    (WithDistance (C.BlockInMode C.CardanoMode))
    EpochStateIndexer
  where
  index timedEvent@(Core.Timed p Nothing) indexer = do
    let newTimeToSnapshot = pred $ indexer ^. blocksBeforeNextSnapshot
    indexer' <- indexer & performSnapshots newTimeToSnapshot timedEvent
    storeEmptyEpochStateRelatedInfo p indexer'
  index timedEvent@(Core.Timed p (Just (WithDistance d bim))) indexer = do
    newEpochState <- updateEpochState bim indexer
    let oldEpoch = currentEpoch indexer
        newEpoch = getEpochNo $ extLedgerState newEpochState
        newTimeToSnapshot = pred $ indexer ^. blocksBeforeNextSnapshot
    indexer' <-
      indexer
        & currentLedgerState .~ newEpochState
        & performSnapshots newTimeToSnapshot timedEvent
    let epochIsNew = oldEpoch /= newEpoch
        -- We dont populate the event for SQL indexers if we don't have a new epoch
        epochStateEvent = Core.Timed p $ guard epochIsNew $> WithDistance d newEpochState
    storeEpochStateRelatedInfo epochStateEvent indexer'

  rollback p indexer = do
    let rollbackIndexers =
          epochStateIndexer (Core.rollback p)
            <=< blockIndexer (Core.rollback p)
            <=< epochSDDIndexer (Core.rollback p)
            <=< epochNonceIndexer (Core.rollback p)
    indexer' <- rollbackIndexers indexer
    newEpochState <- restoreLedgerState indexer'
    pure $ indexer' & currentLedgerState .~ newEpochState

instance
  (MonadIO m, MonadError Core.IndexerError m, Core.Point event ~ C.ChainPoint)
  => Core.IsSync m event EpochStateIndexer
  where
  lastSyncPoint indexer = Core.lastSyncPoint $ indexer ^. epochStateIndexer
  lastSyncPoints n indexer = Core.lastSyncPoints n $ indexer ^. epochStateIndexer

instance Core.Closeable (ExceptT Core.IndexerError IO) EpochStateIndexer where
  close indexer = do
    Core.close $ indexer ^. epochStateIndexer
    Core.close $ indexer ^. blockIndexer
    Core.close $ indexer ^. epochSDDIndexer
    Core.close $ indexer ^. epochNonceIndexer

{- | From LedgerState, get epoch stake pool delegation: a mapping of pool ID to amount staked in
 lovelace. We do this by getting the 'ssStakeMark stake snapshot and then use 'ssDelegations' and
 'ssStake' to resolve it into the desired mapping.
-}
getStakeMap
  :: O.ExtLedgerState (O.CardanoBlock O.StandardCrypto)
  -> Map C.PoolId C.Lovelace
getStakeMap extLedgerState' = case O.ledgerState extLedgerState' of
  O.LedgerStateByron _ -> mempty
  O.LedgerStateShelley st -> getStakeMapFromShelleyBlock st
  O.LedgerStateAllegra st -> getStakeMapFromShelleyBlock st
  O.LedgerStateMary st -> getStakeMapFromShelleyBlock st
  O.LedgerStateAlonzo st -> getStakeMapFromShelleyBlock st
  O.LedgerStateBabbage st -> getStakeMapFromShelleyBlock st
  O.LedgerStateConway st -> getStakeMapFromShelleyBlock st
  where
    getStakeMapFromShelleyBlock
      :: forall proto era c
       . (c ~ O.EraCrypto era, c ~ O.StandardCrypto)
      => O.LedgerState (O.ShelleyBlock proto era)
      -> Map C.PoolId C.Lovelace
    getStakeMapFromShelleyBlock st = sdd'
      where
        newEpochState :: Ledger.NewEpochState era
        newEpochState = O.shelleyLedgerState st

        stakeSnapshot :: Ledger.SnapShot c
        stakeSnapshot = Ledger.ssStakeMark . Ledger.esSnapshots . Ledger.nesEs $ newEpochState

        stakes
          :: VMap VMap.VB VMap.VP (Ledger.Credential 'Ledger.Staking c) (Ledger.CompactForm Ledger.Coin)
        stakes = Ledger.unStake $ Ledger.ssStake stakeSnapshot

        delegations
          :: VMap VMap.VB VMap.VB (Ledger.Credential 'Ledger.Staking c) (Ledger.KeyHash 'Ledger.StakePool c)
        delegations = Ledger.ssDelegations stakeSnapshot

        sdd' :: Map C.PoolId C.Lovelace
        sdd' =
          Map.fromListWith (+) $
            catMaybes $
              VMap.elems $
                VMap.mapWithKey
                  ( \cred spkHash ->
                      ( \c ->
                          ( C.StakePoolKeyHash spkHash
                          , C.Lovelace $ coerce $ Ledger.fromCompact c
                          )
                      )
                        <$> VMap.lookup cred stakes
                  )
                  delegations

{- | Get Nonce per epoch given an extended ledger state. The Nonce is only available starting at
 Shelley era. Byron era has the neutral nonce.
-}
getEpochNonce :: O.ExtLedgerState (O.CardanoBlock O.StandardCrypto) -> Ledger.Nonce
getEpochNonce extLedgerState' =
  case O.headerStateChainDep (O.headerState extLedgerState') of
    O.ChainDepStateByron _ -> Ledger.NeutralNonce
    O.ChainDepStateShelley st -> extractNonce st
    O.ChainDepStateAllegra st -> extractNonce st
    O.ChainDepStateMary st -> extractNonce st
    O.ChainDepStateAlonzo st -> extractNonce st
    O.ChainDepStateBabbage st -> extractNoncePraos st
    O.ChainDepStateConway st -> extractNoncePraos st
  where
    extractNonce :: O.TPraosState c -> Ledger.Nonce
    extractNonce =
      Shelley.ticknStateEpochNonce . Shelley.csTickn . O.tpraosStateChainDepState

    extractNoncePraos :: O.PraosState c -> Ledger.Nonce
    extractNoncePraos = O.praosStateEpochNonce

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

getBlockNo :: C.BlockInMode C.CardanoMode -> C.BlockNo
getBlockNo (C.BlockInMode block _eraInMode) =
  case C.getBlockHeader block of C.BlockHeader _ _ b -> b

buildNextEpochState
  :: ExtLedgerConfig -> EpochState -> C.BlockInMode C.CardanoMode -> IO EpochState
buildNextEpochState extLedgerCfg currentState block =
  let currentLedgerState' = extLedgerState currentState
      applyBlock = CE.applyBlockExtLedgerState extLedgerCfg C.QuickValidation
   in case applyBlock block currentLedgerState' of
        Left err -> throw . Core.IndexerInternalError . Text.pack . show $ err
        Right res -> pure $ EpochState res (getBlockNo block)
