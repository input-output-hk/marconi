{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Marconi.ChainIndex.Experimental.Indexers.EpochState (
  -- * Events
  EpochState (EpochState),
  ExtLedgerState,
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
  NodeConfig (..),
  EpochStateWorkerConfig (..),
  StandardEpochStateIndexer,
  WorkerState,
  mkEpochStateIndexer,
  mkEpochStateWorker,

  -- * Queries
  ActiveSDDByEpochNoQuery,
  NonceByEpochNoQuery,
) where

import Cardano.Api (GenesisConfig)
import Cardano.Api qualified as C
import Cardano.Api.Extended.ExtLedgerState qualified as CE
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Trace qualified as BM
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Ledger.UMap qualified as Ledger
import Cardano.Protocol.TPraos.API qualified as Shelley
import Cardano.Protocol.TPraos.Rules.Tickn qualified as Shelley
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Applicative (Alternative (empty))
import Control.Arrow ((<<<))
import Control.Concurrent qualified as Con
import Control.Exception (throw, throwIO)
import Control.Lens (Lens', (%~), (&), (-=), (.=), (.~), (^.))
import Control.Lens qualified as Lens
import Control.Monad (foldM, when, (<=<))
import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Except (runExceptT)
import Control.Monad.State.Strict (MonadState)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (bimap)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Short qualified as BS.Short
import Data.Coerce (coerce)
import Data.Data (Proxy (Proxy))
import Data.Foldable (Foldable (toList))
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.VMap (VMap)
import Data.VMap qualified as VMap
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Experimental.Extract.WithDistance (
  WithDistance (WithDistance),
 )
import Marconi.ChainIndex.Experimental.Indexers.Orphans ()
import Marconi.ChainIndex.Experimental.Indexers.SyncHelper qualified as Sync
import Marconi.ChainIndex.Experimental.Indexers.Worker (
  StandardSQLiteIndexer,
  StandardWorkerConfig (logger, securityParamConfig),
  eventExtractor,
  mkStandardIndexer,
  workerName,
 )
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (SecurityParam (SecurityParam))
import Marconi.Core (IsSync (lastSyncPoint))
import Marconi.Core qualified as Core
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Config qualified as O
import Ouroboros.Consensus.HeaderValidation qualified as O
import Ouroboros.Consensus.Ledger.Extended qualified as O
import Ouroboros.Consensus.Node.NetworkProtocolVersion qualified as O
import Ouroboros.Consensus.Node.Serialisation qualified as O
import Ouroboros.Consensus.Protocol.Praos qualified as O
import Ouroboros.Consensus.Protocol.TPraos qualified as O
import Ouroboros.Consensus.Shelley.Ledger qualified as O
import Ouroboros.Consensus.Storage.Serialisation qualified as O
import Prettyprinter (pretty)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Read qualified as Text

type ExtLedgerState = O.ExtLedgerState (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
type ExtLedgerConfig = O.ExtLedgerCfg (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
type instance Core.Point (C.BlockInMode C.CardanoMode) = C.ChainPoint
type instance Core.Point EpochState = C.ChainPoint
type instance Core.Point (Maybe ExtLedgerState, C.BlockInMode C.CardanoMode) = C.ChainPoint

-- | Base event used to store the 'ExtLedgerState'
data EpochState = EpochState {extLedgerState :: ExtLedgerState, blockNo :: C.BlockNo}

-- | Event for @Nonce@ storage
data EpochNonce = EpochNonce
  { _nonceEpochNo :: !C.EpochNo
  , _nonceNonce :: !Ledger.Nonce
  , _nonceBlockNo :: !C.BlockNo
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (SQL.FromRow, SQL.ToRow, FromJSON, ToJSON)

instance SQL.ToRow (Core.Timed C.ChainPoint EpochNonce) where
  toRow epochNonce =
    SQL.toRow (epochNonce ^. Core.event)
      <> SQL.toRow (epochNonce ^. Core.point)

instance SQL.FromRow (Core.Timed C.ChainPoint EpochNonce) where
  fromRow = do
    nonce <- SQL.fromRow
    point <- SQL.fromRow
    pure $ Core.Timed point nonce

type instance Core.Point EpochNonce = C.ChainPoint

Lens.makeLenses ''EpochNonce

-- | Event for @SDD@ storage
data EpochSDD = EpochSDD
  { _sddEpochNo :: !C.EpochNo
  , _sddPoolId :: !C.PoolId
  , _sddLovelace :: !C.Lovelace
  , _sddBlockNo :: !C.BlockNo
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (SQL.FromRow, SQL.ToRow, FromJSON, ToJSON)

type instance Core.Point (NonEmpty EpochSDD) = C.ChainPoint

instance SQL.ToRow (Core.Timed C.ChainPoint EpochSDD) where
  toRow epochSDD =
    SQL.toRow (epochSDD ^. Core.event)
      <> SQL.toRow (epochSDD ^. Core.point)

instance SQL.FromRow (Core.Timed C.ChainPoint EpochSDD) where
  fromRow = do
    sdd <- SQL.fromRow
    point <- SQL.fromRow
    pure $ Core.Timed point sdd

-- | Metadata used to cerate 'EpochStateIndexer' filenames
data EpochMetadata = EpochMetadata
  { metadataBlockNo :: Maybe C.BlockNo
  , metadataChainpoint :: C.ChainPoint
  }
  deriving (Show)

type instance Core.Point EpochSDD = C.ChainPoint

Lens.makeLenses ''EpochSDD

type LedgerStateFileIndexer = Core.FileIndexer EpochMetadata EpochState

type BlockFileIndexer = Core.FileIndexer EpochMetadata (C.BlockInMode C.CardanoMode)

type NonceIndexer = StandardSQLiteIndexer IO EpochNonce

-- As we rarely index anything with this indexer, we don't need catchup
type SDDIndexer = StandardSQLiteIndexer IO (NonEmpty EpochSDD)

-- | The inner state of the 'EpochStateIndexer'
data EpochStateIndexerState event = EpochStateIndexerState
  { _stateEpochStateIndexer :: LedgerStateFileIndexer
  , _stateBlockIndexer :: BlockFileIndexer
  , _stateEpochNonceIndexer :: NonceIndexer
  , _stateEpochSDDIndexer :: SDDIndexer
  , _stateSnapshots :: [C.ChainPoint]
  }

Lens.makeLenses ''EpochStateIndexerState

-- | The configuration of the 'EpochStateIndexer'
newtype EpochStateIndexerConfig event = EpochStateIndexerConfig
  { _configGenesisConfig :: C.GenesisConfig
  }

Lens.makeLenses ''EpochStateIndexerConfig

{- | The main type of the epoch state indexer, it contains both the indexers it operates and its
configuration
-}
data EpochStateIndexer event = EpochStateIndexer
  { _epochStateIndexerState :: EpochStateIndexerState event
  , _epochStateIndexerConfig :: EpochStateIndexerConfig event
  }

Lens.makeLenses ''EpochStateIndexer

type StandardEpochStateIndexer = EpochStateIndexer (WithDistance (C.BlockInMode C.CardanoMode))

-- | The state maintained by the indexer to decide how to handle incoming events
data WorkerState = WorkerState
  { _lastEpochState :: EpochState
  -- ^ The last computed ledger state
  , _accessToIndexer
      :: Con.MVar (EpochStateIndexer (WithDistance (Maybe ExtLedgerState, C.BlockInMode C.CardanoMode)))
  -- ^ allow query to the indexer
  , _blocksToNextSnapshot :: Word
  -- ^ Number of blocks until the next snapshot
  }

Lens.makeLenses ''WorkerState

epochNonceIndexer :: Lens' (EpochStateIndexer event) NonceIndexer
epochNonceIndexer = epochStateIndexerState . stateEpochNonceIndexer

epochSDDIndexer :: Lens' (EpochStateIndexer event) SDDIndexer
epochSDDIndexer = epochStateIndexerState . stateEpochSDDIndexer

epochStateIndexer :: Lens' (EpochStateIndexer event) LedgerStateFileIndexer
epochStateIndexer = epochStateIndexerState . stateEpochStateIndexer

blockIndexer :: Lens' (EpochStateIndexer event) BlockFileIndexer
blockIndexer = epochStateIndexerState . stateBlockIndexer

genesisConfig :: Lens' (EpochStateIndexer event) C.GenesisConfig
genesisConfig = epochStateIndexerConfig . configGenesisConfig

snapshots :: Lens' (EpochStateIndexer event) [C.ChainPoint]
snapshots = epochStateIndexerState . stateSnapshots

extLedgerConfig :: EpochStateIndexer event -> ExtLedgerConfig
extLedgerConfig = Lens.views genesisConfig CE.mkExtLedgerConfig

initialEpochState :: EpochStateIndexer event -> EpochState
initialEpochState = Lens.views genesisConfig (flip EpochState 0 . CE.mkInitExtLedgerState)

toEpochNonce :: WithDistance EpochState -> Maybe (WithDistance (Maybe EpochNonce))
toEpochNonce (WithDistance d epochState) = do
  let ledgerState = extLedgerState epochState
  epochNo <- getEpochNo ledgerState
  pure $ WithDistance d $ Just $ EpochNonce epochNo (getEpochNonce ledgerState) (blockNo epochState)

toEpochSDD :: WithDistance EpochState -> Maybe (WithDistance (Maybe (NonEmpty EpochSDD)))
toEpochSDD (WithDistance d epochState) = fmap (WithDistance d) $ Just . NonEmpty.nonEmpty $ do
  let ledgerState = extLedgerState epochState
  epochNo <- toList $ getEpochNo ledgerState
  (poolId, lovelace) <- Map.toList $ getStakeMap $ extLedgerState epochState
  pure $ EpochSDD epochNo poolId lovelace (blockNo epochState)

newtype NodeConfig = NodeConfig
  { nodeConfig :: FilePath
  -- ^ node config path
  }

mkEpochStateIndexer
  :: (MonadIO n)
  => StandardWorkerConfig IO a b
  -> GenesisConfig
  -> FilePath
  -> n
      ( EpochStateIndexer (WithDistance (Maybe ExtLedgerState, C.BlockInMode C.CardanoMode))
      )
mkEpochStateIndexer workerCfg genesisCfg rootDir = do
  let securityParam' = securityParamConfig workerCfg
      config = EpochStateIndexerConfig genesisCfg
      extLedgerCfg = CE.mkExtLedgerConfig genesisCfg
      configCodec = O.configCodec . O.getExtLedgerCfg $ extLedgerCfg
  liftIO $ createDirectoryIfMissing True rootDir
  let epochSDDConfig = workerCfg{logger = BM.appendName "epochSDD" $ logger workerCfg}
  epochSDDIndexer' <-
    mkStandardIndexer epochSDDConfig <$> buildEpochSDDIndexer (rootDir </> "epochSDD.db")
  let epochNonceConfig = workerCfg{logger = BM.appendName "nonce" $ logger workerCfg}
  epochNonceIndexer' <-
    mkStandardIndexer epochNonceConfig <$> buildEpochNonceIndexer (rootDir </> "epochNonce.db")
  epochStateIndexer' <-
    buildEpochStateIndexer
      configCodec
      securityParam'
      (rootDir </> "epochState")
  epochBlocksIndexer <-
    buildBlockIndexer
      configCodec
      securityParam'
      (rootDir </> "epochBlocks")
  let state =
        EpochStateIndexerState
          epochStateIndexer'
          epochBlocksIndexer
          epochNonceIndexer'
          epochSDDIndexer'
          []

  let indexer = EpochStateIndexer state config
  pure indexer

data EpochStateWorkerConfig = EpochStateWorkerConfig
  { indexerConfig :: GenesisConfig
  , epochSnapshotInterval :: Word
  }

mkEpochStateWorker
  :: forall m input
   . ( MonadIO m
     , MonadCatch m
     )
  => StandardWorkerConfig IO input (C.BlockInMode C.CardanoMode)
  -- ^ General configuration of the indexer (mostly for logging purpose)
  -> EpochStateWorkerConfig
  -> FilePath
  -> m
      ( Core.WorkerIndexer
          IO
          (WithDistance input)
          (WithDistance (Maybe ExtLedgerState, C.BlockInMode C.CardanoMode))
          EpochStateIndexer
      )
mkEpochStateWorker workerConfig epochStateConfig rootDir = do
  indexer <- mkEpochStateIndexer workerConfig (indexerConfig epochStateConfig) rootDir
  workerState <- liftIO $ Con.newMVar indexer
  lastStable <- Core.lastStablePoint indexer
  epochState <-
    restoreLedgerState (Just lastStable) indexer
      `catch` ( \(_ :: Core.IndexerError) ->
                  liftIO . throwIO $ Core.IndexerInternalError "can't restore ledger state"
              )
  let extLedgerCfg = extLedgerConfig indexer

      snapshotInterval = epochSnapshotInterval epochStateConfig
      initialState = pure $ WorkerState epochState workerState snapshotInterval

      mapOneEvent
        :: (MonadState WorkerState n, MonadIO n)
        => Maybe (WithDistance input)
        -> n (Maybe (WithDistance (Maybe ExtLedgerState, C.BlockInMode C.CardanoMode)))
      mapOneEvent Nothing = pure Nothing
      mapOneEvent (Just (WithDistance d e)) = runMaybeT $ do
        let applyBlock = CE.applyBlockExtLedgerState extLedgerCfg C.QuickValidation
            extract = eventExtractor workerConfig
            resetBlocksToSnapshot = blocksToNextSnapshot .= snapshotInterval
        block <- MaybeT $ liftIO $ extract e
        let newBlockNo = getBlockNo block
        EpochState currentLedgerState' _block <- Lens.use lastEpochState
        case applyBlock block currentLedgerState' of
          Left err -> liftIO . throwIO . Core.IndexerInternalError . Text.pack . show $ err
          Right res -> do
            lastEpochState .= EpochState res newBlockNo
            blocksToNextSnapshot -= 1
            snapshotTime <- (== 0) <$> Lens.use blocksToNextSnapshot
            let isNewEpoch = getEpochNo currentLedgerState' /= getEpochNo res
                isVolatile = SecurityParam d < securityParamConfig workerConfig
                snapshotEpoch = (snapshotTime && isVolatile) || isNewEpoch
            when (snapshotTime || isNewEpoch) resetBlocksToSnapshot
            if
              | snapshotEpoch -> pure $ WithDistance d (Just res, block)
              | isVolatile -> pure $ WithDistance d (Nothing, block)
              | otherwise -> empty

      processAsEpochState
        :: IO WorkerState
        -> Core.Preprocessor
            IO
            C.ChainPoint
            (WithDistance input)
            (WithDistance (Maybe ExtLedgerState, C.BlockInMode C.CardanoMode))
      processAsEpochState = do
        Core.preprocessorM $ \case
          Core.Index x -> pure . Core.Index <$> traverse mapOneEvent x
          Core.IndexAllDescending xs -> pure . Core.IndexAllDescending <$> traverse (traverse mapOneEvent) xs
          Core.Rollback p -> do
            lastIndexerM <- Lens.use accessToIndexer
            lastIndexer <- liftIO $ Con.readMVar lastIndexerM
            queryResult <- liftIO $ Core.query p Core.EventAtQuery lastIndexer
            case queryResult of
              Nothing -> liftIO . throwIO $ Core.IndexerInternalError "Can't rollback to the given epoch: no event found"
              (Just res) -> do
                lastEpochState .= res
                pure . pure $ Core.Rollback p
          Core.StableAt p -> pure . pure $ Core.StableAt p
          Core.Stop -> pure $ pure Core.Stop

  let eventPreprocessing = processAsEpochState initialState <<< Core.withResume lastStable
  pure $
    Core.WorkerIndexer workerState $
      Core.Worker (workerName workerConfig) workerState eventPreprocessing

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

buildEpochStateIndexer
  :: (MonadIO m)
  => O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> SecurityParam
  -> FilePath
  -> m (Core.FileIndexer EpochMetadata EpochState)
buildEpochStateIndexer codecConfig securityParam' path = do
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
          (Just . flip EpochState blockNo' . snd)
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
        :: Core.Timed (Core.Point EpochState) (Maybe EpochState)
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

buildBlockIndexer
  :: (MonadIO m)
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
        :: Core.Timed (Core.Point EpochState) (Maybe (C.BlockInMode C.CardanoMode))
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
    Nothing -> liftIO . throwIO $ Core.IndexerInternalError "Can't finde block to Node version"
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

buildEpochSDDIndexer
  :: (MonadIO m)
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
              , blockHeaderHash BLOB NOT NULL
              )|]
      sddInsertQuery =
        [sql|INSERT INTO epoch_sdd
                ( epochNo
                , poolId
                , lovelace
                , blockNo
                , slotNo
                , blockHeaderHash
                ) VALUES (?, ?, ?, ?, ?, ?)|]
      insertEvent = [Core.SQLInsertPlan (traverse NonEmpty.toList) sddInsertQuery]
  Sync.mkSyncedSqliteIndexer
    path
    [createSDD]
    [insertEvent]
    [Core.SQLRollbackPlan "epoch_sdd" "slotNo" C.chainPointToSlotNo]

buildEpochNonceIndexer
  :: (MonadIO m)
  => FilePath
  -> m (Core.SQLiteIndexer EpochNonce)
buildEpochNonceIndexer path = do
  let createNonce =
        [sql|CREATE TABLE IF NOT EXISTS epoch_nonce
              ( epochNo INT NOT NULL
              , nonce BLOB NOT NULL
              , blockNo INT NOT NULL
              , slotNo INT NOT NULL
              , blockHeaderHash BLOB NOT NULL
              )|]
      nonceInsertQuery =
        [sql|INSERT INTO epoch_nonce
                ( epochNo
                , nonce
                , blockNo
                , slotNo
                , blockHeaderHash
                ) VALUES (?, ?, ?, ?, ?)|]
      insertEvent = [Core.SQLInsertPlan pure nonceInsertQuery]
  Sync.mkSyncedSqliteIndexer
    path
    [createNonce]
    [insertEvent]
    [Core.SQLRollbackPlan "epoch_nonce" "slotNo" C.chainPointToSlotNo]

getLatestNonEmpty
  :: (MonadIO m)
  => Maybe C.ChainPoint
  -> EpochState
  -> LedgerStateFileIndexer
  -> m (Core.Timed C.ChainPoint EpochState)
getLatestNonEmpty p firstEpochState indexer = do
  let query = maybe Core.queryLatest Core.query
      getLatest [] = Core.Timed Core.genesis firstEpochState
      getLatest (x : _) = x
  result <- runExceptT $ query p Core.latestEvent indexer

  case result of
    Right xs -> pure $ getLatest xs
    Left (Core.AheadOfLastSync partialResult) -> case partialResult of
      Nothing ->
        liftIO . throwIO $
          Core.OtherIndexError $
            "Cant resolve last epochState: No previous result: " <> Text.pack (show $ pretty p)
      Just xs -> pure $ getLatest xs
    Left (Core.IndexerQueryError err) ->
      liftIO . throwIO $
        Core.OtherIndexError $
          "Cant resolve last epochState: " <> Text.pack (show err)
    Left Core.NotStoredAnymore ->
      liftIO . throwIO $
        Core.OtherIndexError
          "Cant resolve last epochState: Not stored anymore"
    Left (Core.SlotNoBoundsInvalid _) ->
      liftIO . throwIO $ Core.OtherIndexError "Invalid bounds"

getBlocksFrom
  :: (MonadIO m)
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
        liftIO . throwIO $
          Core.OtherIndexError $
            "Cant resolve last blocks: No result - ahead of sync - No previous result: "
              <> Text.pack (show $ pretty to)
              <> " head is: "
              <> Text.pack (show $ pretty lastSync)
      Just xs -> do
        lastSync <- Core.lastSyncPoint indexer
        liftIO . throwIO $
          Core.OtherIndexError $
            "Cant resolve last blocks: No result - ahead of sync - Latest results: "
              <> Text.pack (show (Lens.view Core.point <$> xs))
              <> " Head is: "
              <> Text.pack (show $ pretty lastSync)
              <> " Expecting: "
              <> Text.pack (show to)
    Left (Core.IndexerQueryError err) ->
      liftIO . throwIO $
        Core.OtherIndexError $
          "Cant resolve last blocks: " <> Text.pack (show err)
    Left Core.NotStoredAnymore ->
      liftIO . throwIO $
        Core.OtherIndexError
          "Cant resolve last blocks: Not stored anymore"
    Left (Core.SlotNoBoundsInvalid _) ->
      liftIO . throwIO $ Core.OtherIndexError "Invalid bounds"

restoreLedgerState
  :: ( MonadIO m
     , Core.Point event ~ C.ChainPoint
     )
  => Maybe C.ChainPoint
  -> EpochStateIndexer event
  -> m EpochState
restoreLedgerState p indexer = do
  let applyBlocksUpToGivenSlot epochStatePoint closestLedgerState = do
        blocks <- getBlocksFrom epochStatePoint p (indexer ^. blockIndexer)
        liftIO $
          foldM
            (buildNextEpochState $ extLedgerConfig indexer)
            closestLedgerState
            blocks
  Core.Timed epochStatePoint closestLedgerState <-
    getLatestNonEmpty p (initialEpochState indexer) (indexer ^. epochStateIndexer)
  lst <- Core.lastSyncPoint indexer
  if epochStatePoint == fromMaybe lst p
    then pure closestLedgerState
    else applyBlocksUpToGivenSlot epochStatePoint closestLedgerState

instance
  Core.IsIndex
    IO
    (WithDistance (Maybe ExtLedgerState, C.BlockInMode C.CardanoMode))
    EpochStateIndexer
  where
  index (Core.Timed point Nothing) indexer =
    Core.indexVia blockIndexer (Core.Timed point Nothing)
      <=< Core.indexVia epochStateIndexer (Core.Timed point Nothing)
      <=< Core.indexVia epochSDDIndexer (Core.Timed point Nothing)
      <=< Core.indexVia epochNonceIndexer (Core.Timed point Nothing)
      $ indexer
  index (Core.Timed point (Just (WithDistance d (Nothing, block)))) indexer =
    Core.indexVia blockIndexer (Core.Timed point (Just block))
      <=< Core.indexVia epochStateIndexer (Core.Timed point Nothing)
      <=< Core.indexVia epochSDDIndexer (Core.Timed point (Just (WithDistance d Nothing)))
      <=< Core.indexVia epochNonceIndexer (Core.Timed point (Just (WithDistance d Nothing)))
      $ indexer
  index (Core.Timed point (Just (WithDistance d (Just ledgerState, block)))) indexer =
    do
      let blockNo' = getBlockNo block
          epochState = EpochState ledgerState blockNo'
          epochStateWithDistance = WithDistance d epochState
          addSnapshot
            :: (EpochStateIndexer a -> IO (EpochStateIndexer b)) -> EpochStateIndexer a -> IO (EpochStateIndexer b)
          addSnapshot f = fmap (snapshots %~ (point :)) <$> f
      addSnapshot $
        Core.indexVia blockIndexer (Core.Timed point (Just block))
          <=< Core.indexVia epochStateIndexer (Core.Timed point $ Just epochState)
          <=< Core.indexVia epochSDDIndexer (Core.Timed point $ toEpochSDD epochStateWithDistance)
          <=< Core.indexVia epochNonceIndexer (Core.Timed point $ toEpochNonce epochStateWithDistance)
      $ indexer
  rollback p =
    epochStateIndexer (Core.rollback p)
      <=< blockIndexer (Core.rollback p)
      <=< epochSDDIndexer (Core.rollback p)
      <=< epochNonceIndexer (Core.rollback p)

  setLastStablePoint p indexer =
    let (volatile, immutable) = span (> p) $ indexer ^. snapshots
        p' = listToMaybe immutable
        indexer' = indexer & snapshots .~ volatile
        setStablePointOnIndexers point =
          epochStateIndexer (Core.setLastStablePoint point)
            <=< blockIndexer (Core.setLastStablePoint point)
            <=< epochSDDIndexer (Core.setLastStablePoint point)
            <=< epochNonceIndexer (Core.setLastStablePoint point)
     in maybe pure setStablePointOnIndexers p' indexer'

instance
  (MonadIO m, Core.Point event ~ C.ChainPoint)
  => Core.IsSync m event EpochStateIndexer
  where
  lastSyncPoint indexer = do
    lastBlock <- Core.lastSyncPoint $ indexer ^. blockIndexer
    lastEpoch <- Core.lastSyncPoint $ indexer ^. epochStateIndexer
    lastSDD <- Core.lastSyncPoint $ indexer ^. epochSDDIndexer
    lastNonce <- Core.lastSyncPoint $ indexer ^. epochNonceIndexer
    pure $ minimum [lastBlock, lastEpoch, lastSDD, lastNonce]
  lastStablePoint indexer = Core.lastStablePoint $ indexer ^. epochStateIndexer

instance Core.Closeable IO EpochStateIndexer where
  close indexer = do
    Core.close $ indexer ^. epochStateIndexer
    Core.close $ indexer ^. blockIndexer
    Core.close $ indexer ^. epochSDDIndexer
    Core.close $ indexer ^. epochNonceIndexer

newtype ActiveSDDByEpochNoQuery = ActiveSDDByEpochNoQuery C.EpochNo
  deriving newtype (FromJSON, ToJSON)

type instance Core.Result ActiveSDDByEpochNoQuery = [Core.Timed C.ChainPoint EpochSDD]

instance
  (MonadIO m)
  => Core.Queryable m event ActiveSDDByEpochNoQuery Core.SQLiteIndexer
  where
  query = do
    let epochSDDQuery =
          [sql|SELECT epochNo, poolId, lovelace, blockNo, slotNo, blockHeaderHash
            FROM epoch_sdd
            WHERE epochNo == :epochNo
          |]
        -- See Note [Active stake pool delegation query] for why we do 'epochNo - 2' for the query.
        getParams _ (ActiveSDDByEpochNoQuery epochNo) = [":epochNo" SQL.:= epochNo - 2]
    Core.querySyncedOnlySQLiteIndexerWith
      getParams
      (const epochSDDQuery)
      (const id)

instance
  ( MonadIO m
  , Core.Point event ~ C.ChainPoint
  )
  => Core.Queryable m event ActiveSDDByEpochNoQuery EpochStateIndexer
  where
  query = Core.queryVia epochSDDIndexer

instance
  ( MonadIO m
  , Core.Point event ~ C.ChainPoint
  )
  => Core.Queryable m event (Core.EventAtQuery EpochState) EpochStateIndexer
  where
  query cp _ = fmap Just . restoreLedgerState (Just cp)

newtype NonceByEpochNoQuery = NonceByEpochNoQuery C.EpochNo
  deriving newtype (FromJSON, ToJSON)

type instance Core.Result NonceByEpochNoQuery = Maybe (Core.Timed C.ChainPoint EpochNonce)

instance
  (MonadIO m)
  => Core.Queryable m event NonceByEpochNoQuery Core.SQLiteIndexer
  where
  query = do
    let epochSDDQuery =
          [sql|SELECT epochNo, nonce, blockNo, slotNo, blockHeaderHash
                 FROM epoch_nonce
                 WHERE epochNo = :epochNo
              |]
        getParams _ (NonceByEpochNoQuery epochNo) = [":epochNo" SQL.:= epochNo]
    Core.querySyncedOnlySQLiteIndexerWith
      getParams
      (const epochSDDQuery)
      (const listToMaybe)

instance
  ( MonadIO m
  , Core.Point event ~ C.ChainPoint
  )
  => Core.Queryable m event NonceByEpochNoQuery EpochStateIndexer
  where
  query = Core.queryVia epochNonceIndexer

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

getBlockNo :: C.BlockInMode C.CardanoMode -> C.BlockNo
getBlockNo (C.BlockInMode block _eraInMode) =
  case C.getBlockHeader block of C.BlockHeader _ _ b -> b

buildNextEpochState
  :: ExtLedgerConfig -> EpochState -> C.BlockInMode C.CardanoMode -> IO EpochState
buildNextEpochState extLedgerCfg currentState block =
  let currentLedgerState' = extLedgerState currentState
      applyBlock = CE.applyBlockExtLedgerState extLedgerCfg C.QuickValidation
   in do
        case applyBlock block currentLedgerState' of
          Left err -> throw . Core.IndexerInternalError . Text.pack . show $ err
          Right res -> pure $ EpochState res (getBlockNo block)
