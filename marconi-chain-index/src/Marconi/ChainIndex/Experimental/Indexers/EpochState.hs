{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
  mkEpochStateIndexer,
  mkEpochStateWorker,
) where

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
import Control.Exception (throw)
import Control.Lens (Lens', (%~), (&), (-=), (.=), (.~), (^.))
import Control.Lens qualified as Lens
import Control.Monad (foldM, when, (<=<))
import Control.Monad.Cont (MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (MonadState)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
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
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.VMap (VMap)
import Data.VMap qualified as VMap
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField qualified as SQL
import Debug.Trace qualified
import GHC.Generics (Generic)
import Marconi.ChainIndex.Experimental.Extract.WithDistance (
  WithDistance (WithDistance),
 )
import Marconi.ChainIndex.Experimental.Indexers.Orphans ()
import Marconi.ChainIndex.Experimental.Indexers.SyncHelper qualified as Sync
import Marconi.ChainIndex.Experimental.Indexers.Worker (
  StandardIndexer,
  StandardWorkerConfig (logger, securityParamConfig),
  eventExtractor,
  mkStandardIndexer,
  workerName,
 )
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (SecurityParam (SecurityParam))
import Marconi.Core.Experiment qualified as Core
import Marconi.Core.Experiment.Indexer.FileIndexer (EventInfo (fileMetadata))
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Config qualified as O
import Ouroboros.Consensus.HeaderValidation qualified as O
import Ouroboros.Consensus.Ledger.Extended qualified as O
import Ouroboros.Consensus.Protocol.Praos qualified as O
import Ouroboros.Consensus.Protocol.TPraos qualified as O
import Ouroboros.Consensus.Shelley.Ledger qualified as O
import Ouroboros.Consensus.Storage.Serialisation qualified as O
import System.Directory (createDirectoryIfMissing)
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

-- | Metadata used to cerate 'EpochStateIndexer' filenames
data EpochMetadata = EpochMetadata
  { metadataBlockNo :: Maybe C.BlockNo
  , metadataChainpoint :: C.ChainPoint
  }

type instance Core.Point EpochSDD = C.ChainPoint

Lens.makeLenses ''EpochSDD

type LedgerStateFileIndexer = Core.FileIndexer EpochMetadata EpochState

type BlockFileIndexer = Core.FileIndexer EpochMetadata (C.BlockInMode C.CardanoMode)

-- | The inner state of the 'EpochStateIndexer'
data EpochStateIndexerState event = EpochStateIndexerState
  { _stateEpochStateIndexer :: LedgerStateFileIndexer
  , _stateBlockIndexer :: BlockFileIndexer
  , _stateEpochNonceIndexer :: StandardIndexer IO Core.SQLiteIndexer EpochNonce
  , _stateEpochSDDIndexer :: StandardIndexer IO Core.SQLiteIndexer (NonEmpty EpochSDD)
  , _stateSnapshots :: [C.ChainPoint]
  }

Lens.makeLenses ''EpochStateIndexerState

-- | The configuration of the 'EpochStateIndexer'
data EpochStateIndexerConfig event = EpochStateIndexerConfig
  { _configGenesisConfig :: C.GenesisConfig
  , _configSecurityParam :: SecurityParam
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
  , _blocksToNextSnapshot :: Word
  -- ^ Number of blocks until the next snapshot
  }

Lens.makeLenses ''WorkerState

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

genesisConfig :: Lens' (EpochStateIndexer event) C.GenesisConfig
genesisConfig = epochStateIndexerConfig . configGenesisConfig

securityParam :: Lens' (EpochStateIndexer event) SecurityParam
securityParam = epochStateIndexerConfig . configSecurityParam

snapshots :: Lens' (EpochStateIndexer event) [C.ChainPoint]
snapshots = epochStateIndexerState . stateSnapshots

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

newtype NodeConfig = NodeConfig
  { nodeConfig :: FilePath
  -- ^ node config path
  }

mkEpochStateIndexer
  :: (MonadIO n, MonadError Core.IndexerError n)
  => StandardWorkerConfig IO a b
  -> NodeConfig
  -> FilePath
  -> n
      ( Core.WithResume EpochStateIndexer (WithDistance (Maybe ExtLedgerState, C.BlockInMode C.CardanoMode))
      )
mkEpochStateIndexer workerCfg cfg rootDir = do
  genesisCfg <- readGenesisFile (nodeConfig cfg)
  let securityParam' = securityParamConfig workerCfg
      config = EpochStateIndexerConfig genesisCfg securityParam'
      extLedgerCfg = CE.mkExtLedgerConfig genesisCfg
      configCodec = O.configCodec . O.getExtLedgerCfg $ extLedgerCfg
  liftIO $ createDirectoryIfMissing True rootDir
  let epochSDDConfig = workerCfg{logger = BM.appendName "epochSDD" $ logger workerCfg}
  epochSDDIndexer' <-
    mkStandardIndexer epochSDDConfig =<< buildEpochSDDIndexer (rootDir </> "epochSDD.db")
  let epochNonceConfig = workerCfg{logger = BM.appendName "nonce" $ logger workerCfg}
  epochNonceIndexer' <-
    mkStandardIndexer epochNonceConfig =<< buildEpochNonceIndexer (rootDir </> "epochNonce.db")
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
  Core.withResume indexer

data EpochStateWorkerConfig = EpochStateWorkerConfig
  { indexerConfig :: NodeConfig
  , epochSnapshotInterval :: Word
  }

mkEpochStateWorker
  :: forall m input
   . (MonadIO m, MonadError Core.IndexerError m)
  => StandardWorkerConfig IO input (C.BlockInMode C.CardanoMode)
  -- ^ General configuration of the indexer (mostly for logging purpose)
  -> EpochStateWorkerConfig
  -> FilePath
  -> m
      ( Core.WorkerIndexer
          IO
          (WithDistance input)
          (WithDistance (Maybe ExtLedgerState, C.BlockInMode C.CardanoMode))
          (Core.WithResume EpochStateIndexer)
      )
mkEpochStateWorker workerConfig epochStateConfig rootDir = do
  indexer <- mkEpochStateIndexer workerConfig (indexerConfig epochStateConfig) rootDir
  ledgerStateE <- runExceptT $ restoreLedgerState Nothing (indexer ^. Core.resumedIndexer)
  epochState <- case ledgerStateE of
    Left _err -> throwError $ Core.IndexerInternalError "can't restore ledger state"
    Right res -> pure res
  let extLedgerCfg = extLedgerConfig $ indexer ^. Core.resumedIndexer

      snapshotInterval = epochSnapshotInterval epochStateConfig
      initialState = pure $ WorkerState epochState snapshotInterval

      mapOneEvent
        :: (MonadState WorkerState n, MonadError Core.IndexerError n, MonadIO n)
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
          Left err -> throwError . Core.IndexerInternalError . Text.pack . show $ err
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
        :: ExceptT Core.IndexerError IO WorkerState
        -> Core.Transformer
            (ExceptT Core.IndexerError IO)
            C.ChainPoint
            (WithDistance input)
            (WithDistance (Maybe ExtLedgerState, C.BlockInMode C.CardanoMode))
      processAsEpochState = Core.transformerM $ \case
        Core.Index x -> do
          pure . Core.Index <$> traverse mapOneEvent x
        Core.IndexAllDescending xs ->
          pure . Core.IndexAllDescending <$> traverse (traverse mapOneEvent) xs
        Core.Rollback p -> do
          queryResult <- lift $ runExceptT $ Core.query p Core.EventAtQuery indexer
          case queryResult of
            Left _err -> throwError $ Core.IndexerInternalError "Can't rollback to the given epoch"
            Right Nothing -> throwError $ Core.IndexerInternalError "Can't rollback to the given epoch"
            Right (Just res) -> do
              lastEpochState .= res
              pure . pure $ Core.Rollback p
        Core.StableAt p -> pure . pure $ Core.StableAt p
        Core.Stop -> pure $ pure Core.Stop

  Core.createWorker (workerName workerConfig) (processAsEpochState initialState) indexer

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
  :: (MonadIO m, MonadError Core.IndexerError m)
  => O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> SecurityParam
  -> FilePath
  -> m (Core.FileIndexer EpochMetadata EpochState)
buildEpochStateIndexer codecConfig securityParam' path = do
  let serialiseLedgerState =
        BS.toStrict
          . CBOR.toLazyByteString
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
        -> [EventInfo EpochMetadata]
        -> [EventInfo EpochMetadata]
      immutableEpochs timedEvent eventsInfo =
        let sortedEvents = sortOn (metadataBlockNo . fileMetadata) eventsInfo
            lastBlockNo = maybe 0 blockNo $ timedEvent ^. Core.event
            blockDepth = (\(C.BlockNo b) -> b) . (lastBlockNo -)
            isImmutable =
              maybe True ((> securityParam') . fromIntegral . blockDepth) . metadataBlockNo . fileMetadata
            immutableEvents = takeWhile isImmutable sortedEvents
         in case immutableEvents of
              [] -> []
              _ -> init immutableEvents
  Core.mkFileIndexer
    path
    (Core.FileStorageConfig False immutableEpochs (comparing metadataBlockNo))
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
          then pure C.ChainPointAtGenesis
          else do
            s <- C.SlotNo <$> CBOR.decodeWord64
            bhh <- C.HeaderHash . BS.Short.toShort <$> CBOR.decodeBytes
            pure $ C.ChainPoint s bhh
   in case CBOR.deserialiseFromBytes pointDecoding . BS.fromStrict $ bs of
        Right (other, res) | BS.Lazy.null other -> Debug.Trace.trace "EpochStatePoint" $ Debug.Trace.traceShowId $ Right res
        _other -> Left "Can't read chainpoint"

buildBlockIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> SecurityParam
  -> FilePath
  -> m (Core.FileIndexer EpochMetadata (C.BlockInMode C.CardanoMode))
buildBlockIndexer codecConfig securityParam' path = do
  let serialiseBlock =
        BS.toStrict . CBOR.toLazyByteString . O.encodeDisk codecConfig . C.toConsensusBlock
      deserialiseBlock (EpochMetadata Nothing _) = const (Right Nothing)
      deserialiseBlock _ =
        bimap
          (Text.pack . show)
          (Just . C.fromConsensusBlock C.CardanoMode)
          . fmap (\(bs', decode) -> decode bs')
          . CBOR.deserialiseFromBytes (O.decodeDisk codecConfig)
          . BS.fromStrict
      blockNoAsText = maybe "" (Text.pack . show . (\(C.BlockNo b) -> b) . getBlockNo)
      metadataAsText (Core.Timed C.ChainPointAtGenesis evt) = [blockNoAsText evt]
      metadataAsText (Core.Timed chainPoint evt) =
        let chainPointTexts = case chainPoint of
              C.ChainPoint (C.SlotNo slotNo) blockHeaderHash ->
                [Text.pack $ show slotNo, C.serialiseToRawBytesHexText blockHeaderHash]
         in blockNoAsText evt : chainPointTexts
      immutableBlocks
        :: Core.Timed (Core.Point EpochState) (Maybe (C.BlockInMode C.CardanoMode))
        -> [EventInfo EpochMetadata]
        -> [EventInfo EpochMetadata]
      immutableBlocks timedEvent eventsInfo =
        let sortedEvents = sortOn (metadataBlockNo . fileMetadata) eventsInfo
            lastBlockNo = maybe 0 getBlockNo $ timedEvent ^. Core.event
            blockDepth = (\(C.BlockNo b) -> b) . (lastBlockNo -)
            isImmutable =
              maybe True ((> securityParam') . fromIntegral . blockDepth) . metadataBlockNo . fileMetadata
            immutableEvents = takeWhile isImmutable sortedEvents
         in case immutableEvents of
              [] -> []
              _ -> init immutableEvents
  Core.mkFileIndexer
    path
    (Core.FileStorageConfig True immutableBlocks (comparing metadataBlockNo))
    (Core.FileBuilder "block" "cbor" metadataAsText serialiseBlock serialisePoint)
    (Core.EventBuilder deserialiseMetadata metadataChainpoint deserialiseBlock deserialisePoint)

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
  Sync.mkSyncedSqliteIndexer
    path
    [createSDD]
    [insertEvent]
    [Core.SQLRollbackPlan "epoch_sdd" "slotNo" C.chainPointToSlotNo]

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
  Sync.mkSyncedSqliteIndexer
    path
    [createNonce]
    [insertEvent]
    [Core.SQLRollbackPlan "epoch_nonce" "slotNo" C.chainPointToSlotNo]

performSnapshots
  :: Core.Timed C.ChainPoint (Maybe (WithDistance (Maybe ExtLedgerState, C.BlockInMode C.CardanoMode)))
  -> EpochStateIndexer event
  -> ExceptT
      Core.IndexerError
      IO
      (Maybe (Core.Timed C.ChainPoint (Maybe (WithDistance EpochState))), EpochStateIndexer event)
performSnapshots (Core.Timed point evtWithDistance) indexer = do
  let (epochStateWithDistance, block) = fromMaybe (Nothing, Nothing) $ do
        WithDistance d (ledgerState, block') <- evtWithDistance
        let securityParam' = indexer ^. securityParam
            isVolatile = securityParam' > SecurityParam d
            blockNo' = getBlockNo block'
            ledgerStateWithDistance' = WithDistance d . flip EpochState blockNo' <$> ledgerState
        pure $ case ledgerStateWithDistance' of
          Nothing -> (Nothing, Just block')
          Just ls -> (Just ls,) $ if isVolatile then Just block' else Nothing
      (epochStateTimed, snapshotEpochState) = case epochStateWithDistance of
        Nothing -> (Nothing, pure)
        Just e@(WithDistance _ e') ->
          let epochStateTimedWithDistance = Core.Timed point $ Just e
              epochStateTimed' = Core.Timed point $ Just e'
              addSnapshot = fmap (snapshots %~ (point :))
           in ( Just epochStateTimedWithDistance
              , addSnapshot . epochStateIndexer (Core.index epochStateTimed')
              )
      snapshotBlock = case block of
        Nothing -> pure
        Just e -> blockIndexer $ Core.index $ Core.Timed point $ Just e
  (epochStateTimed,) <$> (snapshotEpochState <=< snapshotBlock $ indexer)

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
  :: (MonadIO m, MonadError (Core.QueryError (Core.EventAtQuery EpochState)) m)
  => Maybe C.ChainPoint
  -> EpochState
  -> LedgerStateFileIndexer
  -> m (Core.Timed C.ChainPoint EpochState)
getLatestNonEmpty p firstEpochState indexer = do
  let query = maybe Core.queryLatest Core.query
  result <- runExceptT $ query p Core.latestEvent indexer
  case result of
    Left _err -> throwError $ Core.IndexerQueryError "Cant resolve last epochState"
    Right [] -> pure $ Core.Timed Core.genesis firstEpochState
    Right (x : _) -> pure x

getBlocksFrom
  :: (MonadIO m, MonadError (Core.QueryError (Core.EventAtQuery EpochState)) m)
  => C.ChainPoint
  -> Maybe C.ChainPoint
  -> BlockFileIndexer
  -> m [C.BlockInMode C.CardanoMode]
getBlocksFrom from to indexer = do
  let query = maybe Core.queryLatest Core.query
  result <- runExceptT $ query to (Core.EventsFromQuery from) indexer
  case result of
    Left _err -> throwError $ Core.IndexerQueryError "Cant resolve last epochState"
    Right xs -> pure $ Lens.view Core.event <$> xs

restoreLedgerState
  :: ( MonadIO m
     , MonadError (Core.QueryError (Core.EventAtQuery EpochState)) m
     , Core.Point event ~ C.ChainPoint
     )
  => Maybe C.ChainPoint
  -> EpochStateIndexer event
  -> m EpochState
restoreLedgerState p indexer = do
  Core.Timed epochStatePoint closestLedgerState <-
    getLatestNonEmpty p (initialEpochState indexer) (indexer ^. epochStateIndexer)
  mlast <- runExceptT $ Core.lastSyncPoint indexer
  last' <- either (const $ throwError $ Core.IndexerQueryError "can't find lastpoint") pure mlast
  if epochStatePoint == fromMaybe last' p
    then pure closestLedgerState
    else do
      blocks <- getBlocksFrom epochStatePoint p (indexer ^. blockIndexer)
      liftIO $
        foldM
          (buildNextEpochState $ extLedgerConfig indexer)
          closestLedgerState
          blocks

instance
  Core.IsIndex
    (ExceptT Core.IndexerError IO)
    (WithDistance (Maybe ExtLedgerState, C.BlockInMode C.CardanoMode))
    EpochStateIndexer
  where
  index timedEvent indexer = do
    (mEpochStateEvent, indexer') <- indexer & performSnapshots timedEvent
    case mEpochStateEvent of
      Nothing -> storeEmptyEpochStateRelatedInfo (timedEvent ^. Core.point) indexer'
      Just epochStateEvent -> storeEpochStateRelatedInfo epochStateEvent indexer'

  rollback p indexer = do
    let rollbackIndexers =
          epochStateIndexer (Core.rollback p)
            <=< blockIndexer (Core.rollback p)
            <=< epochSDDIndexer (Core.rollback p)
            <=< epochNonceIndexer (Core.rollback p)
    rollbackIndexers indexer

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
  (MonadIO m, MonadError Core.IndexerError m, Core.Point event ~ C.ChainPoint)
  => Core.IsSync m event EpochStateIndexer
  where
  lastSyncPoint indexer = do
    lastBlock <- Core.lastSyncPoint $ indexer ^. blockIndexer
    if lastBlock == Core.genesis
      then Core.lastSyncPoint $ indexer ^. epochStateIndexer
      else pure lastBlock
  lastStablePoint indexer = Core.lastStablePoint $ indexer ^. epochNonceIndexer

instance Core.Closeable (ExceptT Core.IndexerError IO) EpochStateIndexer where
  close indexer = do
    Core.close $ indexer ^. epochStateIndexer
    Core.close $ indexer ^. blockIndexer
    Core.close $ indexer ^. epochSDDIndexer
    Core.close $ indexer ^. epochNonceIndexer

newtype ActiveSDDByEpochNoQuery = ActiveSDDByEpochNoQuery C.EpochNo

type instance Core.Result ActiveSDDByEpochNoQuery = [Core.Timed C.ChainPoint EpochSDD]

instance
  (MonadIO m, MonadError (Core.QueryError ActiveSDDByEpochNoQuery) m)
  => Core.Queryable m (NonEmpty EpochSDD) ActiveSDDByEpochNoQuery Core.SQLiteIndexer
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
  (MonadIO m, MonadError (Core.QueryError ActiveSDDByEpochNoQuery) m)
  => Core.Queryable m EpochSDD ActiveSDDByEpochNoQuery EpochStateIndexer
  where
  query = Core.queryVia epochSDDIndexer

instance
  ( MonadIO m
  , MonadError (Core.QueryError (Core.EventAtQuery EpochState)) m
  , Core.Point a ~ C.ChainPoint
  )
  => Core.Queryable m a (Core.EventAtQuery EpochState) EpochStateIndexer
  where
  query cp _ = fmap Just . restoreLedgerState (Just cp)

newtype NonceByEpochNoQuery = NonceByEpochNoQuery C.EpochNo

type instance Core.Result NonceByEpochNoQuery = Maybe (Core.Timed C.ChainPoint EpochNonce)

instance
  (MonadIO m, MonadError (Core.QueryError NonceByEpochNoQuery) m)
  => Core.Queryable m EpochNonce NonceByEpochNoQuery Core.SQLiteIndexer
  where
  query = do
    let epochSDDQuery =
          [sql|SELECT epochNo, nonce, blockNo, slotNo, blockHeaderHash
                 FROM epoch_nonce
                 WHERE epochNo = ?
              |]
        getParams _ (NonceByEpochNoQuery epochNo) = [":epochNo" SQL.:= epochNo]
    Core.querySyncedOnlySQLiteIndexerWith
      getParams
      (const epochSDDQuery)
      (const listToMaybe)

instance
  (MonadIO m, MonadError (Core.QueryError NonceByEpochNoQuery) m)
  => Core.Queryable m EpochNonce NonceByEpochNoQuery EpochStateIndexer
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
