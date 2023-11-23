{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | This module provides an indexer that provides a snapshot of a range of block from a network.
It serialises the ledger state at the beginning of the snapshot and the serialised version of
these blocks.

From there, you should be able to recreate a stream of events and to initialise any indexer that
would require either no context or an extended ledger state to restart.
-}
module Marconi.ChainIndex.Indexers.ChainSnapshot (
  SnapshotDetails,
  lowerBound,
  upperBound,
  LedgerStateMetadata (..),
  BlockMetadata (..),
  SnapshotState,
  extLedgerState,
  extLedgerConfig,
  snapshotConfig,
  SnapshotIndexer,
  mkSnapshotIndexer,
  mkSnapshotWorker,
  SnapshotWorkerConfig (SnapshotWorkerConfig),
) where

import qualified Cardano.Api as C
import qualified Cardano.Api.Extended.ExtLedgerState as CE (
  applyBlockExtLedgerState,
  mkExtLedgerConfig,
  mkInitExtLedgerState,
 )
import Cardano.Api.Extended.Streaming (BlockEvent (BlockEvent, blockInMode, blockTime, epochNo))
import qualified Cardano.Api.Shelley as C
import Cardano.BM.Trace (Trace)
import qualified Cardano.BM.Trace as BM
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Control.Concurrent as Con
import Control.Lens ((.=), (^.))
import qualified Control.Lens as Lens
import Control.Monad ((<=<))
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (MonadState)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.ByteString.Short as BS.Short
import Data.Data (Proxy (Proxy))
import Data.Fixed (Fixed (MkFixed))
import Data.Foldable (foldrM)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock (secondsToNominalDiffTime)
import Marconi.ChainIndex.Indexers.ExtLedgerStateCoordinator (ExtLedgerConfig, ExtLedgerState)
import qualified Marconi.Core as Core
import qualified Ouroboros.Consensus.Block as O
import qualified Ouroboros.Consensus.Cardano.Block as O
import qualified Ouroboros.Consensus.Config as O
import qualified Ouroboros.Consensus.Ledger.Extended as O
import qualified Ouroboros.Consensus.Node.NetworkProtocolVersion as O
import qualified Ouroboros.Consensus.Node.Serialisation as O
import qualified Ouroboros.Consensus.Storage.Serialisation as O
import System.Directory (createDirectoryIfMissing)
import qualified Text.Read as Text

data SnapshotDetails = SnapshotDetails
  { _lowerBound :: C.BlockNo
  , _upperBound :: C.BlockNo
  }

Lens.makeLenses ''SnapshotDetails

-- | Metadata used to cerate 'EpochStateIndexer' filenames
newtype LedgerStateMetadata = LedgerStateMetadata
  { ledgerMetadataChainpoint :: C.ChainPoint
  }
  deriving stock (Show)

-- | Metadata used to cerate 'EpochStateIndexer' filenames
data BlockMetadata = BlockMetadata
  { blockMetadataBlockNo :: Maybe C.BlockNo
  , blockMetadataChainpoint :: C.ChainPoint
  }
  deriving stock (Show)

type LedgerStateFileIndexer =
  Core.WithTrace IO (Core.FileIndexer LedgerStateMetadata) ExtLedgerState
type BlockIndexer = Core.WithTrace IO (Core.FileIndexer BlockMetadata) BlockEvent

data SnapshotState = SnapshotState
  { _extLedgerState :: Core.Timed C.ChainPoint ExtLedgerState
  -- ^ we need to maintain the ledger state to embed it in our snapshot
  -- start
  , _extLedgerConfig :: ExtLedgerConfig
  -- ^ The extended ledger config used to update the ledger state
  , _snapshotConfig :: SnapshotDetails
  -- ^ The list of remaining snapshot to take
  }

Lens.makeLenses ''SnapshotState

data SnapshotIndexer a = SnapshotIndexer
  { _extLedgerIndexer :: LedgerStateFileIndexer
  , _blockIndexer :: BlockIndexer
  }

Lens.makeLenses ''SnapshotIndexer

data SnapshotWorkerConfig m = SnapshotWorkerConfig
  { workerName :: Text
  , snapshotDetails :: SnapshotDetails
  , logger :: Trace m (Core.IndexerEvent C.ChainPoint)
  , rootDir :: FilePath
  , nodeConfig :: FilePath
  }

mkSnapshotIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => SnapshotWorkerConfig IO
  -> m (SnapshotIndexer (Either ExtLedgerState BlockEvent))
mkSnapshotIndexer cfg = do
  genesisCfg <- readGenesisFile (nodeConfig cfg)
  let extLedgerCfg = CE.mkExtLedgerConfig genesisCfg
      configCodec = O.configCodec . O.getExtLedgerCfg $ extLedgerCfg

  liftIO $ createDirectoryIfMissing True $ rootDir cfg
  ledgerStateIndexer' <-
    Core.withTrace (BM.appendName "ledgerState" $ logger cfg)
      <$> buildLedgerStateIndexer configCodec (rootDir cfg)
  blockIndexer' <-
    Core.withTrace (BM.appendName "blocks" $ logger cfg)
      <$> buildBlockIndexer configCodec (rootDir cfg)
  pure $ SnapshotIndexer ledgerStateIndexer' blockIndexer'

mkSnapshotWorker
  :: forall m input
   . (MonadIO m, MonadError Core.IndexerError m)
  => SnapshotWorkerConfig IO
  -- ^ General configuration of the indexer
  -> (input -> BlockEvent)
  -> m
      ( Core.WorkerIndexer
          IO
          input
          (Either ExtLedgerState BlockEvent)
          (Core.WithTrace IO SnapshotIndexer)
      )
mkSnapshotWorker cfg f = do
  indexer <- Core.withTrace (logger cfg) <$> mkSnapshotIndexer cfg
  genesisCfg <- readGenesisFile (nodeConfig cfg)
  let extLedgerCfg = CE.mkExtLedgerConfig genesisCfg
      initialState =
        pure $
          SnapshotState
            (Core.Timed C.ChainPointAtGenesis (CE.mkInitExtLedgerState genesisCfg))
            extLedgerCfg
            (snapshotDetails cfg)
  workerState <- liftIO $ Con.newMVar indexer
  pure $
    Core.WorkerIndexer workerState $
      Core.Worker (workerName cfg) workerState (processSnapshot f initialState) id

instance
  Core.IsIndex
    (ExceptT Core.IndexerError IO)
    (Either ExtLedgerState BlockEvent)
    SnapshotIndexer
  where
  index (Core.Timed point (Just evt)) = case evt of
    Left ledgerState ->
      Core.indexVia extLedgerIndexer (Core.Timed point (Just ledgerState))
        <=< Core.indexVia blockIndexer (Core.Timed point Nothing)
    Right blockEvent ->
      Core.indexVia extLedgerIndexer (Core.Timed point Nothing)
        <=< Core.indexVia blockIndexer (Core.Timed point (Just blockEvent))
  index (Core.Timed point Nothing) =
    Core.indexVia extLedgerIndexer (Core.Timed point Nothing)
      <=< Core.indexVia blockIndexer (Core.Timed point Nothing)
  rollback point =
    Core.rollbackVia extLedgerIndexer point
      <=< Core.rollbackVia blockIndexer point
  setLastStablePoint point =
    do
      Core.setLastStablePointVia extLedgerIndexer point
      <=< Core.setLastStablePointVia extLedgerIndexer point

instance
  (MonadIO m, Core.Point event ~ C.ChainPoint)
  => Core.IsSync m event SnapshotIndexer
  where
  lastSyncPoint indexer = do
    lastBlock <- Core.lastSyncPointVia blockIndexer indexer
    lastLedgerState <- Core.lastSyncPointVia extLedgerIndexer indexer
    pure $ min lastBlock lastLedgerState
  lastStablePoint = Core.lastStablePointVia blockIndexer

instance Core.Closeable (ExceptT Core.IndexerError IO) SnapshotIndexer where
  close indexer = do
    Core.closeVia extLedgerIndexer indexer
    Core.closeVia blockIndexer indexer

processSnapshot
  :: (input -> BlockEvent)
  -> ExceptT Core.IndexerError IO SnapshotState
  -> Core.Preprocessor
      (ExceptT Core.IndexerError IO)
      C.ChainPoint
      input
      (Either ExtLedgerState BlockEvent)
processSnapshot f = do
  Core.preprocessorM $ \case
    Core.Index evt -> indexEvent (fmap f <$> evt)
    Core.IndexAllDescending xs ->
      let step evt acc = do
            actions <- indexEvent evt
            pure $ acc <> actions
       in foldrM step [] (fmap (fmap f) <$> xs)
    Core.Rollback _p ->
      throwError $
        Core.IndexerInternalError
          "Can't rollback chainSnapshot: it supposed to work on stable part of the chain"
    Core.StableAt p -> pure [Core.StableAt p]
    Core.Stop -> pure [Core.Stop]

type instance Core.Point ExtLedgerState = C.ChainPoint
type instance Core.Point BlockEvent = C.ChainPoint
type instance Core.Point (Either ExtLedgerState BlockEvent) = C.ChainPoint

buildLedgerStateIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> FilePath
  -> m (Core.FileIndexer LedgerStateMetadata ExtLedgerState)
buildLedgerStateIndexer codecConfig path = do
  let serialiseLedgerState =
        CBOR.toStrictByteString
          . O.encodeExtLedgerState
            (O.encodeDisk codecConfig)
            (O.encodeDisk codecConfig)
            (O.encodeDisk codecConfig)
      deserialiseLedgerState _metadata =
        bimap
          (Text.pack . show)
          (Just . snd)
          . CBOR.deserialiseFromBytes
            ( O.decodeExtLedgerState
                (O.decodeDisk codecConfig)
                (O.decodeDisk codecConfig)
                (O.decodeDisk codecConfig)
            )
          . BS.fromStrict
      metadataAsText (Core.Timed C.ChainPointAtGenesis _evt) = []
      metadataAsText (Core.Timed chainPoint _evt) =
        case chainPoint of
          C.ChainPoint (C.SlotNo slotNo) blockHeaderHash ->
            [Text.pack $ show slotNo, C.serialiseToRawBytesHexText blockHeaderHash]
      deserialiseLedgerMetadata [] = do
        Just $ LedgerStateMetadata C.ChainPointAtGenesis
      deserialiseLedgerMetadata [slotNoStr, bhhStr] = do
        slotNo <- fmap C.SlotNo . Text.readMaybe . Text.unpack $ slotNoStr
        bhhBs <- either (const Nothing) Just $ Base16.decode $ Text.encodeUtf8 bhhStr
        bhh <- either (const Nothing) Just $ C.deserialiseFromRawBytes (C.proxyToAsType Proxy) bhhBs
        Just $ LedgerStateMetadata (C.ChainPoint slotNo bhh)
      deserialiseLedgerMetadata _ = Nothing
  Core.mkFileIndexer
    path
    (Just 180_000_000) -- Wait 180s for files to finish writing before terminating
    (Core.FileStorageConfig False (const $ const []) (const $ const EQ))
    (Core.FileBuilder "epochState" "cbor" metadataAsText serialiseLedgerState serialisePoint)
    ( Core.EventBuilder
        deserialiseLedgerMetadata
        ledgerMetadataChainpoint
        deserialiseLedgerState
        deserialisePoint
    )

getBlockNo :: C.BlockInMode C.CardanoMode -> C.BlockNo
getBlockNo (C.BlockInMode block _eraInMode) =
  case C.getBlockHeader block of C.BlockHeader _ _ b -> b

buildBlockIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> FilePath
  -> m (Core.FileIndexer BlockMetadata BlockEvent)
buildBlockIndexer codecConfig path = do
  let blockNoAsText = \case
        Nothing -> mempty
        Just str ->
          pure
            . Text.pack
            . show
            . (\(C.BlockNo b) -> b)
            . getBlockNo
            . blockInMode
            $ str
      metadataAsText (Core.Timed C.ChainPointAtGenesis evt) = blockNoAsText evt
      metadataAsText (Core.Timed chainPoint evt) =
        case chainPoint of
          C.ChainPoint (C.SlotNo slotNo) blockHeaderHash ->
            blockNoAsText evt
              <> [Text.pack $ show slotNo, C.serialiseToRawBytesHexText blockHeaderHash]
      blockNodeToNodeVersionM = do
        nodeToClientVersion <- snd $ O.latestReleasedNodeVersion (Proxy @(O.CardanoBlock O.StandardCrypto))
        Map.lookup nodeToClientVersion $
          O.supportedNodeToClientVersions (Proxy @(O.CardanoBlock O.StandardCrypto))
      parseBlockNo bno = C.BlockNo <$> Text.readMaybe (Text.unpack bno)
      deserialiseMetadata [blockNoStr] =
        Just $ BlockMetadata (parseBlockNo blockNoStr) C.ChainPointAtGenesis
      deserialiseMetadata [blockNoStr, slotNoStr, hashStr] = do
        slotNo <- fmap C.SlotNo . Text.readMaybe $ Text.unpack slotNoStr
        bhhBs <- either (const Nothing) Just $ Base16.decode $ Text.encodeUtf8 hashStr
        headerHash <- either (const Nothing) Just $ C.deserialiseFromRawBytes (C.proxyToAsType Proxy) bhhBs
        Just $ BlockMetadata (parseBlockNo blockNoStr) (C.ChainPoint slotNo headerHash)
      deserialiseMetadata _other = Nothing
      serialiseBlock blockToNode =
        CBOR.toStrictByteString . encodeBlock codecConfig blockToNode
      deserialiseBlock _blockToNode (BlockMetadata Nothing _) = const (Right Nothing)
      deserialiseBlock blockToNode metadata =
        bimap
          (Text.pack . show)
          (Just . snd)
          . CBOR.deserialiseFromBytes (decodeBlock codecConfig blockToNode metadata)
          . BS.fromStrict
  blockNodeToNodeVersion <- case blockNodeToNodeVersionM of
    Nothing -> throwError $ Core.IndexerInternalError "Can't finde block to Node version"
    Just v -> pure v
  Core.mkFileIndexer
    path
    (Just 180_000_000) -- Wait 60s for files to finish writing before terminating
    (Core.FileStorageConfig False (const $ const []) (comparing blockMetadataBlockNo))
    ( Core.FileBuilder
        "block"
        "cbor"
        metadataAsText
        (serialiseBlock blockNodeToNodeVersion)
        serialisePoint
    )
    ( Core.EventBuilder
        deserialiseMetadata
        blockMetadataChainpoint
        (deserialiseBlock blockNodeToNodeVersion)
        deserialisePoint
    )

encodeBlock
  :: O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> O.BlockNodeToClientVersion (O.CardanoBlock O.StandardCrypto)
  -> BlockEvent
  -> CBOR.Encoding
encodeBlock codecConfig blockToNode block =
  O.encodeNodeToClient codecConfig blockToNode (C.toConsensusBlock $ blockInMode block)
    <> CBOR.encodeWord64 ((\(C.EpochNo e) -> e) $ epochNo block)
    <> CBOR.encodeWord64 (fromIntegral $ (\(MkFixed x) -> x) $ nominalDiffTimeToSeconds $ blockTime block)

decodeBlock
  :: O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> O.BlockNodeToClientVersion (O.CardanoBlock O.StandardCrypto)
  -> BlockMetadata
  -> CBOR.Decoder s BlockEvent
decodeBlock codecConfig blockToNode _metadata = do
  block <- C.fromConsensusBlock C.CardanoMode <$> O.decodeNodeToClient codecConfig blockToNode
  epochNo' <- fromIntegral <$> CBOR.decodeWord64
  time' <- secondsToNominalDiffTime . MkFixed . fromIntegral <$> CBOR.decodeWord64
  pure $ BlockEvent block epochNo' time'

indexEvent
  :: (MonadState SnapshotState m, MonadError Core.IndexerError m)
  => Core.Timed C.ChainPoint (Maybe BlockEvent)
  -> m [Core.ProcessedInput C.ChainPoint (Either ExtLedgerState BlockEvent)]
indexEvent (Core.Timed point (Just blockEvent)) = do
  config <- Lens.use extLedgerConfig
  currentExtLedgerState <- Lens.use extLedgerState
  let block = blockInMode blockEvent
      applyBlock = CE.applyBlockExtLedgerState config C.QuickValidation
      blockNo' = getBlockNo block
      startEvent = Core.Index $ Just . Left <$> currentExtLedgerState
      ongoingEvent = Core.Index $ Core.Timed point . Just $ Right blockEvent
  lowerBound' <- Lens.use $ snapshotConfig . lowerBound
  upperBound' <- Lens.use $ snapshotConfig . upperBound
  case applyBlock block (currentExtLedgerState ^. Core.event) of
    Left _err -> throwError $ Core.IndexerInternalError "Can't update ledger state"
    Right newLedgerState -> extLedgerState .= Core.Timed point newLedgerState
  case compare lowerBound' blockNo' of
    GT -> pure []
    EQ -> pure [startEvent, ongoingEvent]
    LT -> pure [ongoingEvent | upperBound' >= blockNo']
indexEvent (Core.Timed _ Nothing) = pure []

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
