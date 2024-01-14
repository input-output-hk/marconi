{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.Cardano.Indexers.SnapshotBlockEvent (
  snapshotBlockEventWorker,
  SnapshotBlockEvent (..),
  SnapshotWorkerConfig (..),
  SnapshotMetadata (..),
  getConfigCodec,

  -- * For testing
  CodecConfig,
  BlockNodeToClientVersion,
  deserialiseSnapshotBlockEvent,
  deserialiseMetadata,
  blockNodeToNodeVersionM,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.ExtLedgerState qualified as CE
import Cardano.Api.Extended.Streaming (BlockEvent (BlockEvent, blockInMode, blockTime, epochNo))
import Cardano.Api.Shelley qualified as C
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Arrow ((<<<))
import Control.Monad.Cont (MonadTrans (lift))
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Short qualified as BS.Short
import Data.Data (Proxy (Proxy))
import Data.Fixed (Fixed (MkFixed), HasResolution (resolution), Pico)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word64)
import Marconi.Cardano.Core.Indexer.Worker (
  StandardWorkerConfig (eventExtractor, logger, workerName),
 )
import Marconi.Cardano.Core.Types (BlockRange, isInBlockRange)
import Marconi.Core qualified as Core
import Ouroboros.Consensus.Block qualified as O
import Ouroboros.Consensus.Byron.Ledger.Block qualified as O
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Config qualified as O
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common qualified as O
import Ouroboros.Consensus.Ledger.Extended qualified as O
import Ouroboros.Consensus.Node.NetworkProtocolVersion qualified as O
import Ouroboros.Consensus.Node.Serialisation qualified as O
import Text.Read qualified as Text

{- | Type which contains the data needed to configure the snapshot
 indexer workers for 'BlockEvent's and 'ExtLedgerState' events.
-}
data SnapshotWorkerConfig input = SnapshotWorkerConfig
  { currentBlockNo :: input -> C.BlockNo
  -- ^ retrieves the current block number from the indexer's input
  , blockRange :: BlockRange
  -- ^ sub-chain to index, passed by the user on the CL
  , nodeConfig :: FilePath
  -- ^ configuration of the Cardano node, passed by the user on the CL
  }

{- | A representation of the names provided for the snapshot files.
 This is used by the file indexer when parsing and unparsing the
 file names.
-}
data SnapshotMetadata = SnapshotMetadata
  { snapshotMetadataBlockNo :: Maybe C.BlockNo
  -- ^ the block which was serialized
  , snapshotMetadataChainpoint :: C.ChainPoint
  -- ^ the chain point which was serialized
  }
  deriving (Show)

newtype SnapshotBlockEvent = SnapshotBlockEvent
  { getBlockEvent :: BlockEvent
  }

type instance Core.Point SnapshotBlockEvent = C.ChainPoint

type CodecConfig = O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
type BlockNodeToClientVersion = O.BlockNodeToClientVersion (O.CardanoBlock O.StandardCrypto)

mkSnapshotBlockEventIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => FilePath
  -> CodecConfig
  -> m (Core.FileIndexer SnapshotMetadata SnapshotBlockEvent)
mkSnapshotBlockEventIndexer path codecConfig = do
  blockNodeToNodeVersion <-
    maybe
      (throwError $ Core.IndexerInternalError "Can't find block to Node version")
      pure
      blockNodeToNodeVersionM
  Core.mkFileIndexer
    path
    Nothing
    fileStorageConfig
    (fileBuilder blockNodeToNodeVersion)
    (eventBuilder blockNodeToNodeVersion)
  where
    fileStorageConfig =
      Core.FileStorageConfig
        False
        (Core.withPartition $ const (,[]))
        (comparing snapshotMetadataBlockNo)
    fileBuilder toVersion =
      Core.FileBuilder
        "block"
        "cbor"
        metadataAsText
        (serializeSnapshotBlockEvent codecConfig toVersion)
        serializeChainPoint
    eventBuilder toVersion =
      Core.EventBuilder
        deserialiseMetadata
        snapshotMetadataChainpoint
        (deserialiseSnapshotBlockEvent codecConfig toVersion)
        deserialiseChainPoint

blockNodeToNodeVersionM
  :: Maybe
      ( O.HardForkNodeToClientVersion
          (O.ByronBlock : O.CardanoShelleyEras O.StandardCrypto)
      )
blockNodeToNodeVersionM = do
  nodeToClientVersion <- snd $ O.latestReleasedNodeVersion (Proxy @(O.CardanoBlock O.StandardCrypto))
  Map.lookup nodeToClientVersion $
    O.supportedNodeToClientVersions (Proxy @(O.CardanoBlock O.StandardCrypto))

getConfigCodec
  :: (MonadIO m, MonadError Core.IndexerError m)
  => FilePath
  -> m CodecConfig
getConfigCodec nodeConfigFile = do
  genesisCfg <- readGenesisFile nodeConfigFile
  let extLedgerCfg = CE.mkExtLedgerConfig genesisCfg
      configCodec = O.configCodec . O.getExtLedgerCfg $ extLedgerCfg
  return configCodec

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
    Left err ->
      throwError
        . Core.IndexerInternalError
        . Text.pack
        . show
        . C.renderGenesisConfigError
        $ err
    Right cfg -> pure cfg

deserialiseSnapshotBlockEvent
  :: CodecConfig
  -> BlockNodeToClientVersion
  -> SnapshotMetadata
  -> BS.ByteString
  -> Either Text (Maybe SnapshotBlockEvent)
deserialiseSnapshotBlockEvent _codecConfig _blockToNode (SnapshotMetadata Nothing _) =
  const (Right Nothing)
deserialiseSnapshotBlockEvent codecConfig blockToNode _ =
  bimap
    (Text.pack . show)
    (Just . SnapshotBlockEvent . snd)
    . CBOR.deserialiseFromBytes (decodeBlock codecConfig blockToNode)

serializeSnapshotBlockEvent
  :: CodecConfig
  -> BlockNodeToClientVersion
  -> SnapshotBlockEvent
  -> Builder
serializeSnapshotBlockEvent codecConfig blockToNode =
  CBOR.toBuilder
    . encodeBlock codecConfig blockToNode
    . getBlockEvent

metadataAsText :: Core.Timed C.ChainPoint (Maybe SnapshotBlockEvent) -> [Text.Text]
metadataAsText (Core.Timed C.ChainPointAtGenesis evt) = blockNoAsText evt
metadataAsText (Core.Timed chainPoint evt) =
  case chainPoint of
    C.ChainPoint (C.SlotNo slotNo) blockHeaderHash ->
      blockNoAsText evt
        <> [Text.pack $ show slotNo, C.serialiseToRawBytesHexText blockHeaderHash]

blockNoAsText :: Maybe SnapshotBlockEvent -> [Text.Text]
blockNoAsText mBlockNo =
  case mBlockNo of
    Nothing -> mempty
    Just str ->
      pure
        . Text.pack
        . show
        . (\(C.BlockNo b) -> b)
        . getBlockNo
        . blockInMode
        . getBlockEvent
        $ str

getBlockNo :: C.BlockInMode C.CardanoMode -> C.BlockNo
getBlockNo (C.BlockInMode block _eraInMode) =
  case C.getBlockHeader block of C.BlockHeader _ _ b -> b

parseBlockNo :: Text.Text -> Maybe O.BlockNo
parseBlockNo bno = C.BlockNo <$> Text.readMaybe (Text.unpack bno)

deserialiseMetadata :: [Text.Text] -> Maybe SnapshotMetadata
deserialiseMetadata [blockNoStr] =
  Just $ SnapshotMetadata (parseBlockNo blockNoStr) C.ChainPointAtGenesis
deserialiseMetadata [blockNoStr, slotNoStr, hashStr] = do
  slotNo <- fmap C.SlotNo . Text.readMaybe $ Text.unpack slotNoStr
  bhhBs <- either (const Nothing) Just $ Base16.decode $ Text.encodeUtf8 hashStr
  headerHash <- either (const Nothing) Just $ C.deserialiseFromRawBytes (C.proxyToAsType Proxy) bhhBs
  Just $ SnapshotMetadata (parseBlockNo blockNoStr) (C.ChainPoint slotNo headerHash)
deserialiseMetadata _other = Nothing

{- | Builds the worker on top of the 'BlockEvent' indexer.
 This is where the events are preprocessed by filtering out
 any blocks which are not in the given 'BlockRange'.
-}
snapshotBlockEventWorker
  :: forall input m n
   . (MonadIO m, MonadError Core.IndexerError m, MonadIO n)
  => StandardWorkerConfig n input SnapshotBlockEvent
  -> SnapshotWorkerConfig input
  -> FilePath
  -> m
      ( Core.WorkerIndexer
          n
          input
          SnapshotBlockEvent
          (Core.WithTrace n (Core.FileIndexer SnapshotMetadata))
      )
snapshotBlockEventWorker standardWorkerConfig snapshotBlockEventWorkerConfig path = do
  codecConfig <- getConfigCodec (nodeConfig snapshotBlockEventWorkerConfig)
  indexer <-
    Core.withTrace (logger standardWorkerConfig) <$> mkSnapshotBlockEventIndexer path codecConfig
  let preprocessor =
        Core.traverseMaybeEvent (lift . eventExtractor standardWorkerConfig)
          <<< inBlockRangePreprocessor
            (currentBlockNo snapshotBlockEventWorkerConfig)
            (blockRange snapshotBlockEventWorkerConfig)
  Core.createWorkerWithPreprocessing (workerName standardWorkerConfig) preprocessor indexer

{- | A preprocessor for applying indexers to only the blocks
 inside a given 'BlockRange'.
-}
inBlockRangePreprocessor
  :: (Monad m)
  => (a -> C.BlockNo)
  -> BlockRange
  -> Core.Preprocessor m C.ChainPoint a a
inBlockRangePreprocessor toBlockNo br =
  Core.scanMaybeEvent filterWithinBlockRange Nothing
  where
    filterWithinBlockRange input =
      if isInBlockRange (toBlockNo input) br
        then pure . Just $ input
        else pure Nothing

encodeBlock
  :: CodecConfig
  -> BlockNodeToClientVersion
  -> BlockEvent
  -> CBOR.Encoding
encodeBlock codecConfig blockToNode block =
  O.encodeNodeToClient codecConfig blockToNode (C.toConsensusBlock $ blockInMode block)
    <> CBOR.encodeWord64 ((\(C.EpochNo e) -> e) $ epochNo block)
    <> CBOR.encodeWord64 (fromPosixToWord . blockTime $ block)

fromPosixToWord :: POSIXTime -> Word64
fromPosixToWord =
  fromIntegral . picoToIntegralPart . nominalDiffTimeToSeconds
  where
    picoToIntegralPart pico@(MkFixed num) =
      num `div` resolution pico

decodeBlock
  :: CodecConfig
  -> BlockNodeToClientVersion
  -> CBOR.Decoder s BlockEvent
decodeBlock codecConfig blockToNode = do
  block <- C.fromConsensusBlock C.CardanoMode <$> O.decodeNodeToClient codecConfig blockToNode
  epochNo' <- fromIntegral <$> CBOR.decodeWord64
  time' <- fromWordToPosix <$> CBOR.decodeWord64
  pure $ BlockEvent block epochNo' time'

fromWordToPosix :: Word64 -> POSIXTime
fromWordToPosix =
  secondsToNominalDiffTime . integralToPico . fromIntegral
  where
    integralToPico i =
      -- the 'undefined' won't be evaluated, 'resolution' is a function from types to data
      MkFixed $ i * resolution (undefined :: Pico)

serializeChainPoint :: C.ChainPoint -> Builder
serializeChainPoint =
  let pointEncoding :: C.ChainPoint -> CBOR.Encoding
      pointEncoding C.ChainPointAtGenesis = CBOR.encodeBool False
      pointEncoding (C.ChainPoint (C.SlotNo s) (C.HeaderHash bhh)) =
        CBOR.encodeBool True <> CBOR.encodeWord64 s <> CBOR.encodeBytes (BS.Short.fromShort bhh)
   in CBOR.toBuilder . pointEncoding

deserialiseChainPoint :: BS.ByteString -> Either Text C.ChainPoint
deserialiseChainPoint bs =
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
