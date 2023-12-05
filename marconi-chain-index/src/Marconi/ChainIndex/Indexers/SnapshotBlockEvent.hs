{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Indexers.SnapshotBlockEvent (
  snapshotBlockEventWorker,
  SnapshotBlockEvent (..),
  SnapshotBlockEventWorkerConfig (..),
  SnapshotBlockEventMetadata,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent (BlockEvent, blockInMode, blockTime, epochNo))
import Cardano.Api.Shelley qualified as C
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Arrow ((<<<))
import Control.Monad.Cont (MonadTrans (lift))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Short qualified as BS.Short
import Data.Data (Proxy (Proxy))
import Data.Fixed (Fixed (MkFixed))
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock (secondsToNominalDiffTime)
import Marconi.Cardano.Core.Indexer.Worker (
  StandardWorkerConfig (eventExtractor, logger, workerName),
 )
import Marconi.Cardano.Core.Types (BlockEvent (BlockEvent), BlockRange, isInBlockRange)
import Marconi.Core qualified as Core
import Marconi.Core.Indexer.FileIndexer (mkFileIndexer)
import Marconi.Core.Preprocessor qualified as Core
import Ouroboros.Consensus.Block qualified as O
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Config qualified as O
import Ouroboros.Consensus.Ledger.Extended qualified as O
import Ouroboros.Consensus.Node.NetworkProtocolVersion qualified as O
import Ouroboros.Consensus.Node.Serialisation qualified as O
import Ouroboros.Consensus.Storage.Serialisation qualified as O
import Text.Read qualified as Text

data SnapshotBlockEventWorkerConfig input = SnapshotBlockEventWorkerConfig
  { currentBlockNo :: input -> C.BlockNo
  , blockRange :: BlockRange
  }

-- TODO: I think this is a structured representation of the filename?
data SnapshotBlockEventMetadata = SnapshotBlockEventMetadata
  { blockMetadataBlockNo :: Maybe C.BlockNo
  , blockMetadataChainpoint :: C.ChainPoint
  }
  deriving (Show)

newtype SnapshotBlockEvent = SnapshotBlockEvent
  { getBlockEvent :: BlockEvent
  }

type instance Core.Point SnapshotBlockEvent = C.ChainPoint

mkSnapshotBlockEventIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => FilePath
  -> m (Core.FileIndexer SnapshotBlockEventMetadata SnapshotBlockEvent)
mkSnapshotBlockEventIndexer path =
  let fileStorageConfig =
        Core.FileStorageConfig
          False
          (const $ const [])
          (comparing blockMetadataBlockNo)
      fileBuilder =
        Core.FileBuilder
          "block"
          "cbor"
          metadataAsText
          (serializeSnapshotBlockEvent codecConfig blockToNode)
          serializeChainPoint
      eventBuilder =
        Core.EventBuilder
          deserializeMetadata
          blockMetadataChainpoint
          (deserializeSnapshotBlockEvent codecConfig blockToNode)
          deserializeChainPoint
   in mkFileIndexer path Nothing fileStorageConfig fileBuilder eventBuilder

deserializeSnapshotBlockEvent
  :: O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> O.BlockNodeToClientVersion (O.CardanoBlock O.StandardCrypto)
  -> SnapshotBlockEventMetadata
  -> BS.ByteString
  -> Either Text (Maybe SnapshotBlockEvent)
deserializeSnapshotBlockEvent _codecConfig _blockToNode (SnapshotBlockEventMetadata Nothing _) =
  const (Right Nothing)
deserializeSnapshotBlockEvent codecConfig blockToNode metadata =
  bimap
    (Text.pack . show)
    (Just . SnapshotBlockEvent . snd)
    . CBOR.deserialiseFromBytes (decodeBlock codecConfig blockToNode metadata)
    . BS.fromStrict

serializeSnapshotBlockEvent
  :: O.CodecConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> O.BlockNodeToClientVersion (O.CardanoBlock O.StandardCrypto)
  -> SnapshotBlockEvent
  -> BS.ByteString
serializeSnapshotBlockEvent codecConfig blockToNode =
  CBOR.toStrictByteString
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

deserializeMetadata :: [Text.Text] -> Maybe SnapshotBlockEventMetadata
deserializeMetadata [blockNoStr] =
  Just $ SnapshotBlockEventMetadata (parseBlockNo blockNoStr) C.ChainPointAtGenesis
deserializeMetadata [blockNoStr, slotNoStr, hashStr] = do
  slotNo <- fmap C.SlotNo . Text.readMaybe $ Text.unpack slotNoStr
  bhhBs <- either (const Nothing) Just $ Base16.decode $ Text.encodeUtf8 hashStr
  headerHash <- either (const Nothing) Just $ C.deserialiseFromRawBytes (C.proxyToAsType Proxy) bhhBs
  Just $ SnapshotBlockEventMetadata (parseBlockNo blockNoStr) (C.ChainPoint slotNo headerHash)
deserializeMetadata _other = Nothing

snapshotBlockEventWorker
  :: forall input m n
   . (MonadIO m, MonadError Core.IndexerError m, MonadIO n)
  => StandardWorkerConfig n input SnapshotBlockEvent
  -> SnapshotBlockEventWorkerConfig input
  -> FilePath
  -> m
      ( Core.WorkerIndexer
          n
          input
          SnapshotBlockEvent
          (Core.WithTrace n (Core.FileIndexer SnapshotBlockEventMetadata))
      )
snapshotBlockEventWorker standardWorkerConfig snapshotBlockEventWorkerConfig path = do
  indexer <- Core.withTrace (logger standardWorkerConfig) <$> mkSnapshotBlockEventIndexer path
  let preprocessor =
        Core.traverseMaybeEvent (lift . eventExtractor standardWorkerConfig)
          <<< inBlockRangePreprocessor
            (currentBlockNo snapshotBlockEventWorkerConfig)
            (blockRange snapshotBlockEventWorkerConfig)
  Core.createWorkerWithPreprocessing (workerName standardWorkerConfig) preprocessor indexer

inBlockRangePreprocessor
  :: (Monad m) => (a -> C.BlockNo) -> BlockRange -> Core.Preprocessor m C.ChainPoint a a
inBlockRangePreprocessor getBlockNo br =
  Core.scanMaybeEvent filterWithinBlockRange Nothing
  where
    filterWithinBlockRange input =
      if isInBlockRange (getBlockNo input) br
        then pure . Just $ input
        else pure Nothing

-- From Nicolas' branch

-- | Metadata used to cerate 'EpochStateIndexer' filenames

-- The codec config I will get from reading the result from indexing the ExtLedgerStateEvent from
-- the first block in the range OR is it from genesis?
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
  -> SnapshotBlockEventMetadata
  -> CBOR.Decoder s BlockEvent
decodeBlock codecConfig blockToNode _metadata = do
  block <- C.fromConsensusBlock C.CardanoMode <$> O.decodeNodeToClient codecConfig blockToNode
  epochNo' <- fromIntegral <$> CBOR.decodeWord64
  time' <- secondsToNominalDiffTime . MkFixed . fromIntegral <$> CBOR.decodeWord64
  pure $ BlockEvent block epochNo' time'

serializeChainPoint :: C.ChainPoint -> BS.ByteString
serializeChainPoint =
  let pointEncoding :: C.ChainPoint -> CBOR.Encoding
      pointEncoding C.ChainPointAtGenesis = CBOR.encodeBool False
      pointEncoding (C.ChainPoint (C.SlotNo s) (C.HeaderHash bhh)) =
        CBOR.encodeBool True <> CBOR.encodeWord64 s <> CBOR.encodeBytes (BS.Short.fromShort bhh)
   in CBOR.toStrictByteString . pointEncoding

deserializeChainPoint :: BS.ByteString -> Either Text C.ChainPoint
deserializeChainPoint bs =
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
