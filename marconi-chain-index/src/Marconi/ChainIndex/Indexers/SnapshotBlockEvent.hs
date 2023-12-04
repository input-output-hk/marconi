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
import Data.Fixed (Fixed (MkFixed))
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

data SnapshotBlockEventWorkerConfig input = SnapshotBlockEventWorkerConfig
  { currentBlockNo :: input -> C.BlockNo
  , blockRange :: BlockRange
  }

-- TODO: I think this is a structured representation of the filename?
data SnapshotBlockEventMetadata = TODO2

newtype SnapshotBlockEvent = SnapshotBlockEvent BlockEvent

type instance Core.Point SnapshotBlockEvent = C.ChainPoint

mkSnapshotBlockEventIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => FilePath
  -> m (Core.FileIndexer SnapshotBlockEventMetadata SnapshotBlockEvent)
mkSnapshotBlockEventIndexer path =
  let fileStorageConfig =
        Core.FileStorageConfig
          False
          (\_ x -> x)
          undefined
      fileBuilder =
        Core.FileBuilder
          "Block"
          ""
          undefined
          serializeSnapshotBlockEvent
          serializeChainPoint
      eventBuilder =
        Core.EventBuilder
          undefined
          undefined
          deserializeSnapshotBlockEvent
          deserializeChainPoint
   in mkFileIndexer path Nothing fileStorageConfig fileBuilder eventBuilder

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
data BlockMetadata = BlockMetadata
  { blockMetadataBlockNo :: Maybe C.BlockNo
  , blockMetadataChainpoint :: C.ChainPoint
  }
  deriving (Show)

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
