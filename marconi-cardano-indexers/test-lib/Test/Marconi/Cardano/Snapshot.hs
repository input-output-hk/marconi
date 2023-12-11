{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Marconi.Cardano.Snapshot where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent)
import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT)
import Data.ByteString qualified as BS
import Data.List (find, isPrefixOf, sortOn)
import Data.Text qualified as Text
import Marconi.Cardano.Indexers.ExtLedgerStateCoordinator (ExtLedgerStateEvent)
import Marconi.Cardano.Indexers.SnapshotBlockEvent (
  BlockNodeToClientVersion,
  CodecConfig,
  SnapshotBlockEvent (getBlockEvent),
  SnapshotMetadata (snapshotMetadataBlockNo),
  blockNodeToNodeVersionM,
  deserializeSnapshotBlockEvent,
  getConfigCodec,
 )
import Marconi.Cardano.Indexers.SnapshotBlockEvent qualified as BlockEvent
import Streaming (Of, Stream)
import Streaming.Prelude qualified as Stream
import System.Directory (listDirectory)

data Snapshot = Snapshot
  { snapshotPreviousLedgerState :: ExtLedgerStateEvent
  , snapshotBlockStream :: Stream (Of BlockEvent) IO ()
  }

data SnapshotFileData = SnapshotFileData
  { path :: FilePath
  , metadata :: SnapshotMetadata
  , index :: C.BlockNo
  }

mkSnapshotFileData :: FilePath -> SnapshotFileData
mkSnapshotFileData orig@(Text.pack -> name) =
  case BlockEvent.deserializeMetadata $ Text.splitOn "_" name of
    Nothing -> error "Malformed metadata"
    Just m ->
      case snapshotMetadataBlockNo m of
        Nothing -> error "Malformed metadata: missing block number"
        Just i -> SnapshotFileData orig m i

mkBlockEventStream
  :: CodecConfig
  -> BlockNodeToClientVersion
  -> [FilePath]
  -> Stream (Of BlockEvent) IO ()
mkBlockEventStream codecConfig toClientVersion =
  Stream.mapM serializeBlock . mkFileStream
  where
    serializeBlock :: SnapshotFileData -> IO BlockEvent
    serializeBlock (SnapshotFileData file meta _blockNo) = do
      rawBytes <- BS.readFile file
      let eBlockEvent = deserializeSnapshotBlockEvent codecConfig toClientVersion meta rawBytes
          blockEvent =
            case eBlockEvent of
              Left err -> error (Text.unpack err)
              Right mBlockEvent ->
                case mBlockEvent of
                  Nothing -> error "Cannot deserialize block event"
                  Just be -> be
      return (getBlockEvent blockEvent)
    mkFileStream :: [FilePath] -> Stream (Of SnapshotFileData) IO ()
    mkFileStream = Stream.each . sortOn index . fmap mkSnapshotFileData

-- TODO: I think there is an issue with the metadata parsing, I should
-- look at how it's implemented in the FileIndexer
mkSnapshot
  :: FilePath
  -- ^ path to the node config file
  -> FilePath
  -- ^ directory which contains the serialized events
  -> IO Snapshot
mkSnapshot nodeConfig inputDir = do
  codecConfig <- getConfigCodec' nodeConfig
  toClientVersion <- getBlockToNodeClientVersion
  files <- listDirectory inputDir
  let blockFiles = filter isBlockEventFile files
      ledgerStateFile = findLedgerState files
      blockStream = mkBlockEventStream codecConfig toClientVersion blockFiles
      previousLedgerState = deserializeLedgerState codecConfig toClientVersion ledgerStateFile
  return (Snapshot previousLedgerState blockStream)
  where
    getConfigCodec' = either (error . show) pure <=< runExceptT . getConfigCodec

    getBlockToNodeClientVersion =
      maybe (error "Error in getting the node client version") pure blockNodeToNodeVersionM

    isBlockEventFile = isPrefixOf "block_"

    findLedgerState =
      maybe (error "Could not find file containing serialized ledger state") id
        . find ("epochState_" `isPrefixOf`)
