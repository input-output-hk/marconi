{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Marconi.Cardano.Snapshot where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent)
import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT)
import Data.ByteString qualified as BS
import Data.List (isPrefixOf, sortOn)
import Data.Text qualified as Text
import Marconi.Cardano.Indexers.SnapshotBlockEvent (
  BlockNodeToClientVersion,
  CodecConfig,
  SnapshotBlockEvent (getBlockEvent),
  SnapshotMetadata (snapshotMetadataBlockNo),
  blockNodeToNodeVersionM,
  deserializeMetadata,
  deserializeSnapshotBlockEvent,
  getConfigCodec,
 )
import Streaming (Of, Stream)
import Streaming.Prelude qualified as Stream
import System.Directory (listDirectory)

data SnapshotFileData = SnapshotFileData
  { path :: FilePath
  , metadata :: SnapshotMetadata
  , index :: C.BlockNo
  }

mkFileStream :: [FilePath] -> Stream (Of SnapshotFileData) IO ()
mkFileStream = Stream.each . sortOn index . fmap mkSnapshotFileData

mkSnapshotFileData :: FilePath -> SnapshotFileData
mkSnapshotFileData orig@(Text.pack -> name) =
  case deserializeMetadata $ Text.splitOn "_" name of
    Nothing -> error "Malformed metadata"
    Just m ->
      case snapshotMetadataBlockNo m of
        Nothing -> error "Malformed metadata: missing block number"
        Just i -> SnapshotFileData orig m i

mkBlockEventStream
  :: CodecConfig
  -> BlockNodeToClientVersion
  -> Stream (Of SnapshotFileData) IO ()
  -> Stream (Of BlockEvent) IO ()
mkBlockEventStream codecConfig toClientVersion = Stream.mapM serializeBlock
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

mkStreamFromSnapshot
  :: FilePath
  -- ^ path to the node config file
  -> FilePath
  -- ^ directory which contains serialized block events
  -> IO (Stream (Of BlockEvent) IO ())
mkStreamFromSnapshot nodeConfig dir = do
  codecConfig <- getConfigCodec' nodeConfig
  toClientVersion <- getBlockToNodeClientVersion
  files <- filter isBlockEventFile <$> listDirectory dir
  return
    . mkBlockEventStream codecConfig toClientVersion
    $ mkFileStream files
  where
    getConfigCodec' = either (error . show) pure <=< runExceptT . getConfigCodec
    getBlockToNodeClientVersion =
      maybe (error "Error in getting the node client version") pure blockNodeToNodeVersionM
    isBlockEventFile = isPrefixOf "block_"
