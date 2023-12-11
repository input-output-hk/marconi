{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Marconi.Cardano.Snapshot where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent)
import Data.ByteString qualified as BS
import Data.List (sortOn)
import Data.Text qualified as Text
import Marconi.Cardano.Indexers.SnapshotBlockEvent (
  BlockNodeToClientVersion,
  CodecConfig,
  SnapshotBlockEvent (getBlockEvent),
  SnapshotMetadata (snapshotMetadataBlockNo),
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
mkFileStream = Stream.each . sortOn index . fmap f
  where
    f :: FilePath -> SnapshotFileData
    f orig@(Text.pack -> name) =
      case deserializeMetadata $ Text.splitOn "_" name of
        Nothing -> error "TODO"
        Just m ->
          case snapshotMetadataBlockNo m of
            Nothing -> error "TODO"
            Just i -> SnapshotFileData orig m i

mkBlockEventStream
  :: CodecConfig
  -> BlockNodeToClientVersion
  -> Stream (Of SnapshotFileData) IO ()
  -> Stream (Of BlockEvent) IO ()
mkBlockEventStream codecConfig toClientVersion = Stream.mapM serializeBlock
  where
    serializeBlock (SnapshotFileData file meta _) = do
      rawBytes <- BS.readFile file
      let eBlockEvent = deserializeSnapshotBlockEvent codecConfig toClientVersion meta rawBytes
          blockEvent =
            case eBlockEvent of
              Left err -> error (Text.unpack err)
              Right mBlockEvent ->
                case mBlockEvent of
                  Nothing -> error "TODO"
                  Just be -> be
      return (getBlockEvent blockEvent)

mkStreamFromSnapshot
  :: FilePath
  -- ^ path to the node config file
  -> FilePath
  -- ^ directory which contains serialized block events
  -> IO (Stream (Of BlockEvent) IO ())
mkStreamFromSnapshot nodeConfig dir = do
  codecConfig <- getConfigCodec nodeConfig
  toClientVersion <- getBlockToNodeClientVersion
  files <- listDirectory dir
  return $ mkBlockEventStream codecConfig toClientVersion $ mkFileStream files
