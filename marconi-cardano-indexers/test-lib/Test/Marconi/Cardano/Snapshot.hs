{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Marconi.Cardano.Snapshot (
  SnapshotFileData (..),
  setupSnapshot,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent)
import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT)
import Data.ByteString qualified as BS
import Data.List (find, isPrefixOf, sortOn)
import Data.Text (Text)
import Data.Text qualified as Text
import Marconi.Cardano.Indexers.SnapshotBlockEvent (
  BlockNodeToClientVersion,
  CodecConfig,
  SnapshotBlockEvent (getBlockEvent),
  SnapshotMetadata (snapshotMetadataBlockNo),
  blockNodeToNodeVersionM,
  deserialiseSnapshotBlockEvent,
  getConfigCodec,
 )
import Marconi.Cardano.Indexers.SnapshotBlockEvent qualified as BlockEvent
import Streaming (Of, Stream)
import Streaming.Prelude qualified as Stream
import System.Directory (copyFile, createDirectoryIfMissing, listDirectory)
import System.FilePath (takeBaseName, (</>))

{- | A snapshot of a sub-chain is a directory containing a series of serialised
block events and a ledger state of the chain immediately preceding the series of blocks.

'setupSnapshot' is the function which prepares the above data for indexing.
It creates and returns a stream of block events to be deserialised. The file containing
the ledger state is copied into Marconi's database.

Ultimately, it provides the necessary data to be able to simulate a partial
running node.
-}
setupSnapshot
  :: FilePath
  -- ^ path to the node config file
  -> FilePath
  -- ^ directory which contains the serialized events
  -> FilePath
  -- ^ directory to be used as the indexer's DB
  -> IO (Stream (Of BlockEvent) IO ())
setupSnapshot nodeConfig inputDir dbDir = do
  codecConfig <- getConfigCodec' nodeConfig
  toClientVersion <- getBlockToNodeClientVersion
  files <- fmap (inputDir </>) <$> listDirectory inputDir
  let blockFiles = filter isBlockEventFile files
      ledgerStateFile = findLedgerState files
      blockStream = mkBlockEventStream codecConfig toClientVersion blockFiles
  createDirectoryIfMissing True (dbDir </> "epochState")
  copyFile ledgerStateFile (dbDir </> "epochState" </> "epochState")
  return blockStream
  where
    getConfigCodec' = either (error . show) pure <=< runExceptT . getConfigCodec

    getBlockToNodeClientVersion =
      maybe (error "Error in getting the node client version") pure blockNodeToNodeVersionM

    isBlockEventFile = isPrefixOf "block_" . takeBaseName

    findLedgerState =
      maybe (error "Could not find file containing serialized ledger state") id
        . find (isPrefixOf "epochState_" . takeBaseName)

-- | Necessary information about each file containing a serialised 'BlockEvent'.
data SnapshotFileData = SnapshotFileData
  { path :: FilePath
  , metadata :: SnapshotMetadata
  , index :: C.BlockNo
  }

{- | Parses the the file name into a 'SnapshotFileData'. It expects it to abide to
the format outputted by the 'FileIndexer' used by marconi-chain-snapshot.
-}
mkSnapshotFileData :: FilePath -> SnapshotFileData
mkSnapshotFileData orig@(Text.pack . takeBaseName -> name) =
  case BlockEvent.deserialiseMetadata . extractRawMetadata "block" $ name of
    Nothing -> error "Malformed metadata for serialized BlockEvent."
    Just m ->
      case snapshotMetadataBlockNo m of
        Nothing -> error "Malformed metadata: missing block number"
        Just i -> SnapshotFileData orig m i

{- | Creates the actual stream of blocks. Each block will eventually be deserialised
from the data present in its respective file on disk.
-}
mkBlockEventStream
  :: CodecConfig
  -> BlockNodeToClientVersion
  -> [FilePath]
  -> Stream (Of BlockEvent) IO ()
mkBlockEventStream codecConfig toClientVersion =
  Stream.mapM deserialiseBlock . mkFileStream
  where
    deserialiseBlock :: SnapshotFileData -> IO BlockEvent
    deserialiseBlock (SnapshotFileData file meta _blockNo) = do
      rawBytes <- BS.readFile file
      let eBlockEvent = deserialiseSnapshotBlockEvent codecConfig toClientVersion meta rawBytes
          blockEvent =
            case eBlockEvent of
              Left err -> error (Text.unpack err)
              Right mBlockEvent ->
                case mBlockEvent of
                  Nothing -> error "Cannot deserialise block event"
                  Just be -> be
      return (getBlockEvent blockEvent)
    mkFileStream :: [FilePath] -> Stream (Of SnapshotFileData) IO ()
    mkFileStream = Stream.each . sortOn index . fmap mkSnapshotFileData

{- | Splits the file name into the relevant parts which constitute
the metadata.
-}
extractRawMetadata :: Text -> Text -> [Text]
extractRawMetadata typeOfEvent name =
  case Text.splitOn "_" name of
    (prefix : contentFlag : rawMetadata)
      | prefix == typeOfEvent
      , contentFlag == "just" ->
          rawMetadata
    _ -> error $ "Malformed metadata format for: " <> show name
