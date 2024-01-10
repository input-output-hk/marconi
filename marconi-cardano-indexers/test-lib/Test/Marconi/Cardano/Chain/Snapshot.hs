{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Marconi.Cardano.Chain.Snapshot (
  SnapshotFileData (..),
  setupSnapshot,
  deserialiseSnapshot,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent)
import Control.Monad (when, (<=<))
import Control.Monad.Except (runExceptT)
import Data.ByteString qualified as BS
import Data.List (find, isPrefixOf, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Marconi.Cardano.Indexers.ExtLedgerStateCoordinator (ExtLedgerStateEvent)
import Marconi.Cardano.Indexers.ExtLedgerStateCoordinator qualified as ExtLedgerState
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
  -- ^ directory which contains the serialised events
  -> FilePath
  -- ^ directory to be used as the indexer's DB
  -> IO (Stream (Of BlockEvent) IO ())
setupSnapshot nodeConfigPath inputDir dbDir = do
  (ledgerFile, blockFiles) <- findSnapshotFiles inputDir
  setupLedgerState ledgerFile dbDir
  getBlockEvents nodeConfigPath blockFiles

-- | Deserialise the files and return the Haskell data for testing.
deserialiseSnapshot
  :: FilePath
  -- ^ path to the node config file
  -> FilePath
  -- ^ directory which contains the serialised events
  -> IO (ExtLedgerStateEvent, Stream (Of BlockEvent) IO ())
deserialiseSnapshot nodeConfigPath inputDir = do
  (ledgerFile, blockFiles) <- findSnapshotFiles inputDir
  blockEvents <- getBlockEvents nodeConfigPath blockFiles
  ledgerState <- deserialiseLedgerState nodeConfigPath ledgerFile
  pure (ledgerState, blockEvents)

{- | Utility function which searches for the serialised ledger state
and the serialised block events. The files are expected to be named
with a specific prefix.
-}
findSnapshotFiles
  :: FilePath
  -- ^ directory which contains the serialized events
  -> IO (FilePath, [FilePath])
findSnapshotFiles inputDir = do
  files <- fmap (inputDir </>) <$> listDirectory inputDir
  let blockFiles = filter isBlockEventFile files
      ledgerStateFile = findLedgerState files
  when
    (null blockFiles)
    (error "Could not find any files containing serialized block events.")
  pure (ledgerStateFile, blockFiles)
  where
    isBlockEventFile = isPrefixOf "block_" . takeBaseName

    findLedgerState =
      fromMaybe (error "Could not find file containing serialized ledger state.")
        . find (isPrefixOf "epochState_" . takeBaseName)

{- | Reads the node configuration file which is used to build the stream of
block events from the snapshot on disk.
-}
getBlockEvents
  :: FilePath
  -- ^ path to the node config file
  -> [FilePath]
  -- ^ paths to the serialised block events
  -> IO (Stream (Of BlockEvent) IO ())
getBlockEvents nodeConfigPath blockEventPaths = do
  codecConfig <- getConfigCodec' nodeConfigPath
  toClientVersion <- getBlockToNodeClientVersion
  pure $ mkBlockEventStream codecConfig toClientVersion blockEventPaths
  where
    getBlockToNodeClientVersion =
      maybe (error "Error in getting the node client version") pure blockNodeToNodeVersionM

-- | Initialises the indexer database with the serialised ledger state.
setupLedgerState
  :: FilePath
  -- ^ path to the serialized ledger state
  -> FilePath
  -- ^ directory to be used as the indexer's DB
  -> IO ()
setupLedgerState ledgerStatePath dbDir = do
  createDirectoryIfMissing True (dbDir </> "epochState")
  copyFile ledgerStatePath (dbDir </> "epochState" </> "epochState")

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
      pure (getBlockEvent blockEvent)
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

-- | Internalises the ledger state from a serialisation on disk.
deserialiseLedgerState
  :: FilePath
  -- ^ path to the node config file
  -> FilePath
  -- ^ path to the serialized ledger state
  -> IO ExtLedgerStateEvent
deserialiseLedgerState configFile file@(Text.pack . takeBaseName -> name) = do
  codecConfig <- getConfigCodec' configFile
  case ExtLedgerState.deserialiseMetadata . extractRawMetadata "epochState" $ name of
    Nothing ->
      error $ "Malformed metadata for serialized ExtLedgerStateEvent: " <> show name
    Just meta -> do
      rawBytes <- BS.readFile file
      let eExtLedgerState = ExtLedgerState.deserialiseLedgerState codecConfig meta rawBytes
      case eExtLedgerState of
        Left err -> error (Text.unpack err)
        Right mExtLedgerState ->
          case mExtLedgerState of
            Nothing -> error "Cannot deserialise ledger state."
            Just ledger -> pure ledger

getConfigCodec' :: FilePath -> IO CodecConfig
getConfigCodec' = either (error . show) pure <=< runExceptT . getConfigCodec
