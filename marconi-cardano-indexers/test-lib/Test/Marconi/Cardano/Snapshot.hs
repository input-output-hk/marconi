{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Marconi.Cardano.Snapshot (
  Snapshot (..),
  SnapshotFileData (..),
  internaliseSnapshot,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent)
import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT)
import Data.ByteString qualified as BS
import Data.List (find, isPrefixOf, sortOn)
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
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))

{- | A 'Snapshot' is an internal representation for a sub-chain which has been
serialised to disk.
It stores a ledger state and a stream of blocks. The ledger state is the state
before the eventual application of the range of blocks in the stream. This
allows one to simulate the behavior of the blockchain for a specific range of
blocks which was serialized to disk using the marconi-chain-snapshot executable.
-}
data Snapshot = Snapshot
  { snapshotPreviousLedgerState :: ExtLedgerStateEvent
  , snapshotBlockStream :: Stream (Of BlockEvent) IO ()
  }

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

{- | The function which builds the internal 'Snapshot' to be used in tests.
It requires a path to the configuration file for the Cardano blockchain
(preview/preprod/mainnet) which was used to create the snapshot on disk,
and a path to the directory which contains the actual files.
-}
internaliseSnapshot
  :: FilePath
  -- ^ path to the node config file
  -> FilePath
  -- ^ directory which contains the serialized events
  -> IO Snapshot
internaliseSnapshot nodeConfig inputDir = do
  codecConfig <- getConfigCodec' nodeConfig
  toClientVersion <- getBlockToNodeClientVersion
  files <- fmap (inputDir </>) <$> listDirectory inputDir
  let blockFiles = filter isBlockEventFile files
      ledgerStateFile = findLedgerState files
      blockStream = mkBlockEventStream codecConfig toClientVersion blockFiles
  previousLedgerState <- deserialiseLedgerState codecConfig ledgerStateFile
  return (Snapshot previousLedgerState blockStream)
  where
    getConfigCodec' = either (error . show) pure <=< runExceptT . getConfigCodec

    getBlockToNodeClientVersion =
      maybe (error "Error in getting the node client version") pure blockNodeToNodeVersionM

    isBlockEventFile = isPrefixOf "block_" . takeBaseName

    findLedgerState =
      maybe (error "Could not find file containing serialized ledger state") id
        . find (isPrefixOf "epochState_" . takeBaseName)

-- | Internalises the ledger state from a serialisation on disk.
deserialiseLedgerState
  :: CodecConfig
  -> FilePath
  -> IO ExtLedgerStateEvent
deserialiseLedgerState codecConfig file@(Text.pack . takeBaseName -> name) =
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
            Just ledger -> return ledger

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
