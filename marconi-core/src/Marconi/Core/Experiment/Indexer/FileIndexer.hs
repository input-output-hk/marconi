{-# LANGUAGE TemplateHaskell #-}

{- | An indexer that stores its events to files, using the provided serialisation.

Note that the indexer stores one file per event, it recommended to either limit the number of
events we keep or to use it for sparse events.
-}
module Marconi.Core.Experiment.Indexer.FileIndexer (
  FileIndexer (FileIndexer),
  FileStorageConfig (FileStorageConfig),
  FileBuilder (FileBuilder),
  EventBuilder (EventBuilder),
  mkFileIndexer,
  compareMeta,
  eventDirectory,
  eventPrefix,
  eventSuffix,
  eventBuilder,
  deserialiseEvent,
  deserialisePoint,
  extractPoint,
  serialiseEvent,
  serialisePoint,
  deserialiseTimedEvent,
  fileEventIdentifier,
  fileIndexerLastSyncPoint,
  getDirectoryMetadata,
  EventInfo (..),
) where

import Control.Exception (Exception (displayException), handle)
import Control.Lens qualified as Lens
import Control.Lens.Operators ((.~), (^.))
import Control.Monad (forM_, join, void, when)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (catchError, throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (Foldable (toList), traverse_)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace qualified
import Marconi.Core.Experiment.Class (
  Closeable (close),
  HasGenesis (genesis),
  IsIndex (index, indexAllDescending, rollback, setLastStablePoint),
  IsSync (lastStablePoint, lastSyncPoint),
 )
import Marconi.Core.Experiment.Type (
  IndexerError (IndexerInternalError),
  Point,
  QueryError (IndexerQueryError),
  Timed (Timed),
  event,
  point,
 )
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removeFile)
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

{- | Configuration datatype for 'FileIndexer' that provides the content needed to create the
filenames used by the indexer
-}
data FileBuilder meta event = FileBuilder
  { _eventPrefix :: Text
  -- ^ The prefix used in the filename of the events
  , _eventSuffix :: Text
  -- ^ The suffix used in the filename of the events
  , _fileEventIdentifier :: Timed (Point event) (Maybe event) -> [Text]
  -- ^ build a list of identifier used in the filename of an event
  , _serialiseEvent :: event -> ByteString
  , _serialisePoint :: Point event -> ByteString
  }

Lens.makeLenses ''FileBuilder

{- | This datatypes gather the functions needed to deserialise events from both the filenames and
the file content.

@meta@ is the metadata type and @event@ the type of events we handle.
-}
data EventBuilder meta event = EventBuilder
  { _deserialiseMeta :: [Text] -> Maybe meta
  -- ^ this function take the parts of the filename (split with @_@) and rebuild the metadata from
  -- there
  , _extractPoint :: meta -> Point event
  , _deserialiseEvent :: meta -> ByteString -> Either Text (Maybe event)
  , _deserialisePoint :: ByteString -> Either Text (Point event)
  }

Lens.makeLenses ''EventBuilder

-- | Information about an event that can be gathered from the filename
data EventInfo meta = EventInfo
  { hasContent :: Bool
  , fileMetadata :: meta
  , path :: FilePath
  }

{- | The dataytpe used to control which events are saved and how many we keep on disk.

Be careful in the choice of the function used to remove events
as you probably don't want to store all the events on disk.
-}
data FileStorageConfig meta event = FileStorageConfig
  { _keepEmptyEvent :: Bool
  -- ^ Do we create a file for empty event
  , _eventsToRemove :: Timed (Point event) (Maybe event) -> [EventInfo meta] -> [EventInfo meta]
  -- ^ Filtering unction used to decide which events must be removed
  , _fileComparison :: meta -> meta -> Ordering
  -- ^ Function used to sort event files, based on their metadata
  }

Lens.makeLenses ''FileStorageConfig

{- | An indexer that store events in a directory, one file per event.

It as to type parameters:

    * @meta@ the metadata type, used to build the filename
    * @event@ the indexed events type.
-}
data FileIndexer meta event = FileIndexer
  { _eventDirectory :: FilePath
  -- ^ The directory where we store the events
  , _storageConfig :: FileStorageConfig meta event
  -- ^ Define storage management
  , _fileBuilder :: FileBuilder meta event
  -- ^ Describe how we build filename
  , _eventBuilder :: EventBuilder meta event
  -- ^ Gather deserialisation functions
  , _fileIndexerLastSyncPoint :: Point event
  -- ^ keep track of the last sync point
  , _fileIndexerLastStablePoint :: Point event
  -- ^ keep track of the last stable point
  }

Lens.makeLenses ''FileIndexer

compareMeta
  :: FileIndexer meta event
  -> meta
  -> meta
  -> Ordering
compareMeta ix = ix ^. storageConfig . fileComparison

-- Create a 'FileIndexer', rebuilding the contenc
mkFileIndexer
  :: ( MonadIO m
     , MonadError IndexerError m
     , Ord (Point event)
     , HasGenesis (Point event)
     )
  => FilePath
  -> FileStorageConfig meta event
  -> FileBuilder meta event
  -> EventBuilder meta event
  -> m (FileIndexer meta event)
mkFileIndexer path storageCfg filenameBuilder' eventBuilder' = do
  liftIO $ createDirectoryIfMissing True path
  let indexer =
        FileIndexer
          path
          storageCfg
          filenameBuilder'
          eventBuilder'
          genesis
          genesis
  Debug.Trace.traceM "read stable point"
  lastStablePoint' <- fromMaybe genesis <$> readCurrentStable indexer
  let indexer' =
        indexer
          & fileIndexerLastSyncPoint .~ lastStablePoint'
          & fileIndexerLastStablePoint .~ lastStablePoint'
  Debug.Trace.traceM "Rolling back"
  indexer'' <- rollback lastStablePoint' indexer'
  Debug.Trace.traceM "Rolled back"
  pure indexer''

toFilename :: FilePath -> FileBuilder meta event -> Timed (Point event) (Maybe event) -> FilePath
toFilename dir indexer evt =
  let eventId = (indexer ^. fileEventIdentifier) evt
      contentFlag = case evt ^. event of
        Just _ -> "just"
        Nothing -> "nothing"
      filename =
        Text.unpack $
          (Text.intercalate "_" $ indexer ^. eventPrefix : contentFlag : eventId)
            <> "."
            <> (indexer ^. eventSuffix)
   in dir </> filename

handleIOErrors :: (MonadIO m, MonadError IndexerError m) => IO a -> m a
handleIOErrors action = do
  let throwIOError :: IOError -> IO (Either IndexerError b)
      throwIOError e = pure $ Left $ IndexerInternalError . Text.pack $ displayException e
  result <- liftIO $ handle throwIOError (Right <$> action)
  either throwError pure result

fullPath :: FileIndexer meta event -> EventInfo meta -> FilePath
fullPath indexer info = indexer ^. eventDirectory </> path info

extractEventsInfo
  :: (MonadIO m, MonadError (QueryError q) m)
  => Text
  -> ([Text] -> Maybe meta)
  -> FilePath
  -> m [EventInfo meta]
extractEventsInfo expectedPrefix metaExtractor eventDir =
  let extractEventInfo path = do
        let filename = FilePath.dropExtension $ FilePath.takeFileName path
        case Text.splitOn "_" $ Text.pack filename of
          (prefix : contentFlag : parts) | prefix == expectedPrefix -> do
            let hasContent' = contentFlag == "just"
            meta <- case metaExtractor parts of
              Nothing ->
                throwError $
                  IndexerQueryError $
                    "Invalid file in a file indexer: " <> Text.pack filename
              Just x -> pure x
            pure [EventInfo hasContent' meta path]
          _ -> pure []
   in do
        files <- liftIO $ listDirectory eventDir
        fmap join $ traverse extractEventInfo files

getDirectoryMetadata
  :: (MonadIO m, MonadError (QueryError q) m)
  => FileIndexer meta event
  -> m [EventInfo meta]
getDirectoryMetadata indexer = do
  let eventDir = indexer ^. eventDirectory
  extractEventsInfo
    (indexer ^. fileBuilder . eventPrefix)
    (indexer ^. eventBuilder . deserialiseMeta)
    eventDir

deserialiseTimedEvent
  :: (MonadIO m)
  => FileIndexer meta event
  -> EventInfo meta
  -> ExceptT Text m (Timed (Point event) (Maybe event))
deserialiseTimedEvent indexer eventFile = do
  let meta = fileMetadata eventFile
      deserialise = (indexer ^. eventBuilder . deserialiseEvent) meta
      pt = indexer ^. eventBuilder . extractPoint $ meta
  evt <-
    if hasContent eventFile
      then ExceptT $ deserialise <$> liftIO (BS.readFile $ fullPath indexer eventFile)
      else pure Nothing
  pure $ Timed pt evt

instance (Applicative m) => IsSync m event (FileIndexer meta) where
  lastSyncPoint = pure . Lens.view fileIndexerLastSyncPoint
  lastStablePoint = pure . Lens.view fileIndexerLastStablePoint

writeTimedEvent
  :: (MonadIO m, MonadError IndexerError m)
  => Timed (Point event) (Maybe event)
  -> FileIndexer meta event
  -> m ()
writeTimedEvent timedEvent indexer =
  let filename = toFilename (indexer ^. eventDirectory) (indexer ^. fileBuilder) timedEvent
   in case timedEvent ^. event of
        Nothing -> when (indexer ^. storageConfig . keepEmptyEvent) $ writeIndexerFile filename ""
        Just evt -> writeIndexerFile filename (indexer ^. fileBuilder . serialiseEvent $ evt)

writeIndexerFile
  :: (MonadIO m, MonadError IndexerError m)
  => FilePath
  -> ByteString
  -> m ()
writeIndexerFile filename content = handleIOErrors $ BS.writeFile filename content

cleanEvents
  :: (MonadIO m, MonadError IndexerError m)
  => Timed (Point event) (Maybe event)
  -> FileIndexer meta event
  -> m ()
cleanEvents timedEvent indexer = do
  dirMetadataE <- runExceptT $ getDirectoryMetadata indexer
  case dirMetadataE of
    Left _err -> throwError $ IndexerInternalError "Can't read directory content"
    Right dirMetadata -> do
      let removeFilter = indexer ^. storageConfig . eventsToRemove
          toRemove = removeFilter timedEvent dirMetadata
      liftIO $ traverse_ removeFile (fullPath indexer <$> toRemove)

instance (MonadIO m, MonadError IndexerError m) => IsIndex m event (FileIndexer meta) where
  index timedEvent indexer = do
    let currentPoint = timedEvent ^. point
        setLastSync ix = ix & fileIndexerLastSyncPoint .~ currentPoint
    writeTimedEvent timedEvent indexer
    cleanEvents timedEvent indexer
    pure $ setLastSync indexer

  indexAllDescending timedEvents indexer =
    case toList timedEvents of
      [] -> pure indexer
      x : _ -> do
        let currentPoint = x ^. point
            setLastSync ix = ix & fileIndexerLastSyncPoint .~ currentPoint
        traverse_ (flip writeTimedEvent indexer) timedEvents
        pure $ setLastSync indexer

  rollback p indexer = do
    filesWithMetadata <- runExceptT $ getDirectoryMetadata indexer
    case filesWithMetadata of
      Left _err -> throwError $ IndexerInternalError "can't parse directory content"
      Right xs -> forM_ xs $ \eventFile -> do
        let pt = indexer ^. eventBuilder . extractPoint $ fileMetadata eventFile
        when (pt > p) $ do
          let filename = fullPath indexer eventFile
          liftIO $ removeFile filename
    pure indexer

  setLastStablePoint p indexer = do
    currentStable <- readCurrentStable indexer `catchError` const (pure Nothing)
    case currentStable of
      Nothing -> writeStable p indexer
      Just p' -> do
        when (p > p') $ void $ writeStable p indexer
        pure indexer

lastStableFilename :: FileIndexer meta event -> FilePath
lastStableFilename indexer =
  let
    dir = indexer ^. eventDirectory
    filename = indexer ^. fileBuilder . eventPrefix <> "LatestStable.cbor"
   in
    dir </> Text.unpack filename

readCurrentStable
  :: (MonadIO m, MonadError IndexerError m)
  => FileIndexer meta event
  -> m (Maybe (Point event))
readCurrentStable indexer = do
  let f = lastStableFilename indexer
      deserialise = indexer ^. eventBuilder . deserialisePoint
  fileExists <- liftIO $ doesFileExist f
  if fileExists
    then do
      res <- deserialise <$> liftIO (BS.readFile f)
      case res of
        Left _ -> throwError $ IndexerInternalError "Can't read current stable"
        Right r -> pure $ Just r
    else pure Nothing

writeStable
  :: (MonadIO m)
  => Point event
  -> FileIndexer meta event
  -> m (FileIndexer meta event)
writeStable p indexer = do
  let f = lastStableFilename indexer
      serialise = indexer ^. fileBuilder . serialisePoint
  liftIO $ BS.writeFile f $ serialise p
  pure indexer

instance (Applicative m) => Closeable m (FileIndexer meta) where
  close = const $ pure ()
