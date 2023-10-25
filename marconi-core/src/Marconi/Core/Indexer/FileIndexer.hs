{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

{- | An indexer that stores its events to files, using the provided serialisation.

Note that the indexer stores one file per event, it recommended to either limit the number of
events we keep or to use it for sparse events.
-}
module Marconi.Core.Indexer.FileIndexer (
  FileIndexer (FileIndexer),
  FileStorageConfig (..),
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
  fullPath,
  getDirectoryMetadata,
  EventInfo (..),
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.QSem (QSem)
import Control.Concurrent.QSem qualified as Con
import Control.Exception (Exception (displayException), bracket_, handle)
import Control.Lens qualified as Lens
import Control.Lens.Operators ((.~), (^.))
import Control.Monad (forM_, join, unless, void, when)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (Foldable (toList), traverse_)
import Data.Function ((&))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Marconi.Core.Class (
  Closeable (close),
  HasGenesis (genesis),
  IsIndex (index, indexAllDescending, rollback, setLastStablePoint),
  IsSync (lastStablePoint, lastSyncPoint),
 )
import Marconi.Core.Type (
  IndexerError (IndexerCloseTimeoutError, IndexerInternalError),
  Point,
  QueryError (IndexerQueryError),
  Timed (Timed),
  event,
  point,
 )
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removeFile)
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import System.Timeout (timeout)

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

deriving stock instance (Show meta) => Show (EventInfo meta)

{- | The dataytpe used to configure the way we store files, including:

      - control over which events are saved
      - how many events we keep on disk

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

-- | A config used for asynchronous file writes
data AsyncWriteFileConfig = AsyncFileWriteConfig
  { _fileWriteEnvSem :: QSem
  -- ^ Semaphore representing file-write acquisition
  , _fileWriteEnvTimeout :: Int
  -- ^ The timeout length in microseconds
  }

Lens.makeLenses ''AsyncWriteFileConfig

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
  , _fileIndexerWriteEnv :: Maybe AsyncWriteFileConfig
  -- ^ Used to allow file writing to finish in the event of sigterm. Its presence means file writes
  --   should occur asynchronously
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
  -> Maybe Int
  -> FileStorageConfig meta event
  -> FileBuilder meta event
  -> EventBuilder meta event
  -> m (FileIndexer meta event)
mkFileIndexer path writeFilesAsyncTimeout storageCfg filenameBuilder' eventBuilder' = do
  liftIO $ createDirectoryIfMissing True path
  fileWriteTokens <-
    case writeFilesAsyncTimeout of
      Just timeoutValue -> liftIO $ do
        sem <- Con.newQSem 1
        pure $ Just (AsyncFileWriteConfig sem timeoutValue)
      Nothing -> pure Nothing
  let indexer =
        FileIndexer
          path
          storageCfg
          filenameBuilder'
          eventBuilder'
          genesis
          genesis
          fileWriteTokens
  lastStablePoint' <- fromMaybe genesis <$> readCurrentStable indexer
  let indexer' =
        indexer
          & fileIndexerLastSyncPoint .~ lastStablePoint'
          & fileIndexerLastStablePoint .~ lastStablePoint'
  rollback lastStablePoint' indexer'

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
          _other -> pure []
   in do
        files <- liftIO $ listDirectory eventDir
        join <$> traverse extractEventInfo files

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
      write :: (MonadIO m, MonadError IndexerError m) => FilePath -> ByteString -> m ()
      write filepath = handleIOErrors . writeIndexer indexer filepath
   in case timedEvent ^. event of
        Nothing -> when (indexer ^. storageConfig . keepEmptyEvent) $ write filename ""
        Just evt -> write filename (indexer ^. fileBuilder . serialiseEvent $ evt)

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
    cleanEvents timedEvent indexer
    writeTimedEvent timedEvent indexer
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
    pure $ indexer & fileIndexerLastSyncPoint .~ p

  setLastStablePoint p indexer = do
    let currentStable = indexer ^. fileIndexerLastStablePoint
    if p > currentStable
      then do
        indexer' <- writeStable p indexer
        pure $ indexer' & fileIndexerLastStablePoint .~ p
      else pure indexer

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

writeIndexer :: (MonadIO m) => FileIndexer meta event -> FilePath -> ByteString -> m ()
writeIndexer indexer =
  {- `isJust (indexer ^. fileIndexerWriteTokens) == True` means that we need to write the file
   asynchronously.

   It carries a semaphore which we wait on during the action. This means that, elsewhere in the
   program (given a termination command for example), we can determine when the write has
   finished. -}
  case indexer ^. fileIndexerWriteEnv of
    Just fileWriteEnv -> writeFileAsync fileWriteEnv
    Nothing -> writeFileSync

writeStable
  :: (MonadIO m)
  => Point event
  -> FileIndexer meta event
  -> m (FileIndexer meta event)
writeStable p indexer = do
  let filename = lastStableFilename indexer
      serialise = indexer ^. fileBuilder . serialisePoint
  writeIndexer indexer filename $ serialise p
  pure indexer

instance (MonadIO m, MonadError IndexerError m) => Closeable m (FileIndexer meta) where
  {- If we've got a write token it means a write is occurring asynchronously.
     We need to wait for it to finish before closing. -}
  close :: FileIndexer meta event -> m ()
  close indexer =
    case indexer ^. fileIndexerWriteEnv of
      Just fileWriteEnv -> do
        res <-
          liftIO $
            timeout
              (fileWriteEnv ^. fileWriteEnvTimeout)
              (Con.waitQSem (fileWriteEnv ^. fileWriteEnvSem))
        unless (isJust res) (throwError IndexerCloseTimeoutError)
      Nothing -> pure ()

-- * File writing | TODO https://input-output.atlassian.net/browse/PLT-7809
writeFileSync :: (MonadIO m) => FilePath -> ByteString -> m ()
writeFileSync filepath content = liftIO $ BS.writeFile filepath content

writeFileAsync :: (MonadIO m) => AsyncWriteFileConfig -> FilePath -> ByteString -> m ()
writeFileAsync fileWriteEnv filepath content =
  let sem = fileWriteEnv ^. fileWriteEnvSem
   in liftIO . void $
        forkIO $
          bracket_
            (Con.waitQSem sem)
            (Con.signalQSem sem)
            (BS.writeFile filepath content)
