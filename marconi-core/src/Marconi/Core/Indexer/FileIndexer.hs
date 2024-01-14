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
  FileCleanup,
  withPartition,
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
  eventsMetadata,
  EventInfo (..),
) where

import Control.Concurrent.QSem (QSem)
import Control.Concurrent.QSem qualified as Con
import Control.Exception (Exception (displayException), bracket_, handle)
import Control.Lens (Traversal')
import Control.Lens qualified as Lens
import Control.Lens.Operators ((%~), (.=), (.~), (?~), (^.))
import Control.Monad (forM_, join, unless, when)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BS.Builder
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS
import Data.Foldable (Foldable (toList), traverse_)
import Data.Function ((&))
import Data.List qualified as List
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text qualified as Text
import Marconi.Core.Class (
  Closeable (close),
  HasGenesis (genesis),
  IsIndex (index, indexAll, indexAllDescending, rollback, setLastStablePoint),
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
import System.Directory qualified as Directory
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
  , _serialiseEvent :: event -> Builder
  , _serialisePoint :: Point event -> Builder
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

data FileCleanup meta event = FileCleanUp
  { _eventsInfo :: [EventInfo meta]
  -- ^ store the events info of the current files
  , _partitionEvents
      :: Timed (Point event) (Maybe event)
      -> [EventInfo meta]
      -> ([EventInfo meta], [EventInfo meta])
  -- ^ The first part are the kept events, the second the removed events
  }

Lens.makeLenses ''FileCleanup

withPartition
  :: (Timed (Point event) (Maybe event) -> [EventInfo meta] -> ([EventInfo meta], [EventInfo meta]))
  -> Maybe (FileCleanup meta event)
withPartition = Just . FileCleanUp []

decideCleanUp
  :: Timed (Point event) (Maybe event) -> State (FileCleanup meta event) [EventInfo meta]
decideCleanUp evt = do
  s <- State.get
  let (keep, remove) = (s ^. partitionEvents $ evt) (s ^. eventsInfo)
  eventsInfo .= keep
  pure remove

{- | The datatype used to configure the way we store files, including:

      - control over which events are saved
      - how many events we keep on disk

    Be careful in the choice of the function used to remove events
    as you probably don't want to store all the events on disk.
-}
data FileStorageConfig meta event = FileStorageConfig
  { _keepEmptyEvent :: Bool
  -- ^ Do we create a file for empty event
  , _cleanupConfig :: Maybe (FileCleanup meta event)
  -- ^ Filtering function used to decide which events must be removed
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

eventsMetadata :: Traversal' (FileIndexer meta event) [EventInfo meta]
eventsMetadata = storageConfig . cleanupConfig . traverse . eventsInfo

timedEventInfo
  :: FileIndexer meta event
  -> Timed (Point event) (Maybe event)
  -> Maybe (EventInfo meta)
timedEventInfo indexer evt =
  let meta =
        (indexer ^. eventBuilder . deserialiseMeta)
          . (indexer ^. fileBuilder . fileEventIdentifier)
          $ evt
      hasContent = isJust $ evt ^. event
      path = toFilename indexer evt
   in (\m -> EventInfo hasContent m path) <$> meta

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
mkFileIndexer path' writeFilesAsyncTimeout storageCfg filenameBuilder' eventBuilder' = do
  liftIO $ Directory.createDirectoryIfMissing True path'
  fileWriteTokens <-
    case writeFilesAsyncTimeout of
      Just timeoutValue -> liftIO $ do
        sem <- Con.newQSem 1
        pure $ Just (AsyncFileWriteConfig sem timeoutValue)
      Nothing -> pure Nothing
  let indexer =
        FileIndexer
          path'
          storageCfg
          filenameBuilder'
          eventBuilder'
          genesis
          genesis
          fileWriteTokens
  lastStablePoint' <- fromMaybe genesis <$> readCurrentStable indexer
  unsortedMetadata <- runExceptT $ getDirectoryMetadata indexer
  let metadata = case unsortedMetadata of
        Left _err -> []
        Right xs -> List.sortOn (Down . (eventBuilder' ^. extractPoint) . fileMetadata) xs
  let indexer' =
        indexer
          & fileIndexerLastSyncPoint .~ lastStablePoint'
          & fileIndexerLastStablePoint .~ lastStablePoint'
          & eventsMetadata .~ metadata
  rollback lastStablePoint' indexer'

toFilename :: FileIndexer meta event -> Timed (Point event) (Maybe event) -> FilePath
toFilename indexer evt =
  let dir = indexer ^. eventDirectory
      eventId = (indexer ^. fileBuilder . fileEventIdentifier) evt
      contentFlag = case evt ^. event of
        Just _ -> "just"
        Nothing -> "nothing"
      filename =
        Text.unpack $
          (Text.intercalate "_" $ indexer ^. fileBuilder . eventPrefix : contentFlag : eventId)
            <> "."
            <> (indexer ^. fileBuilder . eventSuffix)
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
        files <- liftIO $ Directory.listDirectory eventDir
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
  -> m (FileIndexer meta event)
writeTimedEvent timedEvent indexer = do
  let filename = toFilename indexer timedEvent
      eventInfo = timedEventInfo indexer timedEvent
      write :: (MonadIO m, MonadError IndexerError m) => FilePath -> Builder -> m ()
      write filepath = handleIOErrors . writeIndexer indexer filepath
  case timedEvent ^. event of
    Nothing -> when (indexer ^. storageConfig . keepEmptyEvent) $ write filename ""
    Just evt -> write filename (indexer ^. fileBuilder . serialiseEvent $ evt)
  pure $ indexer & maybe id addInfo eventInfo

cleanEvents
  :: (MonadIO io)
  => Timed (Point event) (Maybe event)
  -> FileIndexer meta event
  -> io (FileIndexer meta event)
cleanEvents timedEvent indexer =
  let cleanupConfigM = indexer ^. storageConfig . cleanupConfig
   in case cleanupConfigM of
        Nothing -> pure indexer
        Just cleanupConfig' -> do
          let (toRemove, cleanupConfig'') = State.runState (decideCleanUp timedEvent) cleanupConfig'
          liftIO $ traverse_ Directory.removeFile (fullPath indexer <$> toRemove)
          pure $ indexer & storageConfig . cleanupConfig ?~ cleanupConfig''

instance (MonadIO io, MonadError IndexerError io) => IsIndex io event (FileIndexer meta) where
  index timedEvent indexer = do
    let currentPoint = timedEvent ^. point
        setLastSync ix = ix & fileIndexerLastSyncPoint .~ currentPoint
    indexer' <- cleanEvents timedEvent indexer
    indexer'' <- writeTimedEvent timedEvent indexer'
    pure $ setLastSync indexer''

  indexAll timedEvents indexer =
    case toList timedEvents of
      [] -> pure indexer
      x : _ -> do
        let currentPoint = x ^. point
            setLastSync ix = ix & fileIndexerLastSyncPoint .~ currentPoint
        traverse_ (flip writeTimedEvent indexer) timedEvents
        pure $ setLastSync indexer

  indexAllDescending = indexAll . reverse . toList

  rollback p indexer = do
    let filesWithMetadata = indexer ^. eventsMetadata
        extractPoint' = indexer ^. eventBuilder . extractPoint
        (keep, remove) = List.partition ((<= p) . extractPoint' . fileMetadata) filesWithMetadata
    forM_ remove $ \eventFile -> do
      let filename = fullPath indexer eventFile
      liftIO $ Directory.removeFile filename
    let indexer' =
          indexer
            & fileIndexerLastSyncPoint .~ p
            & eventsMetadata .~ keep
    pure indexer'

  setLastStablePoint p indexer = do
    let currentStable = indexer ^. fileIndexerLastStablePoint
    if p > currentStable
      then do
        indexer' <- writeStable p indexer
        pure $ indexer' & fileIndexerLastStablePoint .~ p
      else pure indexer

addInfo :: EventInfo meta -> FileIndexer meta event -> FileIndexer meta event
addInfo info indexer = indexer & eventsMetadata %~ (info :)

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
  fileExists <- liftIO $ Directory.doesFileExist f
  if fileExists
    then do
      res <- deserialise <$> liftIO (BS.readFile f)
      case res of
        Left _ -> throwError $ IndexerInternalError "Can't read current stable"
        Right r -> pure $ Just r
    else pure Nothing

writeIndexer :: (MonadIO m) => FileIndexer meta event -> FilePath -> Builder -> m ()
writeIndexer indexer =
  {- `isJust (indexer ^. fileIndexerWriteEnv) == True` means that we need to write the file
   asynchronously.

   It carries a semaphore which we wait on during the action. This means that, elsewhere in the
   program (given a termination command for example), we can determine when the write has
   finished. -}
  maybe writeFileSync writeFileAsync (indexer ^. fileIndexerWriteEnv)

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
  close indexer = do
    case indexer ^. fileIndexerWriteEnv of
      Just fileWriteEnv -> do
        res <-
          liftIO $
            timeout
              (fileWriteEnv ^. fileWriteEnvTimeout)
              (Con.waitQSem (fileWriteEnv ^. fileWriteEnvSem))
        unless (isJust res) (throwError IndexerCloseTimeoutError)
      Nothing -> pure ()

-- * File writing

writeFileSync :: (MonadIO m) => FilePath -> Builder -> m ()
-- TODO https://input-output.atlassian.net/browse/PLT-7809
writeFileSync filepath content = liftIO $ BS.Builder.writeFile filepath content

writeFileAsync :: (MonadIO m) => AsyncWriteFileConfig -> FilePath -> Builder -> m ()
writeFileAsync fileWriteEnv filepath content = do
  let sem = fileWriteEnv ^. fileWriteEnvSem
  liftIO $
    bracket_
      (Con.waitQSem sem)
      (Con.signalQSem sem)
      (BS.Builder.writeFile filepath content)
