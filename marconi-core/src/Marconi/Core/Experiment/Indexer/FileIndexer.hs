{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

-- | An indexer that stores its events to filedisk, using the provided serialisation
module Marconi.Core.Experiment.Indexer.FileIndexer (
  FileIndexer (FileIndexer),
  FileStorageConfig (FileStorageConfig),
  FileBuilder (FileBuilder),
  EventBuilder (EventBuilder),
  mkFileIndexer,
  eventDirectory,
  eventPrefix,
  eventSuffix,
  eventBuilder,
  deserialiseEvent,
  extractPoint,
  serialiseEvent,
  deserialiseTimedEvent,
  fileEventIdentifier,
  fileIndexerLastSyncPoint,
  directoryContentWithMeta,
  EventInfo (..),
) where

import Control.Exception (Exception (displayException), handle)
import Control.Lens qualified as Lens
import Control.Lens.Operators ((+~), (-~), (.~), (^.))
import Control.Monad (forM, when)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (execStateT, modify)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS
import Data.Foldable (Foldable (toList), traverse_)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List (genericLength, sortBy)
import Data.Text (Text)
import Data.Text qualified as Text
import Marconi.Core.Experiment.Class (
  Closeable (close),
  HasGenesis (genesis),
  IsIndex (index, indexAllDescending, rollback),
  IsSync (lastSyncPoint, lastSyncPoints),
 )
import Marconi.Core.Experiment.Type (
  IndexerError (IndexerInternalError),
  Point,
  QueryError (IndexerQueryError),
  Timed (Timed),
  event,
  point,
 )
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath ((</>))
import System.FilePath qualified as FilePath

data FileBuilder meta event = FileBuilder
  { _eventPrefix :: Text
  -- ^ The prefix used in the filename of the events
  , _eventSuffix :: Text
  -- ^ The suffix used in the filename of the events
  , _fileEventIdentifier :: Timed (Point event) (Maybe event) -> [Text]
  -- ^ build a list of identifier used in the filename of an event
  , _serialiseEvent :: event -> ByteString
  }

Lens.makeLenses ''FileBuilder

data EventBuilder meta event = EventBuilder
  { _deserialiseMeta :: [Text] -> Maybe meta
  , _extractPoint :: meta -> Point event
  , _deserialiseEvent :: meta -> ByteString -> Either Text (Maybe event)
  }

Lens.makeLenses ''EventBuilder

data FileStorageConfig meta = FileStorageConfig
  { _keepEmptyEvent :: Bool
  -- ^ Do we create a file for empty event
  , _keepAtMost :: Maybe Word
  -- ^ How many file do we want to keep, Nothing if unlimited (you probably don't want it)
  , _fileComparison :: meta -> meta -> Ordering
  , _currentStorageSize :: Word
  }

Lens.makeLenses ''FileStorageConfig

data FileIndexer meta event = FileIndexer
  { _eventDirectory :: FilePath
  -- ^ The directory wher we store the events
  , _storageConfig :: FileStorageConfig meta
  -- ^ Define storage management
  , _fileBuilder :: FileBuilder meta event
  -- ^ Describe how we build filename
  , _eventBuilder :: EventBuilder meta event
  -- ^ Gather deserialisation functions
  , _fileIndexerLastSyncPoint :: Point event
  -- ^ keep track of the last sync point
  }

Lens.makeLenses ''FileIndexer

mkFileIndexer
  :: (MonadIO m, MonadError IndexerError m, HasGenesis (Point event), Ord (Point event))
  => FilePath
  -> FileStorageConfig meta
  -> FileBuilder meta event
  -> EventBuilder meta event
  -> m (FileIndexer meta event)
mkFileIndexer path storageCfg filenameBuilder' eventBuilder' =
  let getEventsInfo = extractEventsInfo (eventBuilder' ^. deserialiseMeta) path
   in do
        liftIO $ createDirectoryIfMissing True path
        eventFiles <- runExceptT getEventsInfo
        (length', lastSyncPoint') <- case eventFiles of
          Left _ -> throwError $ IndexerInternalError "Invalid files in the indexer directory"
          Right [] -> pure (0, genesis)
          Right xs -> pure (genericLength xs, maximum $ (eventBuilder' ^. extractPoint) . metadata <$> xs)
        pure $
          FileIndexer
            path
            (storageCfg & currentStorageSize .~ length')
            filenameBuilder'
            eventBuilder'
            lastSyncPoint'

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

data EventInfo meta = EventInfo
  { hasContent :: Bool
  , metadata :: meta
  , path :: FilePath
  }

fullPath :: FileIndexer meta event -> EventInfo meta -> FilePath
fullPath indexer info = indexer ^. eventDirectory </> path info

extractEventsInfo
  :: (MonadIO m, MonadError (QueryError q) m)
  => ([Text] -> Maybe meta)
  -> FilePath
  -> m [EventInfo meta]
extractEventsInfo metaExtractor eventDir =
  let extractEventInfo path = do
        let filename = FilePath.dropExtension $ FilePath.takeFileName path
        case Text.splitOn "_" $ Text.pack filename of
          (_ : contentFlag : parts) -> do
            let hasContent' = contentFlag == "just"
            meta <- metaExtractor parts
            Just $ EventInfo hasContent' meta path
          _ -> Nothing
   in do
        files <- liftIO $ listDirectory eventDir
        case traverse extractEventInfo files of
          Nothing -> throwError $ IndexerQueryError $ "Invalid file in directory: " <> Text.pack eventDir
          Just points -> pure points

directoryContentWithMeta
  :: (MonadIO m, MonadError (QueryError q) m)
  => FileIndexer meta event
  -> m [EventInfo meta]
directoryContentWithMeta indexer = do
  let eventDir = indexer ^. eventDirectory
  extractEventsInfo (indexer ^. eventBuilder . deserialiseMeta) eventDir

deserialiseTimedEvent
  :: (MonadIO m)
  => FileIndexer meta event
  -> EventInfo meta
  -> ExceptT Text m (Timed (Point event) (Maybe event))
deserialiseTimedEvent indexer eventFile = do
  let meta = metadata eventFile
      deserialise = (indexer ^. eventBuilder . deserialiseEvent) meta
      pt = indexer ^. eventBuilder . extractPoint $ meta
  evt <-
    if hasContent eventFile
      then ExceptT $ deserialise <$> liftIO (BS.readFile $ fullPath indexer eventFile)
      else pure Nothing
  pure $ Timed pt evt

instance (Applicative m) => IsSync m event (FileIndexer meta) where
  lastSyncPoint = pure . Lens.view fileIndexerLastSyncPoint
  lastSyncPoints _n = pure . lastSyncPoint -- TODO do better

writeTimedEvent
  :: (MonadIO m, MonadError IndexerError m)
  => Timed (Point event) (Maybe event)
  -> FileIndexer meta event
  -> m (FileIndexer meta event)
writeTimedEvent timedEvent indexer =
  let filename = toFilename (indexer ^. eventDirectory) (indexer ^. fileBuilder) timedEvent
   in case timedEvent ^. event of
        Nothing ->
          if indexer ^. storageConfig . keepEmptyEvent
            then writeIndexerFile filename "" indexer
            else pure indexer
        Just evt -> writeIndexerFile filename (indexer ^. fileBuilder . serialiseEvent $ evt) indexer

writeIndexerFile
  :: (MonadIO m, MonadError IndexerError m)
  => FilePath
  -> ByteString
  -> FileIndexer meta event
  -> m (FileIndexer meta event)
writeIndexerFile filename content indexer = do
  handleIOErrors $ BS.writeFile filename content
  let currentSize = indexer ^. storageConfig . currentStorageSize
      maxSize = indexer ^. storageConfig . keepAtMost
  if maybe True (currentSize <) maxSize
    then pure $ indexer & storageConfig . currentStorageSize +~ 1
    else removeOldest indexer $> indexer

removeOldest
  :: (MonadIO m, MonadError IndexerError m)
  => FileIndexer meta event
  -> m ()
removeOldest indexer = do
  content <- runExceptT $ directoryContentWithMeta indexer
  case content of
    Left _err -> throwError $ IndexerInternalError "can't read directory content"
    Right c -> do
      let content' = sortBy (\x y -> (indexer ^. storageConfig . fileComparison) (metadata x) (metadata y)) c
      case content' of
        [] -> pure ()
        (x : _) -> liftIO $ removeFile $ fullPath indexer x

instance (MonadIO m, MonadError IndexerError m) => IsIndex m event (FileIndexer meta) where
  index timedEvent indexer = do
    let currentPoint = timedEvent ^. point
        setLastSync ix = ix & fileIndexerLastSyncPoint .~ currentPoint
    setLastSync <$> writeTimedEvent timedEvent indexer

  indexAllDescending timedEvents indexer =
    case toList timedEvents of
      [] -> pure indexer
      x : _ -> do
        let currentPoint = x ^. point
            setLastSync ix = ix & fileIndexerLastSyncPoint .~ currentPoint
        traverse_ (flip writeTimedEvent indexer) timedEvents
        pure $ setLastSync indexer

  rollback p indexer = do
    filesWithMetadata <- runExceptT $ directoryContentWithMeta indexer
    nbRemoved <- case filesWithMetadata of
      Left _err -> throwError $ IndexerInternalError "can't parse directory content"
      Right xs -> flip execStateT 0 $ forM xs $ \eventFile -> do
        let pt = indexer ^. eventBuilder . extractPoint $ metadata eventFile
        when (pt > p) $ do
          let filename = fullPath indexer eventFile
          liftIO $ removeFile filename
          modify succ
    pure $ indexer & storageConfig . currentStorageSize -~ nbRemoved

instance (Applicative m) => Closeable m (FileIndexer meta) where
  close = const $ pure ()
