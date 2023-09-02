{-# LANGUAGE TemplateHaskell #-}

-- | An indexer that stores its events to filedisk, using the provided serialisation
module Marconi.Core.Experiment.Indexer.FileIndexer (
  FileIndexer (FileIndexer),
  mkFileIndexer,
  eventDirectory,
  eventPrefix,
  eventSuffix,
  serialiseEvent,
  deserialiseEvent,
  deserialiseTimedEvent,
  fileEventIdentifier,
  filePointExtractor,
  fileIndexerLastSyncPoint,
  directoryContentWithMeta,
  EventFile (..),
) where

import Control.Exception (Exception (displayException), handle)
import Control.Lens qualified as Lens
import Control.Lens.Operators ((.~), (^.))
import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS
import Data.Foldable (Foldable (toList), traverse_)
import Data.Function ((&))
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

data FileIndexer meta event = FileIndexer
  { _eventDirectory :: FilePath
  -- ^ The directory wher we store the events
  , _keepEmptyEvent :: Bool
  , _eventPrefix :: Text
  -- ^ The prefix used in the filename of the events
  , _eventSuffix :: Text
  -- ^ The suffix used in the filename of the events
  , _serialiseEvent :: event -> ByteString
  , _deserialiseEvent :: meta -> ByteString -> Either Text (Maybe event)
  , _fileEventIdentifier :: Timed (Point event) (Maybe event) -> [Text]
  -- ^ build a list of identifier used in the filename of an event
  , _fileMetaExtractor :: [Text] -> Maybe meta
  , _filePointExtractor :: meta -> Point event
  , _fileIndexerLastSyncPoint :: Point event
  -- ^ keep track of the last sync point
  }

Lens.makeLenses ''FileIndexer

mkFileIndexer
  :: (MonadIO m, MonadError IndexerError m, HasGenesis (Point event), Ord (Point event))
  => FilePath
  -> Bool
  -> Text
  -> Text
  -> (event -> ByteString)
  -> (meta -> ByteString -> Either Text (Maybe event))
  -> (Timed (Point event) (Maybe event) -> [Text])
  -> ([Text] -> Maybe meta)
  -> (meta -> Point event)
  -> m (FileIndexer meta event)
mkFileIndexer path keepEmpty prefix suffix serialise deserialise identifier getMeta getPoint =
  let getLastPoints = extractEventFiles getMeta path
   in do
        liftIO $ createDirectoryIfMissing True path
        eventFiles <- runExceptT getLastPoints
        lastSyncPoint' <- case eventFiles of
          Left _ -> throwError $ IndexerInternalError "Invalid files in the indexer directory"
          Right [] -> pure genesis
          Right xs -> pure $ maximum $ getPoint . metadata <$> xs
        pure $
          FileIndexer
            path
            keepEmpty
            prefix
            suffix
            serialise
            deserialise
            identifier
            getMeta
            getPoint
            lastSyncPoint'

toFilename :: FileIndexer meta event -> Timed (Point event) (Maybe event) -> FilePath
toFilename indexer evt =
  let eventId = (indexer ^. fileEventIdentifier) evt
      contentFlag = case evt ^. event of
        Just _ -> "just"
        Nothing -> "nothing"
      filename =
        Text.unpack $
          (Text.intercalate "_" $ indexer ^. eventPrefix : contentFlag : eventId)
            <> "."
            <> (indexer ^. eventSuffix)
   in (indexer ^. eventDirectory) </> filename

handleIOErrors :: (MonadIO m, MonadError IndexerError m) => IO a -> m a
handleIOErrors action = do
  let throwIOError :: IOError -> IO (Either IndexerError b)
      throwIOError e = pure $ Left $ IndexerInternalError . Text.pack $ displayException e
  result <- liftIO $ handle throwIOError (Right <$> action)
  either throwError pure result

data EventFile meta = EventFile
  { hasContent :: Bool
  , metadata :: meta
  , path :: FilePath
  }

extractEventFiles
  :: (MonadIO m, MonadError (QueryError q) m)
  => ([Text] -> Maybe meta)
  -> FilePath
  -> m [EventFile meta]
extractEventFiles metaExtractor eventDir =
  let extractPoint path = do
        let filename = FilePath.dropExtension $ FilePath.takeFileName path
        case Text.splitOn "_" $ Text.pack filename of
          (_ : contentFlag : parts) -> do
            let hasContent' = contentFlag == "just"
            meta <- metaExtractor parts
            Just $ EventFile hasContent' meta path
          _ -> Nothing
   in do
        files <- liftIO $ listDirectory eventDir
        case traverse extractPoint files of
          Nothing -> throwError $ IndexerQueryError $ "Invalid file in directory: " <> Text.pack eventDir
          Just points -> pure points

directoryContentWithMeta
  :: (MonadIO m, MonadError (QueryError q) m)
  => FileIndexer meta event
  -> m [EventFile meta]
directoryContentWithMeta indexer = do
  let eventDir = indexer ^. eventDirectory
  extractEventFiles (indexer ^. fileMetaExtractor) eventDir

deserialiseTimedEvent
  :: (MonadIO m)
  => FileIndexer meta event
  -> EventFile meta
  -> ExceptT Text m (Timed (Point event) (Maybe event))
deserialiseTimedEvent indexer eventFile = do
  let meta = metadata eventFile
      deserialise = (indexer ^. deserialiseEvent) meta
      pt = indexer ^. filePointExtractor $ meta
  evt <-
    if hasContent eventFile
      then ExceptT $ deserialise <$> liftIO (BS.readFile $ path eventFile)
      else pure Nothing
  pure $ Timed pt evt

instance (Applicative m) => IsSync m event (FileIndexer meta) where
  lastSyncPoint = pure . Lens.view fileIndexerLastSyncPoint
  lastSyncPoints _n = pure . lastSyncPoint -- TODO do better

writeTimedEvent
  :: (MonadIO m, MonadError IndexerError m)
  => Timed (Point event) (Maybe event)
  -> FileIndexer meta event
  -> m ()
writeTimedEvent timedEvent indexer =
  let filename = toFilename indexer timedEvent
   in case timedEvent ^. event of
        Nothing -> when (indexer ^. keepEmptyEvent) $ writeIndexerFile filename ""
        Just evt -> writeIndexerFile filename $ indexer ^. serialiseEvent $ evt

writeIndexerFile :: (MonadIO m, MonadError IndexerError m) => FilePath -> ByteString -> m ()
writeIndexerFile filename = handleIOErrors . BS.writeFile filename

instance (MonadIO m, MonadError IndexerError m) => IsIndex m event (FileIndexer meta) where
  index timedEvent indexer = do
    let currentPoint = timedEvent ^. point
        setLastSync ix = ix & fileIndexerLastSyncPoint .~ currentPoint
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
    filesWithMetadata <- runExceptT $ directoryContentWithMeta indexer
    case filesWithMetadata of
      Left _err -> throwError $ IndexerInternalError "can't parse directory content"
      Right xs -> forM_ xs $ \eventFile -> do
        let pt = indexer ^. filePointExtractor $ metadata eventFile
        when (pt > p) $ liftIO $ removeFile $ path eventFile
    pure indexer

instance (Applicative m) => Closeable m (FileIndexer meta) where
  close = const $ pure ()
