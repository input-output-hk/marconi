{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
   An indexer that tracks the last event received from the source.

   On rollback we can either decide to remove the last event or to keep the last one we received.
-}
module Marconi.Core.Indexer.LastEventIndexer (
  LastEventIndexer (LastEventIndexer),
  mkLastEventIndexer,
  LastEventConfig (LastEventConfig),
  GetLastQuery (GetLastQuery),
) where

import Control.Lens (Lens', (&), (.~), (?~), (^.))
import Control.Lens qualified as Lens
import Control.Monad (void, when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Marconi.Core.Class (
  Closeable,
  HasGenesis (genesis),
  IsIndex (index, indexAll, indexAllDescending, rollback, setLastStablePoint),
  IsSync (lastStablePoint, lastSyncPoint),
  Queryable (query),
  close,
 )
import Marconi.Core.Type (
  IndexerError (IndexerInternalError),
  Point,
  QueryError (IndexerQueryError),
  Result,
  event,
  point,
 )
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

data LastEventConfig event = LastEventConfig
  { _configDirectory :: FilePath
  -- ^ The directories where we save content
  , _configCleanOnRollback :: Bool
  -- ^ Do we remove the stored event on rollback
  , _configSerialisePoint :: Point event -> ByteString
  -- ^ The serialisation function for points
  , _configDeserialisePoint :: ByteString -> Either Text (Point event)
  -- ^ The deserialisation function for points
  , _configSerialiseEvent :: event -> ByteString
  -- ^ The serialisation function for events
  , _configDeserialiseEvent :: ByteString -> Either Text event
  -- ^ The deserialisation function for events
  , _configSaveEvery :: Word
  -- ^ How often do we save last event
  }

Lens.makeLenses ''LastEventConfig

-- The stateful part of the LastEventIndexer
data LastEventState event = LastEventState
  { _stateLastEvent :: Maybe event
  -- ^ Store the last event
  , _stateLastSync :: Point event
  -- ^ Store the last sync point
  , _stateLastStablePoint :: Point event
  -- ^ Store the last stable point
  , _stateTimeToNextSave :: Word
  -- ^ Time before next snapshot
  }

Lens.makeLenses ''LastEventState

{- | An indexer that only track the latest value of an event
On rollback, we either delete the value or keep the one provided before the rollback,
depending on the provided configuration.
-}
data LastEventIndexer event = LastEventIndexer
  { _config :: LastEventConfig event
  , _state :: LastEventState event
  }

Lens.makeLenses ''LastEventIndexer

saveDirectory :: Lens' (LastEventIndexer event) FilePath
saveDirectory = config . configDirectory

deserialisePoint :: Lens' (LastEventIndexer event) (ByteString -> Either Text (Point event))
deserialisePoint = config . configDeserialisePoint

serialisePoint :: Lens' (LastEventIndexer event) (Point event -> ByteString)
serialisePoint = config . configSerialisePoint

deserialiseEvent :: Lens' (LastEventIndexer event) (ByteString -> Either Text event)
deserialiseEvent = config . configDeserialiseEvent

serialiseEvent :: Lens' (LastEventIndexer event) (event -> ByteString)
serialiseEvent = config . configSerialiseEvent

cleanOnRollback :: Lens' (LastEventIndexer event) Bool
cleanOnRollback = config . configCleanOnRollback

lastEvent :: Lens' (LastEventIndexer event) (Maybe event)
lastEvent = state . stateLastEvent

lastSync :: Lens' (LastEventIndexer event) (Point event)
lastSync = state . stateLastSync

indexerLastStablePoint :: Lens' (LastEventIndexer event) (Point event)
indexerLastStablePoint = state . stateLastStablePoint

saveEvery :: Lens' (LastEventIndexer event) Word
saveEvery = config . configSaveEvery

timeToNextSave :: Lens' (LastEventIndexer event) Word
timeToNextSave = state . stateTimeToNextSave

-- Advance the indexer towards the next snapshot.
-- If it times, reset the timer and returns true, along with the modified indexer.
tickSave :: LastEventIndexer event -> (Bool, LastEventIndexer event)
tickSave indexer = do
  let timeToNextSave' = indexer ^. timeToNextSave . Lens.to pred
  if timeToNextSave' == 0
    then (True, indexer & timeToNextSave .~ indexer ^. saveEvery)
    else (False, indexer & timeToNextSave .~ timeToNextSave')

instance (MonadIO m, MonadError IndexerError m) => IsIndex m a LastEventIndexer where
  index timedEvent indexer = do
    -- We only keep the last non empty event, so we don't update the lastEvent field on empty events
    let setLastEvent = case timedEvent ^. event of
          Nothing -> id
          Just e -> lastEvent ?~ e
        (saveTime, indexer') =
          tickSave $
            indexer
              & lastSync .~ (timedEvent ^. point)
              & setLastEvent
    when saveTime (savePoint indexer')
    pure indexer'

  indexAll = indexAllDescending . reverse . toList

  indexAllDescending xs = case toList xs of
    [] -> pure
    x : _ -> index x

  rollback p indexer =
    pure $
      if indexer ^. cleanOnRollback
        then indexer & lastSync .~ p & lastEvent .~ Nothing
        else indexer & lastSync .~ p

  setLastStablePoint p indexer = do
    let currentStable = indexer ^. indexerLastStablePoint
     in if p > currentStable
          then do
            void $ writeStable p indexer
            pure $ indexer & indexerLastStablePoint .~ p
          else do
            pure indexer

instance (Applicative m) => IsSync m event LastEventIndexer where
  lastSyncPoint = pure . Lens.view lastSync
  lastStablePoint = pure . Lens.view indexerLastStablePoint

instance (MonadIO m) => Closeable m LastEventIndexer where
  -- On close, we perform a last save of the stored values
  close = savePoint

mkLastEventIndexer
  :: (HasGenesis (Point event), MonadIO m, MonadError IndexerError m)
  => LastEventConfig event
  -> m (LastEventIndexer event)
mkLastEventIndexer cfg = do
  liftIO $ createDirectoryIfMissing True (cfg ^. configDirectory)
  let initialState = LastEventState Nothing genesis genesis (cfg ^. configSaveEvery)
      indexer = LastEventIndexer cfg initialState
  lastStablePoint' <- fromMaybe genesis <$> readCurrentStable indexer
  pure $
    indexer
      & indexerLastStablePoint .~ lastStablePoint'
      & lastSync .~ lastStablePoint'

-- | Datatype used to query the stored value of a 'LastEventIndexer'
data GetLastQuery a = GetLastQuery

type instance Result (GetLastQuery a) = Maybe a

instance
  (MonadIO m, MonadError (QueryError (GetLastQuery event)) m)
  => Queryable m event (GetLastQuery event) LastEventIndexer
  where
  query _point _query indexer =
    let lastEventFile = eventFilename indexer
        queryFromDisk = do
          hasEvent <- liftIO $ doesFileExist lastEventFile
          if hasEvent
            then do
              content <- liftIO $ BS.readFile lastEventFile
              let result = indexer ^. deserialiseEvent $ content
              case result of
                Left err -> throwError $ IndexerQueryError err
                Right res -> pure $ Just res
            else pure Nothing
     in maybe queryFromDisk (pure . Just) $ indexer ^. lastEvent

savePoint :: (MonadIO m) => LastEventIndexer a -> m ()
savePoint indexer = do
  void $ writeStable (indexer ^. indexerLastStablePoint) indexer
  maybe (pure ()) (flip writeEvent indexer) (indexer ^. lastEvent)

lastStableFilename :: LastEventIndexer event -> FilePath
lastStableFilename indexer =
  let
    dir = indexer ^. saveDirectory
    filename = "latestStable.cbor"
   in
    dir </> Text.unpack filename

readCurrentStable
  :: (MonadIO m, MonadError IndexerError m)
  => LastEventIndexer event
  -> m (Maybe (Point event))
readCurrentStable indexer = do
  let f = lastStableFilename indexer
      deserialise = indexer ^. deserialisePoint
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
  -> LastEventIndexer event
  -> m ()
writeStable p indexer = do
  let f = lastStableFilename indexer
      serialise = indexer ^. serialisePoint
  liftIO $ BS.writeFile f $ serialise p

eventFilename :: LastEventIndexer event -> FilePath
eventFilename indexer =
  let
    dir = indexer ^. saveDirectory
    filename = "event.cbor"
   in
    dir </> filename

writeEvent
  :: (MonadIO m)
  => event
  -> LastEventIndexer event
  -> m ()
writeEvent p indexer = do
  let f = eventFilename indexer
      serialise = indexer ^. serialiseEvent
  liftIO $ BS.writeFile f $ serialise p
