{-# LANGUAGE DerivingVia #-}

module Marconi.Core.Transformer.WithStream (
  WithStream (..),
  withStream,
) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, dupTChan, writeTChan)
import Control.Lens (makeLenses, traverseOf_, (^.), _Just)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Marconi.Core.Class (
  Closeable,
  IsIndex (index, rollback, setLastStablePoint),
  IsSync,
  Queryable,
 )
import Marconi.Core.Transformer.Class (IndexerTrans, unwrap)
import Marconi.Core.Transformer.IndexTransformer (
  IndexTransformer (IndexTransformer),
  indexVia,
  rollbackVia,
  setLastStablePointVia,
  wrappedIndexer,
 )
import Marconi.Core.Type (IndexerError, Point, event)

data WithStreamConfig r event = WithStreamConfig
  { _withStreamConfigMap :: event -> r
  , _withStreamConfigChan :: TChan r
  }

newtype WithStream r indexer event = WithStream {_mappingWrapper :: IndexTransformer (WithStreamConfig r) indexer event}

makeLenses 'WithStream

deriving via
  (IndexTransformer (WithStreamConfig r) indexer)
  instance
    (IsSync m event indexer) => IsSync m event (WithStream r indexer)

deriving via
  (IndexTransformer (WithStreamConfig r) indexer)
  instance
    (Queryable m event query indexer) => Queryable m event query (WithStream r indexer)

deriving via
  (IndexTransformer (WithStreamConfig r) indexer)
  instance
    (Closeable m indexer) => Closeable m (WithStream r indexer)

-- | A smart constructor for @WithStream@
withStream
  :: (event -> r)
  -> TChan r
  -> indexer event
  -> IO (WithStream r indexer event)
withStream mapping broadcastChan idx = do
  chan <- atomically $ dupTChan broadcastChan
  pure $ WithStream $ IndexTransformer (WithStreamConfig mapping chan) idx

instance IndexerTrans (WithStream r) where
  unwrap = mappingWrapper . wrappedIndexer

instance
  (Point r ~ Point event, MonadIO m, MonadError IndexerError m, IsIndex m event indexer)
  => IsIndex m event (WithStream r indexer)
  where
  index timedEvent idx@(WithStream (IndexTransformer (WithStreamConfig f chan) _)) = do
    pushEvent (timedEvent ^. event)
    indexVia unwrap timedEvent idx
    where
      pushEvent = traverseOf_ _Just $ \x -> liftIO $ atomically $ writeTChan chan (f x)
  rollback = rollbackVia unwrap
  setLastStablePoint = setLastStablePointVia unwrap
