{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}

{- |
  A transformer that fans events into an arbitrary action.

 The configuration is a function @Timed (Point event) event -> IO ()@

 See Marconi.Core.Transformer.WithStream.Socket and Marconi.Core.Transformer.WithStream.TBQueue
 for example usages.
-}
module Marconi.Core.Transformer.WithAction (
  WithAction (..),
  withActionWrapper,
  WithActionConfig (..),
  withActionConfigAction,
) where

import Control.Lens (makeLenses)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
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
import Marconi.Core.Type (IndexerError, Point, Timed)

-- | Configuration is a function @Timed (Point event) event -> IO ()@
newtype WithActionConfig event = WithActionConfig
  { _withActionConfigAction :: Timed (Point event) event -> IO ()
  }

-- | An indexer which runs an action on events during indexing
newtype WithAction indexer event = WithAction
  { _withActionWrapper :: IndexTransformer WithActionConfig indexer event
  }

makeLenses 'WithAction
makeLenses 'WithActionConfig

deriving via
  (IndexTransformer WithActionConfig indexer)
  instance
    (IsSync m event indexer) => IsSync m event (WithAction indexer)

deriving via
  (IndexTransformer WithActionConfig indexer)
  instance
    (Queryable m event query indexer) => Queryable m event query (WithAction indexer)

deriving via
  (IndexTransformer WithActionConfig indexer)
  instance
    (Closeable m indexer) => Closeable m (WithAction indexer)

instance IndexerTrans WithAction where
  unwrap = withActionWrapper . wrappedIndexer

instance
  (MonadIO m, MonadError IndexerError m, IsIndex m event indexer)
  => IsIndex m event (WithAction indexer)
  where
  index timedEvent idx@(WithAction (IndexTransformer (WithActionConfig stream) _)) = do
    res <- indexVia unwrap timedEvent idx
    liftIO $ traverse_ stream (sequence timedEvent)
    pure res
  rollback = rollbackVia unwrap
  setLastStablePoint = setLastStablePointVia unwrap
