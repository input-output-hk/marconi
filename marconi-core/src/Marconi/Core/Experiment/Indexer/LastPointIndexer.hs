{-# LANGUAGE UndecidableInstances #-}

{- |
    On-disk indexer backed by a sqlite database.

    See "Marconi.Core.Experiment" for documentation.
-}
module Marconi.Core.Experiment.Indexer.LastPointIndexer (
  LastPointIndexer,
  lastPoint,
  lastPointIndexer,
) where

import Control.Lens (folded, makeLenses, maximumOf, view)
import Control.Lens.Operators ((^.))
import Data.Maybe (fromMaybe)

import Marconi.Core.Experiment.Class (
  HasGenesis (genesis),
  IsIndex (index, indexAllDescending),
  IsSync (lastSyncPoint),
  Rollbackable (rollback),
 )
import Marconi.Core.Experiment.Type (Point, point)

{- | LastPointIndexer.
 An indexer that does nothing except keeping track of the last point.
 While it may sound useless,
 it can be usefull when you want to benefit of the capabilities of a transformer.
-}
newtype LastPointIndexer event = LastPointIndexer {_lastPoint :: Point event}

deriving stock instance (Show event, Show (Point event)) => Show (LastPointIndexer event)

makeLenses 'LastPointIndexer

-- | A smart constructor for 'LastPointIndexer'
lastPointIndexer :: HasGenesis (Point event) => LastPointIndexer event
lastPointIndexer = LastPointIndexer genesis

instance
  (HasGenesis (Point event), Monad m)
  => IsIndex m event LastPointIndexer
  where
  index timedEvent _ = pure $ LastPointIndexer $ timedEvent ^. point

  indexAllDescending evts _ = pure $ LastPointIndexer $ fromMaybe genesis $ maximumOf (folded . point) evts

instance Applicative m => IsSync m event LastPointIndexer where
  lastSyncPoint = pure . view lastPoint

instance Applicative m => Rollbackable m event LastPointIndexer where
  rollback p _ = pure $ LastPointIndexer p
