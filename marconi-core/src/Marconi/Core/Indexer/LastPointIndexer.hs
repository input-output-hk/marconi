{-# LANGUAGE UndecidableInstances #-}

{- |
    On-disk indexer backed by a sqlite database.

    See "Marconi.Core" for documentation.
-}
module Marconi.Core.Indexer.LastPointIndexer (
  LastPointIndexer,
  lastPointIndexer,
) where

import Control.Lens (makeLenses, view)
import Control.Lens.Operators ((.~), (^.))
import Data.Foldable (Foldable (toList))
import Marconi.Core.Class (
  HasGenesis (genesis),
  IsIndex (index, indexAllDescending, rollback, setLastStablePoint),
  IsSync (lastStablePoint, lastSyncPoint),
 )
import Marconi.Core.Type (Point, point)

{- | LastPointIndexer.
 An indexer that does nothing except keeping track of the last point.
 While it may sound useless,
 it can be usefull when you want to benefit of the capabilities of a transformer.
-}
data LastPointIndexer event = LastPointIndexer
  { _latestSyncPoint :: Point event
  , _latestStablePoint :: Point event
  }

deriving stock instance (Show event, Show (Point event)) => Show (LastPointIndexer event)

makeLenses 'LastPointIndexer

-- | A smart constructor for 'LastPointIndexer'
lastPointIndexer :: (HasGenesis (Point event)) => LastPointIndexer event
lastPointIndexer = LastPointIndexer genesis genesis

instance
  (HasGenesis (Point event), Monad m)
  => IsIndex m event LastPointIndexer
  where
  index timedEvent = pure . (latestSyncPoint .~ timedEvent ^. point)

  indexAllDescending evts = case toList evts of
    [] -> pure
    (evt : _) -> pure . (latestSyncPoint .~ evt ^. point)

  rollback p = pure . (latestSyncPoint .~ p)
  setLastStablePoint p = pure . (latestStablePoint .~ p)

instance (Applicative m) => IsSync m event LastPointIndexer where
  lastSyncPoint = pure . view latestSyncPoint
  lastStablePoint = pure . view latestStablePoint
