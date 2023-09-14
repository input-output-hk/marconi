{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    A transformer that map the events in an indexer.

    See "Marconi.Core.Experiment" for documentation.
-}
module Marconi.Core.Experiment.Transformer.WithTransform (
  WithTransform,
  config,
  withTransform,
  HasTransformConfig (transformEvent),
) where

import Control.Lens (Lens', makeLenses)
import Control.Lens.Operators ((^.))
import Marconi.Core.Experiment.Class (
  Closeable (close),
  HasGenesis,
  IsIndex (index, indexAllDescending, rollback),
  IsSync (lastSyncPoint, lastSyncPoints),
  Queryable (query),
  Resetable (reset),
 )
import Marconi.Core.Experiment.Indexer.SQLiteAggregateQuery (HasDatabasePath (getDatabasePath))
import Marconi.Core.Experiment.Transformer.Class (IndexerMapTrans (unwrapMap))
import Marconi.Core.Experiment.Transformer.IndexTransformer (
  closeVia,
  getDatabasePathVia,
  indexAllDescendingVia,
  indexVia,
  lastSyncPointVia,
  lastSyncPointsVia,
  queryVia,
  resetVia,
  rollbackVia,
 )
import Marconi.Core.Experiment.Type (Point, Timed (Timed), event, point)

data TransformConfig output input = TransformConfig
  { _transformPointConfig :: Point input -> Point output
  , _transformEventConfig :: input -> Maybe output
  }

makeLenses ''TransformConfig

-- | WithTransform is an indexer transformer that map the event type of an indexer
data WithTransform indexer output input = WithTransform
  { _config :: TransformConfig output input
  , _transformedIndexer :: indexer output
  }

makeLenses ''WithTransform

-- | A smart constructor for 'WithTransform'
withTransform
  :: (Point input -> Point output)
  -> (input -> Maybe output)
  -> indexer output
  -> WithTransform indexer output input
withTransform f g _transformedIndexer =
  WithTransform
    { _config = TransformConfig f g
    , _transformedIndexer
    }

{- | Provide accces to the tranformation function of a 'WithTransform' transformer.
The provided instance ensure that access is granted even if other indexer transformers are used
on top of this one
-}
class HasTransformConfig input output indexer where
  transformEvent :: Lens' (indexer input) (input -> Maybe output)

instance HasTransformConfig input output (WithTransform indexer output) where
  transformEvent = config . transformEventConfig

transformPoint :: Lens' (WithTransform indexer output input) (Point input -> Point output)
transformPoint = config . transformPointConfig

instance IndexerMapTrans WithTransform where
  unwrapMap = transformedIndexer

instance
  (Point output ~ Point input, IsIndex m output indexer, Ord (Point output))
  => IsIndex m input (WithTransform indexer output)
  where
  index timedEvent indexer = do
    let point' = indexer ^. transformPoint $ timedEvent ^. point
        event' = indexer ^. transformEvent =<< timedEvent ^. event
        asOutput = Timed point' event'
    indexVia unwrapMap asOutput indexer

  indexAllDescending events indexer = do
    let toOutput te =
          Timed
            (indexer ^. transformPoint $ te ^. point)
            (indexer ^. transformEvent =<< te ^. event)
        asOutputs = toOutput <$> events
    indexAllDescendingVia unwrapMap asOutputs indexer

  rollback = rollbackVia unwrapMap

instance
  (Point output ~ Point event, IsSync m output indexer)
  => IsSync m event (WithTransform indexer output)
  where
  lastSyncPoint = lastSyncPointVia unwrapMap
  lastSyncPoints = lastSyncPointsVia unwrapMap

instance (HasDatabasePath indexer) => HasDatabasePath (WithTransform indexer output) where
  getDatabasePath = getDatabasePathVia unwrapMap

instance
  ( Functor m
  , Resetable m output indexer
  , HasGenesis (Point event)
  )
  => Resetable m event (WithTransform indexer output)
  where
  reset = resetVia unwrapMap

instance
  (Closeable m indexer)
  => Closeable m (WithTransform indexer output)
  where
  close = closeVia unwrapMap

instance
  (Queryable m output query indexer, Point output ~ Point input)
  => Queryable m input query (WithTransform indexer output)
  where
  query = queryVia unwrapMap
