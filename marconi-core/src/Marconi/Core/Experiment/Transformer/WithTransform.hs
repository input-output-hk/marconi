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
  IsIndex (index, indexAllDescending),
  IsSync (lastSyncPoint),
  Queryable (query),
  Resetable (reset),
  Rollbackable (rollback),
 )
import Marconi.Core.Experiment.Transformer.Class (IndexerMapTrans (ConfigMap, unwrapMap, wrapMap))
import Marconi.Core.Experiment.Transformer.IndexWrapper (
  closeVia,
  indexAllDescendingVia,
  indexVia,
  lastSyncPointVia,
  queryVia,
  resetVia,
  rollbackVia,
 )
import Marconi.Core.Experiment.Type (Point, Timed (Timed), event, point)

newtype TransformConfig output input = TransformConfig
  { _transformEventConfig :: input -> output
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
  :: (input -> output)
  -> indexer output
  -> WithTransform indexer output input
withTransform f _transformedIndexer =
  WithTransform
    { _config = TransformConfig f
    , _transformedIndexer
    }

class HasTransformConfig input output indexer where
  transformEvent :: Lens' (indexer input) (input -> output)

instance HasTransformConfig input output (WithTransform indexer output) where
  transformEvent = config . transformEventConfig

instance IndexerMapTrans WithTransform where
  type ConfigMap WithTransform = TransformConfig

  wrapMap = WithTransform

  unwrapMap = transformedIndexer

instance
  (Point input ~ Point output, IsIndex m output indexer, Ord (Point output))
  => IsIndex m input (WithTransform indexer output)
  where
  index timedEvent indexer = do
    let point' = timedEvent ^. point
        event' = indexer ^. transformEvent $ timedEvent ^. event
        asOutput = Timed point' event'
    indexVia unwrapMap asOutput indexer

  indexAllDescending events indexer = do
    let event' = indexer ^. transformEvent
        toOutput te = Timed (te ^. point) (event' $ te ^. event)
        asOutputs = toOutput <$> events
    indexAllDescendingVia unwrapMap asOutputs indexer

instance
  (Point output ~ Point event, IsSync m output indexer)
  => IsSync m event (WithTransform indexer output)
  where
  lastSyncPoint = lastSyncPointVia unwrapMap

instance
  ( Functor m
  , Point output ~ Point event
  , Rollbackable m output indexer
  )
  => Rollbackable m event (WithTransform indexer output)
  where
  rollback = rollbackVia unwrapMap

instance
  ( Functor m
  , Resetable m output indexer
  , HasGenesis (Point event)
  )
  => Resetable m event (WithTransform indexer output)
  where
  reset = resetVia unwrapMap

instance
  Closeable m indexer
  => Closeable m (WithTransform indexer output)
  where
  close = closeVia unwrapMap

instance
  ( Queryable m output query indexer
  , Point input ~ Point output
  )
  => Queryable m input query (WithTransform indexer output)
  where
  query p q indexer = queryVia unwrapMap p q indexer
