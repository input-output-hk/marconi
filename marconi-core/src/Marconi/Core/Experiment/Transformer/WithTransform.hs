{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    A transformer that cache result of some queries

    See "Marconi.Core.Experiment" for documentation.
 -}
module Marconi.Core.Experiment.Transformer.WithTransform
    ( WithTransform
        , config
        , transformedIndexer
    ) where
import Control.Lens (makeLenses)
import Control.Lens.Operators ((^.))
import Marconi.Core.Experiment.Class (Closeable (close), HasGenesis, IsIndex (index, indexAll), IsSync (lastSyncPoint),
                                      Resetable (reset), Rollbackable (rollback))
import Marconi.Core.Experiment.Transformer.Class (IndexerMapTrans (ConfigMap, unwrapMap, wrapMap))
import Marconi.Core.Experiment.Transformer.IndexWrapper (closeVia, indexAllVia, indexVia, lastSyncPointVia, resetVia,
                                                         rollbackVia)
import Marconi.Core.Experiment.Type (Point, TimedEvent (TimedEvent), event, point)

data TransformConfig output input
    = AggregationConfig
        { _transformEvent :: input -> output
        , _transformPoint :: Point input -> Point output
        }

makeLenses ''TransformConfig

data WithTransform indexer output input
    = WithTransform
        { _config             :: TransformConfig output input
        , _transformedIndexer :: indexer output
        }

makeLenses ''WithTransform

instance IndexerMapTrans WithTransform where

    type instance ConfigMap WithTransform = TransformConfig

    wrapMap = WithTransform

    unwrapMap = transformedIndexer

instance
    (Monad m, IsIndex m output indexer, Ord (Point output))
    => IsIndex m input (WithTransform indexer output) where

   index timedEvent indexer = do
       let point' = indexer ^. config . transformPoint $ timedEvent ^. point
           event' = indexer ^. config . transformEvent $ timedEvent ^. event
           asOutput = TimedEvent point' event'
       indexVia unwrapMap asOutput indexer

   indexAll events indexer = do
       let point' = indexer ^. config . transformPoint
           event' = indexer ^. config . transformEvent
           toOutput te = TimedEvent (point' $ te ^. point) (event' $ te ^. event)
           asOutputs = toOutput <$> events
       indexAllVia unwrapMap asOutputs indexer

instance (Point output ~ Point event, IsSync m output indexer)
    => IsSync m event (WithTransform indexer output) where

    lastSyncPoint = lastSyncPointVia unwrapMap

instance
    ( Functor m
    , Point output ~ Point event
    , Rollbackable m output indexer)
    => Rollbackable m event (WithTransform indexer output) where

    rollback = rollbackVia unwrapMap

instance
    ( Functor m
    , Resetable m output indexer
    , HasGenesis (Point event)
    ) => Resetable m event (WithTransform indexer output) where

    reset = resetVia unwrapMap

instance Closeable m indexer
    => Closeable m (WithTransform indexer output) where

    close = closeVia unwrapMap
