{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    A transformer that allows an indexer to aggregate events:
    each new event is the result of a computation between the last result in the indexer
    and the incoming event.


    See "Marconi.Core.Experiment" for documentation.
 -}
module Marconi.Core.Experiment.Transformer.WithAggregate
    ( WithAggregate
        , config
    , withAggregate
    , HasAggregateConfig (toAggregate)
    ) where
import Control.Lens (Lens', makeLenses)
import Control.Lens.Operators ((^.))
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Data.Foldable (Foldable (toList))
import Data.List (scanl', sortOn)
import Marconi.Core.Experiment.Class (Closeable (close), HasGenesis (genesis), IsIndex (index, indexAll),
                                      IsSync (lastSyncPoint), Queryable (query), Resetable (reset),
                                      Rollbackable (rollback), query')
import Marconi.Core.Experiment.Query (EventAtQuery (EventAtQuery))
import Marconi.Core.Experiment.Transformer.Class (IndexerMapTrans (ConfigMap, unwrapMap, wrapMap))
import Marconi.Core.Experiment.Transformer.IndexWrapper (closeVia, indexAllVia, indexVia, lastSyncPointVia, queryVia,
                                                         resetVia, rollbackVia)
import Marconi.Core.Experiment.Type (IndexerError (IndexerInternalError), Point, QueryError, TimedEvent (TimedEvent),
                                     event, point)

newtype AggregateConfig output input
    = AggregationConfig
        { _toAggregateConfig :: input -> output
        }

makeLenses ''AggregateConfig

-- | 'WithAggregate' transform data to a semigroupt instance and aggregate @input@ event
-- to produce an @output@.
-- It can be sued if you don't care about when an event happen, but you do care about
-- the result it produces.
data WithAggregate indexer output input
    = WithAggregate
        { _config            :: AggregateConfig output input
        , _aggregatedIndexer :: indexer output
        }

makeLenses ''WithAggregate

-- | A smart constructor for 'WithAggregate'
withAggregate
    :: (input -> output)
    -> indexer output
    -> WithAggregate indexer output input
withAggregate f _aggregatedIndexer
    = WithAggregate
        { _config = AggregationConfig f
        , _aggregatedIndexer
        }

-- | There are few scenarios where you want to modify the aggregation function but it may happen
class HasAggregateConfig input output indexer where

    toAggregate :: Lens' (indexer input) (input -> output)

instance HasAggregateConfig input output (WithAggregate indexer output) where

    toAggregate = config . toAggregateConfig

instance IndexerMapTrans WithAggregate where

    type instance ConfigMap WithAggregate = AggregateConfig

    wrapMap = WithAggregate

    unwrapMap = aggregatedIndexer

instance
    ( MonadError IndexerError m
    , Semigroup output
    , HasGenesis (Point output)
    , Point input ~ Point output
    , IsSync m output indexer
    , IsIndex m output indexer
    , Queryable (ExceptT (QueryError (EventAtQuery output)) m) output (EventAtQuery output) indexer
    , Ord (Point output)
    )
    => IsIndex m input (WithAggregate indexer output) where

   index timedEvent indexer = do
       let asAggregate = indexer ^. toAggregate $ timedEvent ^. event
       let point' = timedEvent ^. point
       lSync <- lastSyncPoint indexer
       event' <- if lSync == genesis
               then pure asAggregate
               else do
                   lastAggregateOrError <- query' lSync EventAtQuery indexer
                   case lastAggregateOrError of
                       Left _    -> throwError $ IndexerInternalError "can't find last aggregate"
                       Right agg -> pure $ agg <> asAggregate
       let asOutput = TimedEvent point' event'
       indexVia unwrapMap asOutput indexer

   indexAll events indexer = case sortOn (^. point) $ toList events of
       [] -> pure indexer
       (x:xs) -> do
           lSync <- lastSyncPoint indexer
           let event' = indexer ^. toAggregate
               asAggregate :: output = indexer ^. toAggregate $ x ^. event
           firstEvent <- if lSync == genesis
               then pure asAggregate
               else do
                   lastAggregateOrError <- query' lSync EventAtQuery indexer
                   case lastAggregateOrError of
                       Left _    -> throwError $ IndexerInternalError "can't find last aggregate"
                       Right agg -> pure $ agg <> asAggregate
           let firstTimedEvent = TimedEvent (x ^. point) firstEvent
           let toOutput' tacc te = TimedEvent (te ^. point) ((tacc ^. event) <> (event' $ te ^. event))
               asOutputs tacc es = scanl' toOutput' tacc es
           indexAllVia unwrapMap (asOutputs firstTimedEvent xs) indexer

instance (Point output ~ Point event, IsSync m output indexer)
    => IsSync m event (WithAggregate indexer output) where

    lastSyncPoint = lastSyncPointVia unwrapMap

instance
    ( Functor m
    , Point output ~ Point event
    , Rollbackable m output indexer)
    => Rollbackable m event (WithAggregate indexer output) where

    rollback = rollbackVia unwrapMap

instance
    ( Functor m
    , Resetable m output indexer
    , HasGenesis (Point event)
    ) => Resetable m event (WithAggregate indexer output) where

    reset = resetVia unwrapMap

instance Closeable m indexer
    => Closeable m (WithAggregate indexer output) where

    close = closeVia unwrapMap

instance
    ( Queryable m output query indexer
    , Point input ~ Point output
    ) => Queryable m input query (WithAggregate indexer output) where

    query p q indexer = queryVia unwrapMap p q indexer
