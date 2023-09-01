{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    A transformer that allows an indexer to fold incoming events:
    each new event is the result of a computation between the last result in the indexer
    and the incoming event.


    See "Marconi.Core.Experiment" for documentation.
-}
module Marconi.Core.Experiment.Transformer.WithFold (
  WithFold,
  withFold,
  withFoldMap,
  HasFoldConfig (fold),
) where

import Control.Lens (Getter, Lens', makeLenses, (.~))
import Control.Lens.Operators ((?~), (^.))
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Data.Foldable (Foldable (toList))
import Data.Function ((&))
import Data.List (scanl', sortOn)
import Data.Maybe (fromMaybe)
import Marconi.Core.Experiment.Class (
  Closeable (close),
  HasGenesis (genesis),
  IsIndex (index, indexAllDescending, rollback),
  IsSync (lastSyncPoint, lastSyncPoints),
  Queryable (query),
  Resetable (reset),
  queryEither,
 )
import Marconi.Core.Experiment.Query (EventAtQuery (EventAtQuery))
import Marconi.Core.Experiment.Transformer.Class (IndexerMapTrans (ConfigMap, unwrapMap, wrapMap))
import Marconi.Core.Experiment.Transformer.IndexTransformer (
  closeVia,
  indexAllDescendingVia,
  indexVia,
  lastSyncPointVia,
  lastSyncPointsVia,
  queryVia,
  resetVia,
  rollbackVia,
 )
import Marconi.Core.Experiment.Type (
  IndexerError (IndexerInternalError),
  Point,
  QueryError,
  Timed (Timed),
  event,
  point,
 )

data FoldConfig output input = FoldConfig
  { _initialConfig :: output
  , _foldConfig :: output -> input -> output
  }

makeLenses ''FoldConfig

-- | 'WithFold' fold incoming @event@ to produce an @output@.
data WithFold indexer output input = WithFold
  { _config :: FoldConfig output input
  , _foldedIndexer :: indexer output
  }

makeLenses ''WithFold

-- | A smart constructor for 'WitFold'
withFold
  :: output
  -> (output -> input -> output)
  -> indexer output
  -> WithFold indexer output input
withFold init' fold' _foldedIndexer =
  WithFold
    { _config = FoldConfig init' fold'
    , _foldedIndexer
    }

-- | A smart constructor for 'WitFold'
withFoldMap
  :: (Monoid output)
  => (input -> output)
  -> indexer output
  -> WithFold indexer output input
withFoldMap f _foldedIndexer =
  WithFold
    { _config = FoldConfig mempty (\acc x -> acc <> f x)
    , _foldedIndexer
    }

-- | There are few scenarios where you want to modify the fold function but it may happen
class HasFoldConfig input output indexer where
  initial :: Getter (indexer input) output
  fold :: Lens' (indexer input) (output -> input -> output)

instance HasFoldConfig input output (WithFold indexer output) where
  initial = config . initialConfig
  fold = config . foldConfig

instance IndexerMapTrans WithFold where
  type ConfigMap WithFold = FoldConfig
  wrapMap = WithFold
  unwrapMap = foldedIndexer

{- | Get the previous value stored by the indexer, or the initial value if the previous value
doesn't exist
-}
getPreviousValue
  :: ( MonadError IndexerError m
     , HasGenesis (Point output)
     , Point input ~ Point output
     , IsSync m output indexer
     , Queryable (ExceptT (QueryError (EventAtQuery output)) m) output (EventAtQuery output) indexer
     , Ord (Point output)
     )
  => WithFold indexer output input
  -> m (Timed (Point output) (Maybe output))
getPreviousValue indexer = do
  lSync <- lastSyncPoint indexer
  evt <-
    if lSync == genesis
      then pure Nothing
      else do
        lastAggregateOrError <- queryEither lSync EventAtQuery indexer
        case lastAggregateOrError of
          Left _ -> throwError $ IndexerInternalError "can't find last aggregate"
          Right previous -> pure previous
  pure $ Timed lSync evt

instance
  (Point output ~ Point event, IsSync m output indexer)
  => IsSync m event (WithFold indexer output)
  where
  lastSyncPoint = lastSyncPointVia unwrapMap
  lastSyncPoints = lastSyncPointsVia unwrapMap

instance
  ( Queryable m output query indexer
  , Point input ~ Point output
  )
  => Queryable m input query (WithFold indexer output)
  where
  query = queryVia unwrapMap

instance
  ( Functor m
  , Resetable m output indexer
  , HasGenesis (Point event)
  )
  => Resetable m event (WithFold indexer output)
  where
  reset = resetVia unwrapMap

instance (Closeable m indexer) => Closeable m (WithFold indexer output) where
  close = closeVia unwrapMap

instance
  ( MonadError IndexerError m
  , HasGenesis (Point output)
  , Point input ~ Point output
  , IsSync m output indexer
  , IsIndex m output indexer
  , Queryable (ExceptT (QueryError (EventAtQuery output)) m) output (EventAtQuery output) indexer
  , Ord (Point output)
  )
  => IsIndex m input (WithFold indexer output)
  where
  index timedEvent indexer = do
    Timed _ previous <- getPreviousValue indexer
    let foldEvent Nothing = previous
        foldEvent (Just evt) = Just $ (indexer ^. fold) (fromMaybe (indexer ^. initial) previous) evt
    indexVia unwrapMap (foldEvent <$> timedEvent) indexer

  indexAllDescending events indexer = case sortOn (^. point) $ toList events of
    [] -> pure indexer
    xs -> do
      previousTimedEvent <- getPreviousValue indexer
      let foldEvent p te = case (p ^. event, te ^. event) of
            (previous, Nothing) -> te & event .~ previous
            (previous, Just evt) ->
              let previous' = fromMaybe (indexer ^. initial) previous
               in te & event ?~ (indexer ^. fold) previous' evt
          asOutputs = scanl' foldEvent
      indexAllDescendingVia unwrapMap (asOutputs previousTimedEvent xs) indexer

  rollback = rollbackVia unwrapMap
