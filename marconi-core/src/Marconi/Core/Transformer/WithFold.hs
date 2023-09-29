{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    A transformer that allows an indexer to fold incoming events:
    each new event is the result of a computation between the last result in the indexer
    and the incoming event.


    See "Marconi.Core" for documentation.
-}
module Marconi.Core.Transformer.WithFold (
  WithFold,
  withFold,
  withFoldPure,
  withFoldMap,
  getLastEventAtQueryValue,
  HasFold (fold),
) where

import Control.Lens (Lens', makeLenses)
import Control.Lens.Operators ((^.))
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Data.Foldable (Foldable (toList))
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Marconi.Core.Class (
  Closeable (close),
  HasGenesis (genesis),
  IsIndex (index, indexAllDescending, rollback, setLastStablePoint),
  IsSync (lastStablePoint, lastSyncPoint),
  Queryable (query),
  Resetable (reset),
  queryEither,
 )
import Marconi.Core.Query (EventAtQuery (EventAtQuery))
import Marconi.Core.Transformer.Class (IndexerMapTrans (unwrapMap))
import Marconi.Core.Transformer.IndexTransformer (
  closeVia,
  indexAllDescendingVia,
  indexVia,
  lastStablePointVia,
  lastSyncPointVia,
  queryVia,
  resetVia,
  rollbackVia,
  setLastStablePointVia,
 )
import Marconi.Core.Type (
  IndexerError (IndexerInternalError),
  Point,
  QueryError,
  Timed (Timed),
  event,
  point,
 )

-- | 'WithFold' fold incoming @event@ to produce an @output@.
data WithFold m indexer output input = WithFold
  { _initial :: output
  , _getLast :: indexer output -> m (Maybe output)
  , _foldEvent :: output -> input -> m output
  , _foldedIndexer :: indexer output
  }

makeLenses ''WithFold

-- Rely on @EventAtQuery@ to retrieve the last value of the fold.
-- If there isn't a previous value, returns 'Nothing'.
getLastEventAtQueryValue
  :: ( IsSync m event indexer
     , HasGenesis (Point event)
     , Queryable (ExceptT (QueryError (EventAtQuery a)) m) event (EventAtQuery a) indexer
     , Ord (Point event)
     , MonadError IndexerError m
     )
  => indexer event
  -> m (Maybe a)
getLastEventAtQueryValue indexer = do
  lSync <- lastSyncPoint indexer
  if lSync == genesis
    then pure Nothing
    else do
      lastAggregateOrError <- queryEither lSync EventAtQuery indexer
      case lastAggregateOrError of
        Left _ -> throwError $ IndexerInternalError "can't retrive previous value"
        Right agg -> pure agg

-- | A smart constructor for 'WithFold'
withFold
  :: output
  -> (indexer output -> m (Maybe output))
  -> (output -> input -> m output)
  -> indexer output
  -> WithFold m indexer output input
withFold = WithFold

-- | A smart constructor for 'WithFold'
withFoldPure
  :: (Applicative m)
  => output
  -> (indexer output -> m (Maybe output))
  -> (output -> input -> output)
  -> indexer output
  -> WithFold m indexer output input
withFoldPure init' lastQuery step = WithFold init' lastQuery (\x y -> pure $ step x y)

-- | A smart constructor for 'WithFold'
withFoldMap
  :: (Monoid output, Applicative m)
  => (indexer output -> m (Maybe output))
  -> (input -> output)
  -> indexer output
  -> WithFold m indexer output input
withFoldMap _getLast f _foldedIndexer =
  WithFold
    { _initial = mempty
    , _getLast
    , _foldEvent = \acc x -> pure $ acc <> f x
    , _foldedIndexer
    }

-- | There are few scenarios where you want to modify the fold function but it may happen
class HasFold m input output indexer where
  fold :: Lens' (indexer input) (output -> input -> m output)

instance HasFold m input output (WithFold m indexer output) where
  fold = foldEvent

instance IndexerMapTrans (WithFold m) where
  unwrapMap = foldedIndexer

instance
  (Point output ~ Point event, IsSync m output indexer)
  => IsSync m event (WithFold m indexer output)
  where
  lastSyncPoint = lastSyncPointVia unwrapMap
  lastStablePoint = lastStablePointVia unwrapMap

instance
  ( Queryable m output query indexer
  , Point input ~ Point output
  )
  => Queryable m input query (WithFold n indexer output)
  where
  query = queryVia unwrapMap

instance
  ( Functor m
  , Resetable m output indexer
  , HasGenesis (Point event)
  )
  => Resetable m event (WithFold m indexer output)
  where
  reset = resetVia unwrapMap

instance (Closeable m indexer) => Closeable m (WithFold m indexer output) where
  close = closeVia unwrapMap

instance
  ( HasGenesis (Point output)
  , Point input ~ Point output
  , IsSync m output indexer
  , IsIndex m output indexer
  , Ord (Point output)
  )
  => IsIndex m input (WithFold m indexer output)
  where
  index timedEvent indexer = do
    previous <- (indexer ^. getLast) (indexer ^. foldedIndexer)
    let foldEvents Nothing = pure previous
        foldEvents (Just evt) = Just <$> (indexer ^. fold) (fromMaybe (indexer ^. initial) previous) evt
    newEvent <- traverse foldEvents timedEvent
    indexVia unwrapMap newEvent indexer

  indexAllDescending events indexer = case sortOn (^. point) $ toList events of
    [] -> pure indexer
    xs -> do
      previous <- (indexer ^. getLast) (indexer ^. foldedIndexer)
      -- TODO check footprint: I didn't put a lot of effort on the scanM function and it may be
      -- unefficient, either memory wise or timewise
      let scanM _ _ [] = pure []
          scanM f acc (y : ys) = do
            y' <- f acc y
            ys' <- scanM f y' ys
            pure $ y' : ys'
          foldEvent' :: Timed p (Maybe output) -> Timed p (Maybe input) -> m (Timed p (Maybe output))
          foldEvent' timedPrevious te = case (timedPrevious ^. event, te ^. event) of
            (previous', Nothing) -> event (pure . const previous') te
            (previous', Just evt) ->
              let previous'' = fromMaybe (indexer ^. initial) previous'
               in event (const $ Just <$> (indexer ^. fold) previous'' evt) te
      evts <- scanM foldEvent' (Timed genesis previous) xs
      indexAllDescendingVia unwrapMap evts indexer

  rollback = rollbackVia unwrapMap
  setLastStablePoint = setLastStablePointVia unwrapMap
