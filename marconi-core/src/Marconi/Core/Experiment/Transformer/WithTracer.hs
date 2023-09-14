{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
    A transformer that add tracing capability to an indexer.

    See "Marconi.Core.Experiment" for documentation.
-}
module Marconi.Core.Experiment.Transformer.WithTracer (
  -- * Event
  IndexerEvent (..),

  -- * Trace
  WithTrace,
  withTrace,
  withTraceM,
  HasTraceConfig (trace),

  -- * Tracer
  WithTracer,
  withTracer,
  withTracerM,
  HasTracerConfig (tracer),
) where

import Cardano.BM.Trace qualified as Trace
import Cardano.BM.Tracing (Trace, Tracer)
import Cardano.BM.Tracing qualified as Tracing
import Control.Lens (Lens', makeLenses, view)
import Control.Lens.Operators ((^.))
import Control.Monad.Trans.Class (MonadTrans (lift))

import Control.Monad.Cont (MonadIO)
import Data.Foldable (Foldable (toList))
import Data.Maybe (listToMaybe)
import Marconi.Core.Experiment.Class (
  Closeable (close),
  HasGenesis,
  IsIndex (index, indexAll, indexAllDescending, rollback),
  IsSync (lastSyncPoint, lastSyncPoints),
  Queryable (query),
  Resetable (reset),
 )
import Marconi.Core.Experiment.Indexer.SQLiteAggregateQuery (HasDatabasePath)
import Marconi.Core.Experiment.Transformer.Class (
  IndexerMapTrans (unwrapMap),
  IndexerTrans (unwrap),
 )
import Marconi.Core.Experiment.Transformer.IndexTransformer (
  IndexTransformer (IndexTransformer),
  closeVia,
  indexAllDescendingVia,
  indexAllVia,
  indexVia,
  lastSyncPointVia,
  lastSyncPointsVia,
  queryVia,
  resetVia,
  rollbackVia,
  wrapperConfig,
 )
import Marconi.Core.Experiment.Type (Point, point)

-- | Event available for the tracer
data IndexerEvent point
  = IndexerIsStarting
  | IndexerStarted
  | IndexerIndexes point
  | IndexerHasIndexed
  | IndexerRollbackTo point
  | IndexerHasRollbackedTo
  | IndexerIsClosing
  | IndexerClosed

deriving stock instance (Show point) => Show (IndexerEvent point)

newtype IndexerTracer m event = IndexerTracer {_unwrapTracer :: Tracer m (IndexerEvent (Point event))}

makeLenses 'IndexerTracer

-- | A tracer modifier that adds tracing to an existing indexer
newtype WithTracer m indexer event = WithTracer {_tracerWrapper :: IndexTransformer (IndexerTracer m) indexer event}

-- | A smart constructor for @WithTracer@
withTracer
  :: (Applicative m)
  => Tracer m (IndexerEvent (Point event))
  -> indexer event
  -> WithTracer m indexer event
withTracer tr = WithTracer . IndexTransformer (IndexerTracer tr)

-- | A monadic smart constructor for @WithTracer@
withTracerM
  :: (Applicative m)
  => Tracer m (IndexerEvent (Point event))
  -> m (indexer event)
  -> m (WithTracer m indexer event)
withTracerM tr indexer = do
  Tracing.traceWith tr IndexerIsStarting
  result <- WithTracer . IndexTransformer (IndexerTracer tr) <$> indexer
  Tracing.traceWith tr IndexerStarted
  pure result

makeLenses 'WithTracer

instance (IsSync m event indexer) => IsSync m event (WithTracer n indexer) where
  lastSyncPoint = lastSyncPointVia unwrap
  lastSyncPoints = lastSyncPointsVia unwrap

deriving via
  (IndexTransformer (IndexerTracer m) indexer)
  instance
    (HasDatabasePath indexer) => HasDatabasePath (WithTracer m indexer)

instance (Applicative m, Closeable m indexer) => Closeable m (WithTracer m indexer) where
  close indexer = do
    let trace = Tracing.traceWith (indexer ^. tracer)
    trace IndexerIsClosing
    res <- closeVia unwrap indexer
    trace IndexerClosed
    pure res

instance
  (MonadTrans t, Applicative (t m), Monad m, Closeable (t m) indexer)
  => Closeable (t m) (WithTracer m indexer)
  where
  close indexer = do
    let trace = lift . Tracing.traceWith (indexer ^. tracer)
    trace IndexerIsClosing
    res <- closeVia unwrap indexer
    trace IndexerClosed
    pure res

instance (Queryable m event query indexer) => Queryable m event query (WithTracer n indexer) where
  query = queryVia unwrap

instance IndexerTrans (WithTracer m) where
  unwrap = tracerWrapper . unwrap

{- | It gives access to the tracer. The provided instances allows access to the tracer event below
an indexer transformer.
-}
class HasTracerConfig m event indexer where
  tracer :: Lens' (indexer event) (Tracer m (IndexerEvent (Point event)))

instance {-# OVERLAPPING #-} HasTracerConfig m event (WithTracer m indexer) where
  tracer = tracerWrapper . wrapperConfig . unwrapTracer

instance
  {-# OVERLAPPABLE #-}
  (IndexerTrans t, HasTracerConfig m event indexer)
  => HasTracerConfig m event (t indexer)
  where
  tracer = unwrap . tracer

instance
  {-# OVERLAPPABLE #-}
  (IndexerMapTrans t, HasTracerConfig m output indexer)
  => HasTracerConfig m output (t indexer output)
  where
  tracer = unwrapMap . tracer

instance
  (Applicative m, IsIndex m event index)
  => IsIndex m event (WithTracer m index)
  where
  index timedEvent indexer = do
    let trace = Tracing.traceWith (indexer ^. tracer)
        point' = timedEvent ^. point
    trace $ IndexerIndexes point'
    res <- indexVia unwrap timedEvent indexer
    trace IndexerHasIndexed
    pure res

  indexAll timedEvents indexer = do
    let trace = Tracing.traceWith (indexer ^. tracer)
        events = toList timedEvents
        firstPoint = fmap (view point) . listToMaybe $ events
        lastPoint = fmap (view point) . listToMaybe $ reverse events
    maybe (pure ()) (trace . IndexerIndexes) firstPoint
    res <- indexAllVia unwrap timedEvents indexer
    maybe (pure ()) (const $ trace IndexerHasIndexed) lastPoint
    pure res

  indexAllDescending timedEvents indexer = do
    let trace = Tracing.traceWith (indexer ^. tracer)
        events = toList timedEvents
        firstPoint = fmap (view point) . listToMaybe $ reverse events
        lastPoint = fmap (view point) . listToMaybe $ events
    maybe (pure ()) (trace . IndexerIndexes) firstPoint
    res <- indexAllDescendingVia unwrap timedEvents indexer
    maybe (pure ()) (const $ trace IndexerHasIndexed) lastPoint
    pure res

  rollback p indexer =
    let rollbackWrappedIndexer p' = rollbackVia unwrap p' indexer
        trace = Tracing.traceWith (indexer ^. tracer)
     in do
          -- Warn about the rollback first
          trace $ IndexerRollbackTo p
          res <- rollbackWrappedIndexer p
          trace IndexerHasRollbackedTo
          pure res

instance
  (MonadTrans t, Monad m, Monad (t m), IsIndex (t m) event index)
  => IsIndex (t m) event (WithTracer m index)
  where
  index timedEvent indexer = do
    let trace = lift . Tracing.traceWith (indexer ^. tracer)
        point' = timedEvent ^. point
    trace $ IndexerIndexes point'
    res <- indexVia unwrap timedEvent indexer
    trace IndexerHasIndexed
    pure res

  indexAll timedEvents indexer = do
    let trace = lift . Tracing.traceWith (indexer ^. tracer)
        events = toList timedEvents
        firstPoint = fmap (view point) . listToMaybe $ events
        lastPoint = fmap (view point) . listToMaybe $ reverse events
    maybe (pure ()) (trace . IndexerIndexes) firstPoint
    res <- indexAllVia unwrap timedEvents indexer
    maybe (pure ()) (const $ trace IndexerHasIndexed) lastPoint
    pure res

  indexAllDescending timedEvents indexer = do
    let trace = lift . Tracing.traceWith (indexer ^. tracer)
        events = toList timedEvents
        firstPoint = fmap (view point) . listToMaybe $ reverse events
        lastPoint = fmap (view point) . listToMaybe $ events
    maybe (pure ()) (trace . IndexerIndexes) firstPoint
    res <- indexAllDescendingVia unwrap timedEvents indexer
    maybe (pure ()) (const $ trace IndexerHasIndexed) lastPoint
    pure res

  rollback p indexer =
    let rollbackWrappedIndexer p' = rollbackVia unwrap p' indexer
        trace = lift . Tracing.traceWith (indexer ^. tracer)
     in do
          -- Warn about the rollback first
          trace $ IndexerRollbackTo p
          res <- rollbackWrappedIndexer p
          trace IndexerHasRollbackedTo
          pure res

instance
  ( HasGenesis (Point event)
  , Functor m
  , Resetable m event indexer
  )
  => Resetable m event (WithTracer m indexer)
  where
  reset = resetVia unwrap

instance
  ( MonadTrans t
  , Monad m
  , Monad (t m)
  , HasGenesis (Point event)
  , Resetable (t m) event indexer
  )
  => Resetable (t m) event (WithTracer m indexer)
  where
  reset = resetVia unwrap

newtype IndexerTrace m event = IndexerTrace {_unwrapTrace :: Trace m (IndexerEvent (Point event))}

makeLenses 'IndexerTrace

-- | A tracer modifier that adds tracing to an existing indexer
newtype WithTrace m indexer event = WithTrace {_traceWrapper :: IndexTransformer (IndexerTrace m) indexer event}

-- | A smart constructor for @WithTrace@
withTrace
  :: (Applicative m)
  => Trace m (IndexerEvent (Point event))
  -> indexer event
  -> WithTrace m indexer event
withTrace tr = WithTrace . IndexTransformer (IndexerTrace tr)

-- | A monadic smart constructor for @WithTrace@
withTraceM
  :: (MonadIO m)
  => Trace m (IndexerEvent (Point event))
  -> m (indexer event)
  -> m (WithTrace m indexer event)
withTraceM tr indexer = do
  Trace.logDebug tr IndexerIsStarting
  result <- WithTrace . IndexTransformer (IndexerTrace tr) <$> indexer
  Trace.logInfo tr IndexerStarted
  pure result

makeLenses 'WithTrace

instance (IsSync m event indexer) => IsSync m event (WithTrace n indexer) where
  lastSyncPoint = lastSyncPointVia unwrap
  lastSyncPoints = lastSyncPointsVia unwrap

deriving via
  (IndexTransformer (IndexerTrace m) indexer)
  instance
    (HasDatabasePath indexer) => HasDatabasePath (WithTrace m indexer)

instance (MonadIO m, Closeable m indexer) => Closeable m (WithTrace m indexer) where
  close indexer = do
    let tr = indexer ^. trace
    Trace.logDebug tr IndexerIsClosing
    res <- closeVia unwrap indexer
    Trace.logInfo tr IndexerClosed
    pure res

instance
  (MonadTrans t, MonadIO (t m), MonadIO m, Closeable (t m) indexer)
  => Closeable (t m) (WithTrace m indexer)
  where
  close indexer = do
    let tr = indexer ^. trace
    lift $ Trace.logDebug tr IndexerIsClosing
    res <- closeVia unwrap indexer
    lift $ Trace.logInfo tr IndexerClosed
    pure res

instance (Queryable m event query indexer) => Queryable m event query (WithTrace n indexer) where
  query = queryVia unwrap

instance IndexerTrans (WithTrace m) where
  unwrap = traceWrapper . unwrap

{- | It gives access to the tracer. The provided instances allows access to the tracer event below
an indexer transformer.
-}
class HasTraceConfig m event indexer where
  trace :: Lens' (indexer event) (Trace m (IndexerEvent (Point event)))

instance {-# OVERLAPPING #-} HasTraceConfig m event (WithTrace m indexer) where
  trace = traceWrapper . wrapperConfig . unwrapTrace

instance
  {-# OVERLAPPABLE #-}
  (IndexerTrans t, HasTraceConfig m event indexer)
  => HasTraceConfig m event (t indexer)
  where
  trace = unwrap . trace

instance
  {-# OVERLAPPABLE #-}
  (IndexerMapTrans t, HasTraceConfig m output indexer)
  => HasTraceConfig m output (t indexer output)
  where
  trace = unwrapMap . trace

instance
  (MonadIO m, IsIndex m event index)
  => IsIndex m event (WithTrace m index)
  where
  index timedEvent indexer = do
    let tr = indexer ^. trace
        point' = timedEvent ^. point
    Trace.logDebug tr $ IndexerIndexes point'
    res <- indexVia unwrap timedEvent indexer
    Trace.logDebug tr IndexerHasIndexed
    pure res

  indexAll timedEvents indexer = do
    let tr = indexer ^. trace
        events = toList timedEvents
        firstPoint = fmap (view point) . listToMaybe $ events
        lastPoint = fmap (view point) . listToMaybe $ reverse events
    maybe (pure ()) (Trace.logDebug tr . IndexerIndexes) firstPoint
    res <- indexAllVia unwrap timedEvents indexer
    maybe (pure ()) (const $ Trace.logDebug tr IndexerHasIndexed) lastPoint
    pure res

  indexAllDescending timedEvents indexer = do
    let tr = indexer ^. trace
        events = toList timedEvents
        firstPoint = fmap (view point) . listToMaybe $ reverse events
        lastPoint = fmap (view point) . listToMaybe $ events
    maybe (pure ()) (Trace.logDebug tr . IndexerIndexes) firstPoint
    res <- indexAllDescendingVia unwrap timedEvents indexer
    maybe (pure ()) (const $ Trace.logDebug tr IndexerHasIndexed) lastPoint
    pure res

  rollback p indexer =
    let rollbackWrappedIndexer p' = rollbackVia unwrap p' indexer
        tr = indexer ^. trace
     in do
          -- Warn about the rollback first
          Trace.logDebug tr $ IndexerRollbackTo p
          res <- rollbackWrappedIndexer p
          Trace.logDebug tr IndexerHasRollbackedTo
          pure res

instance
  (MonadTrans t, MonadIO m, MonadIO (t m), IsIndex (t m) event index)
  => IsIndex (t m) event (WithTrace m index)
  where
  index timedEvent indexer = do
    let tr = indexer ^. trace
        point' = timedEvent ^. point
    lift $ Trace.logDebug tr $ IndexerIndexes point'
    res <- indexVia unwrap timedEvent indexer
    lift $ Trace.logDebug tr IndexerHasIndexed
    pure res

  indexAll timedEvents indexer = do
    let tr = indexer ^. trace
        events = toList timedEvents
        firstPoint = fmap (view point) . listToMaybe $ events
        lastPoint = fmap (view point) . listToMaybe $ reverse events
    maybe (pure ()) (lift . Trace.logDebug tr . IndexerIndexes) firstPoint
    res <- indexAllVia unwrap timedEvents indexer
    maybe (pure ()) (const $ lift $ Trace.logDebug tr IndexerHasIndexed) lastPoint
    pure res

  indexAllDescending timedEvents indexer = do
    let tr = indexer ^. trace
        events = toList timedEvents
        firstPoint = fmap (view point) . listToMaybe $ reverse events
        lastPoint = fmap (view point) . listToMaybe $ events
    maybe (pure ()) (lift . Trace.logDebug tr . IndexerIndexes) firstPoint
    res <- indexAllDescendingVia unwrap timedEvents indexer
    maybe (pure ()) (const $ lift $ Trace.logDebug tr IndexerHasIndexed) lastPoint
    pure res

  rollback p indexer =
    let rollbackWrappedIndexer p' = rollbackVia unwrap p' indexer
        tr = indexer ^. trace
     in do
          -- Warn about the rollback first
          lift . Trace.logDebug tr $ IndexerRollbackTo p
          res <- rollbackWrappedIndexer p
          lift $ Trace.logDebug tr IndexerHasRollbackedTo
          pure res

instance
  ( HasGenesis (Point event)
  , Functor m
  , Resetable m event indexer
  )
  => Resetable m event (WithTrace m indexer)
  where
  reset = resetVia unwrap

instance
  ( MonadTrans t
  , Monad m
  , Monad (t m)
  , HasGenesis (Point event)
  , Resetable (t m) event indexer
  )
  => Resetable (t m) event (WithTrace m indexer)
  where
  reset = resetVia unwrap
