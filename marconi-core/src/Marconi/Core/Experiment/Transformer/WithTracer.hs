{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
    A transformer that add tracing capability to an indexer.

    See "Marconi.Core.Experiment" for documentation.
 -}
module Marconi.Core.Experiment.Transformer.WithTracer
    ( WithTracer
        , withTracer
    , HasTracerConfig (tracer)
    ) where

import Control.Lens (Lens', makeLenses)
import Control.Lens.Operators ((^.))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Tracer (Tracer)
import Control.Tracer qualified as Tracer

import Marconi.Core.Experiment.Class (Closeable, HasGenesis, IsIndex (index), IsSync, Queryable, Resetable (reset),
                                      Rollbackable (rollback))
import Marconi.Core.Experiment.Transformer.Class (IndexerMapTrans (unwrapMap))
import Marconi.Core.Experiment.Transformer.IndexWrapper (IndexWrapper (IndexWrapper),
                                                         IndexerTrans (Config, unwrap, wrap), indexVia, resetVia,
                                                         rollbackVia, wrapperConfig)
import Marconi.Core.Experiment.Type (Point)
import Marconi.Core.Experiment.Worker (ProcessedInput (Index, Rollback))

newtype ProcessedInputTracer m event
    = ProcessedInputTracer { _unwrapTracer :: Tracer m (ProcessedInput event)}

makeLenses 'ProcessedInputTracer

-- | A tracer modifier that adds tracing to an existing indexer
newtype WithTracer m indexer event
    = WithTracer { _tracerWrapper :: IndexWrapper (ProcessedInputTracer m) indexer event }

withTracer :: Tracer m (ProcessedInput event) -> indexer event -> WithTracer m indexer event
withTracer tr = WithTracer . IndexWrapper (ProcessedInputTracer tr)

makeLenses 'WithTracer


deriving via (IndexWrapper (ProcessedInputTracer m) indexer)
    instance IsSync m event indexer
        => IsSync m event (WithTracer m indexer)

deriving via (IndexWrapper (ProcessedInputTracer m) indexer)
    instance (MonadTrans t, IsSync (t m) event indexer)
        => IsSync (t m) event (WithTracer m indexer)

deriving via (IndexWrapper (ProcessedInputTracer m) indexer)
    instance Closeable m indexer
        => Closeable m (WithTracer m indexer)

deriving via (IndexWrapper (ProcessedInputTracer m) indexer)
    instance (MonadTrans t, Closeable (t m) indexer)
        => Closeable (t m) (WithTracer m indexer)

deriving via (IndexWrapper (ProcessedInputTracer m) indexer)
    instance Queryable m event query indexer
        => Queryable m event query (WithTracer m indexer)

deriving via (IndexWrapper (ProcessedInputTracer m) indexer)
    instance (MonadTrans t, Queryable (t m) event query indexer)
        => Queryable (t m) event query (WithTracer m indexer)


instance IndexerTrans (WithTracer m) where

    type instance Config (WithTracer m) = ProcessedInputTracer m

    wrap cfg = WithTracer . IndexWrapper cfg

    unwrap = tracerWrapper . unwrap

class HasTracerConfig m event indexer where

    tracer :: Lens' (indexer event) (Tracer m (ProcessedInput event))

instance {-# OVERLAPPING #-}
    HasTracerConfig m event (WithTracer m indexer) where

    tracer = tracerWrapper . wrapperConfig . unwrapTracer

instance {-# OVERLAPPABLE #-}
    (IndexerTrans t, HasTracerConfig m event indexer)
    => HasTracerConfig m event (t indexer) where

    tracer = unwrap . tracer

instance {-# OVERLAPPABLE #-}
    (IndexerMapTrans t, HasTracerConfig m output indexer)
    => HasTracerConfig m output (t indexer output) where

    tracer = unwrapMap . tracer

instance
    (Applicative m, IsIndex m event index)
    => IsIndex m event (WithTracer m index) where

    index timedEvent indexer = do
        res <- indexVia unwrap timedEvent indexer
        Tracer.traceWith (indexer ^. tracer) $ Index timedEvent
        pure res

instance (MonadTrans t, Monad m, Monad (t m),  IsIndex (t m) event index)
    => IsIndex (t m) event (WithTracer m index) where

    index timedEvent indexer = do
        res <- indexVia unwrap timedEvent indexer
        lift $ Tracer.traceWith (indexer ^. tracer) $ Index timedEvent
        pure res


instance
    ( Monad m
    , Rollbackable m event index
    ) => Rollbackable m event (WithTracer m index) where

    rollback p indexer = let

         rollbackWrappedIndexer p'
              = rollbackVia unwrap p' indexer

         traceRollback
              = Tracer.traceWith (indexer ^. tracer) (Rollback p)

        in do
        -- Warn about the rollback first
        traceRollback
        rollbackWrappedIndexer p

instance
    ( MonadTrans t
    , Monad m
    , Monad (t m)
    , Rollbackable (t m) event index
    ) => Rollbackable (t m) event (WithTracer m index) where

    rollback p indexer = let

         rollbackWrappedIndexer p'
              = rollbackVia unwrap p' indexer

         traceRollback
              = lift $ Tracer.traceWith (indexer ^. tracer) (Rollback p)

        in do
        -- Warn about the rollback first
        traceRollback
        rollbackWrappedIndexer p

instance
    ( HasGenesis (Point event)
    , Functor m
    , Resetable m event indexer
    ) => Resetable m event (WithTracer m indexer) where

    reset = resetVia unwrap

instance
    ( MonadTrans t
    , Monad m
    , Monad (t m)
    , HasGenesis (Point event)
    , Resetable (t m) event indexer
    ) => Resetable (t m) event (WithTracer m indexer) where

    reset = resetVia unwrap

