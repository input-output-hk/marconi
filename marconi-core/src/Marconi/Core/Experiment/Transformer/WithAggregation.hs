{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
    A transformer that cache result of some queries

    See "Marconi.Core.Experiment" for documentation.
 -}
module Marconi.Core.Experiment.Transformer.WithAggregation
    ( WithAggregation
    ) where
import Control.Lens (makeLenses)
import Control.Lens.Operators ((^.))
import Marconi.Core.Experiment.Class (IsIndex (index))
import Marconi.Core.Experiment.Transformer.IndexWrapper (indexVia)
import Marconi.Core.Experiment.Type (Point, TimedEvent (TimedEvent), event, point)

data AggregationConfig output input
    = AggregationConfig
        { _transformEvent :: input -> output
        , _transformPoint :: Point input -> Point output
        }

makeLenses ''AggregationConfig

data WithAggregation output indexer input
    = WithAggregation
        { _config            :: AggregationConfig output input
        , _aggregatedIndexer :: indexer output
        }

makeLenses ''WithAggregation

instance
    (Monad m, IsIndex m output index, Eq (Point output))
    => IsIndex m input (WithAggregation output index) where

   index timedEvent indexer = do
       let point' = indexer ^. config . transformPoint $ timedEvent ^. point
       let asEvent = TimedEvent
               point'
               (indexer ^. config . transformEvent $ timedEvent ^. event)
       indexVia aggregatedIndexer asEvent indexer
