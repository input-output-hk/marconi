{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    Base types to index events.

    See "Marconi.Core.Experiment" for documentation.
-}
module Marconi.Core.Experiment.Type (
  -- * Types and type families
  Point,
  Result,
  Timed (Timed),
  point,
  event,

  -- * Error types
  IndexerError (..),
  QueryError (..),
) where

import Control.Exception (Exception)
import Control.Lens (Lens')
import Data.Text (Text)

{- | A point in time, the concrete type of a point is now derived from an indexer event,
 instead of an event.
 The reason is that you may not want to always carry around a point when you manipulate an event.
-}
type family Point event

{- | A 'Result' is a data family for query descriptor.

 A query is tied to an indexer by a typeclass, this design choice has two main reasons:
     * we want to be able to define different query for the same indexer
       (eg. we may want to define two distinct query types for an utxo indexer:
       one to ge all the utxo for a given address,
       another one for to get all the utxos emitted at a given slot).
     * we want to assign a query type to different indexers.
-}
type family Result query

-- | Attach an event to a point in time
data Timed point event = Timed
  { _point :: point
  , _event :: event
  }

deriving stock instance (Show event, Show point) => Show (Timed point event)
deriving stock instance (Eq event, Eq point) => Eq (Timed point event)
deriving stock instance (Ord event, Ord point) => Ord (Timed point event)
deriving stock instance Functor (Timed point)
deriving stock instance Foldable (Timed point)
deriving stock instance Traversable (Timed point)

-- | When was this event created
point :: Lens' (Timed point event) point
point f te = fmap (\_point -> te{_point}) $ f $ _point te

-- | A lens to get the event without its time information
event :: Lens' (Timed point event) event
event f te = fmap (\_event -> te{_event}) $ f $ _event te

-- | Error that can occur when you index events
data IndexerError
  = -- | An indexer don't have access to the history at the point that is asked
    RollbackBehindHistory
  | -- | The indexer did not respond
    IndexerInternalError Text
  | -- | The indexer is in an invalid state and can't recover
    InvalidIndexer Text
  | -- | Any other cause of failure
    OtherIndexError Text

instance Exception IndexerError

deriving stock instance Show IndexerError
deriving stock instance Eq IndexerError

-- | Error that can occurs when you query an indexer
data QueryError query
  = -- | The required point is ahead of the current index.
    -- The error may still provide its latest result if it make sense for the given query.
    --
    -- It can be useful for indexer that contains a partial knowledge and that want to pass
    -- this knowledge to another indexer to complete the query.
    AheadOfLastSync (Maybe (Result query))
  | -- | The requested point is too far in the past and has been pruned
    NotStoredAnymore
  | -- | The indexer query failed
    IndexerQueryError Text

deriving stock instance Show (Result query) => Show (QueryError query)
