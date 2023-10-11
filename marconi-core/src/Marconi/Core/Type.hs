{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    Base types to index events.

    See "Marconi.Core" for documentation.
-}
module Marconi.Core.Type (
  -- * Types and type families
  Point,
  ProcessedInput (..),
  Result,
  HasPoint (getPoint),
  Timed (Timed),
  point,
  event,

  -- * Error types
  IndexerError (..),
  QueryError (..),
) where

import Control.Exception (Exception)
import Control.Lens (Lens, Lens')
import Data.Data (Typeable)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)

{- | A point in time, the concrete type of a point is now derived from an indexer event,
 instead of an event.
 The reason is that you may not want to always carry around a point when you manipulate an event.
-}
type family Point event

{-
   This class witnesses the fact that events contain enough information to retrieve the point when
   they were produced.
-}
class HasPoint e p where
  getPoint :: e -> p

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

instance HasPoint (Timed point event) point where
  getPoint :: Timed point event -> point
  getPoint = _point

deriving stock instance (Show event, Show point) => Show (Timed point event)
deriving stock instance (Eq event, Eq point) => Eq (Timed point event)
deriving stock instance (Ord event, Ord point) => Ord (Timed point event)
deriving stock instance Functor (Timed point)
deriving stock instance Foldable (Timed point)
deriving stock instance Traversable (Timed point)
deriving stock instance Generic (Timed point event)

{- | The different types of input event that should be handled by an indexer
used to map the chain incoming events to something that an indexer should be able to digest.
-}
data ProcessedInput point event
  = -- | A rollback happen and indexers need to go back to the given point in time
    Rollback point
  | -- | A new event has to be indexed
    Index (Timed point (Maybe event))
  | -- | A new event has to be indexed
    IndexAllDescending (NonEmpty (Timed point (Maybe event)))
  | -- | Inform the indexer of the latest stable point reached
    StableAt point
  | -- | Processing stops
    Stop

deriving stock instance (Show event, Show point) => Show (ProcessedInput point event)
deriving stock instance (Eq event, Eq point) => Eq (ProcessedInput point event)
deriving stock instance (Ord event, Ord point) => Ord (ProcessedInput point event)
deriving stock instance Functor (ProcessedInput point)
deriving stock instance Foldable (ProcessedInput point)
deriving stock instance Traversable (ProcessedInput point)
deriving stock instance Generic (ProcessedInput point event)

-- | When was this event created
point :: Lens' (Timed point event) point
point f te = fmap (\_point -> te{_point}) $ f $ _point te

-- | A lens to get the event without its time information
event :: Lens (Timed point a) (Timed point b) a b
event f te = fmap (\_event -> te{_event}) $ f $ _event te

-- | Error that can occur when you index events
data IndexerError
  = -- | An indexer don't have access to the history at the point that is asked
    RollbackBehindHistory
  | -- | The indexer did not respond
    IndexerInternalError Text
  | -- | The indexer is in an invalid state and can't recover
    InvalidIndexer Text
  | -- | Indexer has to stop as requested by the given worker
    StopIndexer (Maybe Text)
  | -- | The indexer failed at resuming (likely due to a bug)
    ResumingFailed Text
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
  | -- | Upper or lower SlotNo bounds provided in the query are not consistent. For example,
    -- the requested point is too early to answer the query completely.
    SlotNoBoundsInvalid Text

deriving stock instance (Show (Result query)) => Show (QueryError query)
deriving instance (Typeable query, Show (Result query)) => Exception (QueryError query)
