{-# LANGUAGE FlexibleContexts #-}

{- |
    'ResumableInterval' is used by indexers to indicate the interval of points in time they can
    resume from.

    See Note [Resuming strategy] for details on how to deal with resuming.
-}
module Marconi.Core.ResumableInterval where

import Data.List qualified as List
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (Down (Down))

-- | Interval of time an indexer can resume from.
data ResumableInterval a = ResumableInterval
    { ivFrom :: !(ResumableIntervalLowerBound a) -- ^ Inclusive lower bound
    , ivTo   :: !a  -- ^ Exclusive upper bound
    }

-- | Lower bound of the interval which can either be the genesis point or a specific point in time.
data ResumableIntervalLowerBound a = Genesis | At !a

instance Eq a => Eq (ResumableIntervalLowerBound a) where
    Genesis == Genesis = True
    At l == At r       = l == r
    _ == _             = False

instance Ord a => Ord (ResumableIntervalLowerBound a) where
    Genesis `compare` Genesis = EQ
    Genesis `compare` _       = LT
    _ `compare` Genesis       = GT
    At l `compare` At r       = l `compare` r

intersection
    :: Ord a
    => ResumableInterval a
    -> ResumableInterval a
    -> Maybe (ResumableInterval a)
intersection (ResumableInterval l1 h1) (ResumableInterval l2 h2) =
    let newLowerBound = max l1 l2
        newUpperBound = min h1 h2
     in
        case newLowerBound of
          Genesis                     -> Just $ ResumableInterval newLowerBound newUpperBound
          At lb | lb <= newUpperBound -> Just $ ResumableInterval newLowerBound newUpperBound
          At _                        -> Nothing

to :: Enum a => a -> ResumableInterval a
to s = ResumableInterval Genesis (succ s)

singleton :: (Enum a) => a -> ResumableInterval a
singleton s = ResumableInterval (At s) (succ s)

latestResumablePoint
    :: (Ord a)
    => [[ResumableInterval a]]
    -- ^ List of resumable intervals. Each indexer will returns a list of resumable intervals.
    -> Maybe a
latestResumablePoint ivs = listToMaybe $ List.sortOn Down $ fmap ivTo $ combineAll ivs

combineAll
    :: (Ord a)
    => [[ResumableInterval a]] -- ^ List of resumable intervals for multiple indexers.
    -> [ResumableInterval a]
combineAll = foldr (\acc x -> combine acc x) []

combine
    :: (Ord a)
    => [ResumableInterval a] -- ^ List of resumable intervals for indexer 'A'.
    -> [ResumableInterval a] -- ^ List of resumable intervals for indexer 'B'.
    -> [ResumableInterval a]
combine ivs1 ivs2 = catMaybes $ do
    iv1 <- ivs1
    iv2 <- ivs2
    pure $ intersection iv1 iv2
