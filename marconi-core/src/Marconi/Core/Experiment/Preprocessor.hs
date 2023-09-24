{-# LANGUAGE LambdaCase #-}

{- | 'Preprocessor's are used to transform the incoming events of an indexer, generally in a @Worker@.

   'Preprocessor's can carry an internal state, that can be updated on each incoming @ProcessingInput@.

   'Transfromer's are composable using 'Control.Category' and 'Control.Arrow' operators
   ('.', '>>>' and '<<<').
-}
module Marconi.Core.Experiment.Preprocessor (
  Preprocessor,
  runPreprocessor,

  -- * Stateless preprocessors
  mapEvent,
  mapMaybeEvent,
  traverseEvent,
  traverseMaybeEvent,

  -- * Stateful preprocessors
  scanEvent,
  scanEventM,
  scanMaybeEvent,
  scanMaybeEventM,

  -- * Generic builder
  preprocessor,
  preprocessorM,

  -- * Optimisation of a list of Processed inputs
  compactInputs,
) where

import Control.Arrow (Arrow (arr))
import Control.Lens (Lens')
import Control.Lens.Operators ((%~), (&), (.~), (<>~), (^.))
import Control.Monad.State.Strict (StateT (runStateT), join, (<=<))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Trans.State.Strict (State)
import Control.Scanl (Scan (Scan), ScanM (ScanM), arrM, generalize)
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Marconi.Core.Experiment.Type (
  ProcessedInput (Index, IndexAllDescending, Rollback, StableAt, Stop),
  Timed,
  event,
  point,
 )

{- | Stateful tranformer.
Map a list of preprocessor actions to a new lis of actions.
Actions should be read from left to right
element of the list
-}
type Preprocessor m point a b =
  ScanM m [ProcessedInput point a] [ProcessedInput point b]

-- | Apply a preprocessor to an element and retrieve the updated transformer
runPreprocessor
  :: (Monad m)
  => Preprocessor m point a b
  -> [ProcessedInput point a]
  -> m ([ProcessedInput point b], Preprocessor m point a b)
runPreprocessor (ScanM f mState) input = do
  state <- mState
  (res, state') <- f input `runStateT` state
  pure (res, ScanM f (pure state'))

-- | Lift a function on events to a preprocessor
mapEvent :: (Monad m) => (a -> b) -> Preprocessor m point a b
mapEvent = arr . fmap . fmap

-- | Lift a function that may emit an event to a preprocessor.
mapMaybeEvent :: forall m point a b. (Monad m) => (a -> Maybe b) -> Preprocessor m point a b
mapMaybeEvent f =
  let mapOneEvent = event %~ (>>= f)
      go :: (Applicative f) => ProcessedInput point a -> f (ProcessedInput point b)
      go (Index x) = pure $ Index $ mapOneEvent x
      go (IndexAllDescending xs) = pure . IndexAllDescending $ mapOneEvent <$> xs
      go (Rollback p) = pure $ Rollback p
      go (StableAt p) = pure $ StableAt p
      go Stop = pure Stop
   in arr (go =<<)

-- | Lift an effectful function on events to a preprocessor
traverseEvent :: (Monad m) => (a -> m b) -> Preprocessor m point a b
traverseEvent f = arrM (traverse $ traverse f)

-- | Lift an effectful function that may emit an event to a preprocessor
traverseMaybeEvent :: (Monad m) => (a -> m (Maybe b)) -> Preprocessor m point a b
traverseMaybeEvent f =
  let mapOneEvent = event (runMaybeT . (MaybeT . f <=< MaybeT . pure))
      go (Index x) = pure . Index <$> mapOneEvent x
      go (IndexAllDescending xs) = pure . IndexAllDescending <$> traverse mapOneEvent xs
      go (Rollback p) = pure . pure $ Rollback p
      go (StableAt p) = pure . pure $ StableAt p
      go Stop = pure . pure $ Stop
   in arrM $ traverseJoin go

-- | Create a tranformer from a strict stateful computation
scanEvent :: forall m s point a b. (Monad m) => (a -> State s b) -> s -> Preprocessor m point a b
scanEvent f x =
  let go :: [ProcessedInput point a] -> State s [ProcessedInput point b]
      go = traverse (traverse f)
   in generalize $ Scan go x

-- | Create a tranformer from a strict stateful computation
scanMaybeEvent
  :: forall m s point a b. (Monad m) => (a -> State s (Maybe b)) -> s -> Preprocessor m point a b
scanMaybeEvent f =
  let mapOneEvent = event (runMaybeT . (MaybeT . f <=< MaybeT . pure))
      go :: (Applicative f) => ProcessedInput point a -> State s (f (ProcessedInput point b))
      go (Index x) = pure . Index <$> mapOneEvent x
      go (IndexAllDescending xs) = pure . IndexAllDescending <$> traverse mapOneEvent xs
      go (Rollback p) = pure . pure $ Rollback p
      go (StableAt p) = pure . pure $ StableAt p
      go Stop = pure . pure $ Stop
   in generalize . Scan (traverseJoin go)

-- | Create a tranformer from a strict stateful computation
scanEventM
  :: forall m s point a b. (Monad m) => (a -> StateT s m b) -> m s -> Preprocessor m point a b
scanEventM f x =
  let go :: [ProcessedInput point a] -> StateT s m [ProcessedInput point b]
      go = traverse (traverse f)
   in ScanM go x

-- | Create a tranformer from a strict stateful computation
scanMaybeEventM
  :: forall m s point a b. (Monad m) => (a -> StateT s m (Maybe b)) -> m s -> Preprocessor m point a b
scanMaybeEventM f =
  let mapOneEvent = event (runMaybeT . (MaybeT . f <=< MaybeT . pure))
      go :: (Applicative f) => ProcessedInput point a -> StateT s m (f (ProcessedInput point b))
      go (Index x) = pure . Index <$> mapOneEvent x
      go (IndexAllDescending xs) = pure . IndexAllDescending <$> traverse mapOneEvent xs
      go (Rollback p) = pure . pure $ Rollback p
      go (StableAt p) = pure . pure $ StableAt p
      go Stop = pure . pure $ Stop
   in ScanM $ traverseJoin go

preprocessor
  :: (Monad m)
  => (ProcessedInput point a -> State s [ProcessedInput point b])
  -> s
  -> Preprocessor m point a b
preprocessor f = generalize . Scan (traverseJoin f)

preprocessorM
  :: (Monad m)
  => (ProcessedInput point a -> StateT s m [ProcessedInput point b])
  -> m s
  -> Preprocessor m point a b
preprocessorM = ScanM . traverseJoin

data AccumulateInputs point event = AccumulateInputs
  { _ongoingIndex :: [Timed point (Maybe event)]
  , _initialRollback :: Maybe point
  , _currentStable :: Maybe point
  , _shouldStop :: Bool
  }

deriving stock instance (Show point, Show event) => Show (AccumulateInputs point event)

ongoingIndex :: Lens' (AccumulateInputs point event) [Timed point (Maybe event)]
ongoingIndex f acc = fmap (\_ongoingIndex -> acc{_ongoingIndex}) $ f $ _ongoingIndex acc

initialRollback :: Lens' (AccumulateInputs point event) (Maybe point)
initialRollback f acc = fmap (\_initialRollback -> acc{_initialRollback}) $ f $ _initialRollback acc

currentStable :: Lens' (AccumulateInputs point event) (Maybe point)
currentStable f acc = fmap (\_currentStable -> acc{_currentStable}) $ f $ _currentStable acc

shouldStop :: Lens' (AccumulateInputs point event) Bool
shouldStop f acc = fmap (\_shouldStop -> acc{_shouldStop}) $ f $ _shouldStop acc

asInputs :: AccumulateInputs point event -> [ProcessedInput point event]
asInputs acc =
  let stop = if acc ^. shouldStop then [Stop] else []
      rollbackFirst = toList $ Rollback <$> acc ^. initialRollback
      asIndex [] = Nothing
      asIndex [x] = Just $ Index x
      asIndex (x : xs) = Just $ IndexAllDescending (x :| xs)
      finalIndex = toList $ asIndex $ acc ^. ongoingIndex
   in join [rollbackFirst, finalIndex, stop]

-- | Compact a list of input provided in a left to right order to optimise indexers operations
compactInputs :: (Ord point) => [ProcessedInput point event] -> [ProcessedInput point event]
compactInputs inputs =
  let isStop = \case
        Stop -> True
        _other -> False
      chooseStable p = Just . maybe p (max p)
      chooseRollback p = Just . maybe p (min p)
      go acc = \case
        Index e -> case acc ^. initialRollback of
          Nothing -> acc & ongoingIndex %~ (e :)
          Just p | p <= e ^. point -> acc & ongoingIndex %~ (e :)
          _otherwise -> acc
        IndexAllDescending es -> acc & ongoingIndex <>~ toList es
        Rollback p -> acc & initialRollback %~ chooseRollback p
        StableAt p -> case acc ^. initialRollback of
          Nothing -> acc & currentStable %~ chooseStable p
          Just p' | p' <= p -> acc & currentStable %~ chooseStable p
          _otherwise -> acc
        Stop -> acc & shouldStop .~ True
      foldInput = asInputs . foldr (flip go) (AccumulateInputs [] Nothing Nothing False)
   in case span isStop inputs of
        (xs, []) -> foldInput xs
        (xs, _) -> foldInput xs <> [Stop]

traverseJoin :: (Applicative f, Monad m, Traversable m) => (a -> f (m b)) -> m a -> f (m b)
traverseJoin f = fmap join . traverse f
