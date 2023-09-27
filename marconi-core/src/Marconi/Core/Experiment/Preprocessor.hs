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
) where

import Control.Arrow (Arrow (arr))
import Control.Lens.Operators ((%~))
import Control.Monad.State.Strict (StateT (runStateT), join, (<=<))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Trans.State.Strict (State)
import Control.Scanl (Scan (Scan), ScanM (ScanM), arrM, generalize)
import Marconi.Core.Experiment.Type (
  ProcessedInput (Index, IndexAllDescending, Rollback, StableAt, Stop),
  event,
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

-- | Lift a stateful function as a preprocessor
preprocessor
  :: (Monad m)
  => (ProcessedInput point a -> State s [ProcessedInput point b])
  -> s
  -> Preprocessor m point a b
preprocessor f = generalize . Scan (traverseJoin f)

-- | Lift a stateful and effectful function as a preprocessor
preprocessorM
  :: (Monad m)
  => (ProcessedInput point a -> StateT s m [ProcessedInput point b])
  -> m s
  -> Preprocessor m point a b
preprocessorM = ScanM . traverseJoin

traverseJoin :: (Applicative f, Monad m, Traversable m) => (a -> f (m b)) -> m a -> f (m b)
traverseJoin f = fmap join . traverse f
