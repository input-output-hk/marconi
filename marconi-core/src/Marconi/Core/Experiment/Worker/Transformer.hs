{-# LANGUAGE LambdaCase #-}

{- | 'Transformer's are used in a worker to transform the incoming events.

   'Transformer's can carry an internal state, that can be updated on each incoming @ProcessingInput@.

   'Transfromer's are composable using 'Control.Category' operators (@.@, @>>>@ and @<<<@)
-}
module Marconi.Core.Experiment.Worker.Transformer (
  Transformer,
  runTransformer,

  -- * Stateless transformers
  mapEvent,
  mapMaybeEvent,
  traverseEvent,
  traverseMaybeEvent,

  -- * Stateful transformers
  scanEvent,
  scanEventM,
  scanMaybeEvent,
  scanMaybeEventM,

  -- * Generic builder
  transformer,
  transformerM,
) where

import Control.Arrow (Arrow (arr))
import Control.Lens.Operators ((%~))
import Control.Monad.State.Strict (StateT (runStateT), (<=<))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Trans.State.Strict (State)
import Control.Scanl (Scan (Scan), ScanM (ScanM), arrM, generalize)
import Marconi.Core.Experiment.Type (
  ProcessedInput (Index, IndexAllDescending, Rollback, Stop),
  event,
 )

-- | Stateful tranformer.
type Transformer m point a b =
  ScanM m (Maybe (ProcessedInput point a)) (Maybe (ProcessedInput point b))

-- | Apply a transformer to an element and retrieve the updated transformer
runTransformer
  :: (Monad m)
  => Transformer m point a b
  -> Maybe (ProcessedInput point a)
  -> m (Maybe (ProcessedInput point b), Transformer m point a b)
runTransformer (ScanM f mState) input = do
  state <- mState
  (res, state') <- f input `runStateT` state
  pure (res, ScanM f (pure state'))

-- | Lift a function on events to a transformer
mapEvent :: (Monad m) => (a -> b) -> Transformer m point a b
mapEvent = arr . fmap . fmap

-- | Lift a function that may emit an event to a transformer.
mapMaybeEvent :: (Monad m) => (a -> Maybe b) -> Transformer m point a b
mapMaybeEvent f =
  -- TODO later, we should remove empty events from it
  let mapOneEvent = event %~ (>>= f)
      go (Index x) = Just $ Index $ mapOneEvent x
      go (IndexAllDescending xs) = Just . IndexAllDescending $ mapOneEvent <$> xs
      go (Rollback p) = Just $ Rollback p
      go Stop = Just Stop
   in arr (go =<<)

-- | Lift an effectful function on events to a transformer
traverseEvent :: (Monad m) => (a -> m b) -> Transformer m point a b
traverseEvent f = arrM (traverse $ traverse f)

-- | Lift an effectful function that may emit an event to a transformer
traverseMaybeEvent :: (Monad m) => (a -> m (Maybe b)) -> Transformer m point a b
traverseMaybeEvent f =
  -- TODO later, we should remove empty events from it
  let mapOneEvent = event (runMaybeT . (MaybeT . f <=< MaybeT . pure))
      go (Index x) = Just . Index <$> mapOneEvent x
      go (IndexAllDescending xs) = Just . IndexAllDescending <$> traverse mapOneEvent xs
      go (Rollback p) = pure . Just $ Rollback p
      go Stop = pure $ Just Stop
   in arrM $ \case
        Nothing -> pure Nothing
        Just e -> go e

-- | Create a tranformer from a strict stateful computation
scanEvent :: forall m s point a b. (Monad m) => (a -> State s b) -> s -> Transformer m point a b
scanEvent f x =
  let go :: Maybe (ProcessedInput point a) -> State s (Maybe (ProcessedInput point b))
      go = traverse (traverse f)
   in generalize $ Scan go x

-- | Create a tranformer from a strict stateful computation
scanMaybeEvent
  :: forall m s point a b. (Monad m) => (a -> State s (Maybe b)) -> s -> Transformer m point a b
scanMaybeEvent f =
  let mapOneEvent = event (runMaybeT . (MaybeT . f <=< MaybeT . pure))
      goEvent (Index x) = Just . Index <$> mapOneEvent x
      goEvent (IndexAllDescending xs) = Just . IndexAllDescending <$> traverse mapOneEvent xs
      goEvent (Rollback p) = pure . Just $ Rollback p
      goEvent Stop = pure $ Just Stop
      go :: Maybe (ProcessedInput point a) -> State s (Maybe (ProcessedInput point b))
      go Nothing = pure Nothing
      go (Just y) = goEvent y
   in generalize . Scan go

-- | Create a tranformer from a strict stateful computation
scanEventM
  :: forall m s point a b. (Monad m) => (a -> StateT s m b) -> m s -> Transformer m point a b
scanEventM f x =
  let go :: Maybe (ProcessedInput point a) -> StateT s m (Maybe (ProcessedInput point b))
      go = traverse (traverse f)
   in ScanM go x

-- | Create a tranformer from a strict stateful computation
scanMaybeEventM
  :: forall m s point a b. (Monad m) => (a -> StateT s m (Maybe b)) -> m s -> Transformer m point a b
scanMaybeEventM f =
  let mapOneEvent = event (runMaybeT . (MaybeT . f <=< MaybeT . pure))
      goEvent (Index x) = Just . Index <$> mapOneEvent x
      goEvent (IndexAllDescending xs) = Just . IndexAllDescending <$> traverse mapOneEvent xs
      goEvent (Rollback p) = pure . Just $ Rollback p
      goEvent Stop = pure $ Just Stop
      go :: Maybe (ProcessedInput point a) -> StateT s m (Maybe (ProcessedInput point b))
      go Nothing = pure Nothing
      go (Just y) = goEvent y
   in ScanM go

transformer
  :: (Monad m)
  => (ProcessedInput point a -> State s (Maybe (ProcessedInput point b)))
  -> s
  -> Transformer m point a b
transformer f =
  let go Nothing = pure Nothing
      go (Just x) = f x
   in generalize . Scan go

transformerM
  :: (Monad m)
  => (ProcessedInput point a -> StateT s m (Maybe (ProcessedInput point b)))
  -> m s
  -> Transformer m point a b
transformerM f =
  let go Nothing = pure Nothing
      go (Just x) = f x
   in ScanM go
