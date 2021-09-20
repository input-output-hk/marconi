module Halogen.Analytics where

import Analytics (class IsEvent, analyticsTracking)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (HalogenM)
import Prologue

withAnalytics ::
  forall state action slots message m a.
  MonadEffect m =>
  IsEvent action =>
  (action -> HalogenM state action slots message m a) -> action -> HalogenM state action slots message m a
withAnalytics handler action = do
  liftEffect $ analyticsTracking action
  handler action
