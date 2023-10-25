-- | Attach distance to the tip to an event
module Marconi.ChainIndex.Extract.WithDistance (
  WithDistance (WithDistance),
  attachDistance,
  chainDistance,
  getEvent,
) where

import Cardano.Api qualified as C
import Data.Word (Word64)
import Marconi.Core qualified as Core

-- | Attach the distance (in blocks) to the tip to an event
data WithDistance event = WithDistance Word64 event
  deriving (Functor, Foldable, Traversable)

chainDistance :: WithDistance event -> Word64
chainDistance (WithDistance distance _) = distance

getEvent :: WithDistance event -> event
getEvent (WithDistance _ event) = event

type instance Core.Point (WithDistance event) = C.ChainPoint

-- | Extract the timed information from a block
attachDistance
  :: C.BlockNo
  -> C.ChainTip
  -> event
  -> WithDistance event
attachDistance currentBlockNo tip =
  let tipBlockNo = case tip of
        C.ChainTipAtGenesis -> 0
        C.ChainTip _ _ no -> no
      distance = C.unBlockNo $ tipBlockNo - currentBlockNo
   in WithDistance distance
