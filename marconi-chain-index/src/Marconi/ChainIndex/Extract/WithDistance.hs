-- | Attach distance to the tip to an event
module Marconi.ChainIndex.Extract.WithDistance (
  WithDistance (WithDistance),
  attachDistance,
  chainDistance,
  getEvent,
  addDistanceToBlockEvent,
  -- TODO: PLT-8203 this utility belongs elsewhere
  blockEventPoint,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent (BlockEvent))
import Cardano.Api.Extended.Streaming.ChainSyncEvent (ChainSyncEvent (RollBackward, RollForward))
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

-- NOTE: PLT-8203 taken from withDistancePreprocessor
addDistanceToBlockEvent
  :: ChainSyncEvent BlockEvent
  -> [Core.ProcessedInput C.ChainPoint (WithDistance BlockEvent)]
addDistanceToBlockEvent (RollForward x tip) =
  let point = blockEventPoint x
      blockWithDistance
        :: BlockEvent
        -> Core.Timed C.ChainPoint (WithDistance BlockEvent)
      blockWithDistance (BlockEvent b@(C.BlockInMode block _) epochNo' bt) =
        let (C.Block (C.BlockHeader _slotNo _hsh currentBlockNo) _) = block
            withDistance = attachDistance currentBlockNo tip (BlockEvent b epochNo' bt)
         in Core.Timed point withDistance
   in [Core.Index $ Just <$> blockWithDistance x]
addDistanceToBlockEvent (RollBackward x _tip) = [Core.Rollback x]

blockEventPoint :: BlockEvent -> C.ChainPoint
blockEventPoint (BlockEvent (C.BlockInMode block _) _epochNo' _bt) =
  let (C.Block (C.BlockHeader slotNo hsh _currentBlockNo) _) = block
   in C.ChainPoint slotNo hsh
