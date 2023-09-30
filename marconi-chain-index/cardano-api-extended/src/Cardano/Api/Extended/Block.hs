module Cardano.Api.Extended.Block where

import Cardano.Api qualified as C

bimBlockNo :: C.BlockInMode C.CardanoMode -> C.BlockNo
bimBlockNo (C.BlockInMode (C.Block (C.BlockHeader _ _ blockNo) _) _) = blockNo

bimSlotNo :: C.BlockInMode C.CardanoMode -> C.SlotNo
bimSlotNo (C.BlockInMode (C.Block (C.BlockHeader slotNo _ _) _) _) = slotNo

bimBlockHeaderHash :: C.BlockInMode C.CardanoMode -> C.Hash C.BlockHeader
bimBlockHeaderHash (C.BlockInMode (C.Block (C.BlockHeader _ bhh _) _) _) = bhh

chainTipBlockNo :: C.ChainTip -> C.BlockNo
chainTipBlockNo C.ChainTipAtGenesis = 0
chainTipBlockNo (C.ChainTip _ _ bn) = bn
