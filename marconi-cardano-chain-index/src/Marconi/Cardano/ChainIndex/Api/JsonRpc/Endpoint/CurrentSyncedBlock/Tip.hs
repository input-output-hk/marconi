{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.CurrentSyncedBlock.Tip (
  Tip (..),
  fromChainTip,
) where

import Cardano.Api qualified as C
import Data.Aeson.TH (defaultOptions, deriveJSON)

data Tip = Tip
  { blockNo :: C.BlockNo
  , blockHeaderHash :: C.Hash C.BlockHeader
  , slotNo :: C.SlotNo
  }
  deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions ''Tip)

fromChainTip :: C.ChainTip -> Maybe Tip
fromChainTip C.ChainTipAtGenesis = Nothing
fromChainTip (C.ChainTip slotNo blockHeaderHash blockNo) =
  Just $ Tip{blockNo, blockHeaderHash, slotNo}
