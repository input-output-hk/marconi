{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Marconi.ChainIndex.Experimental.Api.JsonRpc.Endpoint.CurrentSyncedBlock.Tip (
  Tip (..),
  fromChainTip,
) where

import Cardano.Api qualified as C
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Tip = Tip
  { blockNo :: C.BlockNo
  , blockHeaderHash :: C.Hash C.BlockHeader
  , slotNo :: C.SlotNo
  }
  deriving (Eq, Ord, Generic, Show, FromJSON, ToJSON)

fromChainTip :: C.ChainTip -> Maybe Tip
fromChainTip C.ChainTipAtGenesis = Nothing
fromChainTip (C.ChainTip slotNo blockHeaderHash blockNo) =
  Just $ Tip{blockNo, blockHeaderHash, slotNo}
