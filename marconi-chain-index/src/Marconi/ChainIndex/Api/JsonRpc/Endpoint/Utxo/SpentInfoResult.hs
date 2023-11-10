module Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.SpentInfoResult where

import Cardano.Api (FromJSON, ToJSON)
import Cardano.Api qualified as C
import GHC.Generics (Generic)

-- This module only exists because of record name clashes

data SpentInfoResult = SpentInfoResult
  { slotNo :: !C.SlotNo
  , txId :: !C.TxId
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
