{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo where

import Cardano.Api qualified as C
import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (TxIndexInBlock)
import Marconi.Core.JsonRpc (ReaderHandler)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig, withChainIndexHandler)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr)

{- METHOD -}
type RpcPastAddressUtxoMethod = ()

--  JsonRpc
--    "getUtxosFromAddress"
--    GetUtxosFromAddressParams
--    String
--    GetUtxosFromAddressResult

{- TYPES -}

newtype GetUtxosFromAddressResult = GetUtxosFromAddressResult
  {unAddressUtxosResult :: [AddressUtxoResult]}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

data AddressUtxoResult = AddressUtxoResult
  { slotNo :: C.SlotNo
  , blockHeaderHash :: C.Hash C.BlockHeader
  , epochNo :: C.EpochNo
  , blockNo :: C.BlockNo
  , txIndexInBlock :: TxIndexInBlock
  , txId :: C.TxId
  , txIx :: C.TxIx
  , datumHash :: Maybe (C.Hash C.ScriptData)
  , datum :: Maybe C.ScriptData
  , value :: C.Value
  , spentBy :: Maybe SpentInfoResult
  , txInputs :: [UtxoTxInput]
  -- ^ List of inputs that were used in the transaction that created this UTxO.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
