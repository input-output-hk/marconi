{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.Types (
  AddressUtxoResult (AddressUtxoResult),
  GetUtxosFromAddressParams (GetUtxosFromAddressParams),
  GetUtxosFromAddressResult (GetUtxosFromAddressResult),
) where

import Cardano.Api (FromJSON, ToJSON)
import Cardano.Api qualified as C
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (parseJSON), (.:), (.:?))
import Data.Aeson qualified as Aeson
import GHC.Generics (Generic)
import Marconi.Cardano.Core.Types (TxIndexInBlock)
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.SpentInfoResult (SpentInfoResult)
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.Wrappers (UtxoTxInput, ValueWrapper)
import Marconi.ChainIndex.Api.Types ()

data GetUtxosFromAddressParams = GetUtxosFromAddressParams
  { address :: !String
  -- ^ Address to query for
  , createdAtOrAfterSlotNo :: !(Maybe C.SlotNo)
  -- ^ Upper slot in the window
  , unspentBeforeSlotNo :: !(Maybe C.SlotNo)
  -- ^ lower slot in the window
  }
  deriving (Show, Eq, Generic)

instance FromJSON GetUtxosFromAddressParams where
  parseJSON =
    let
      parse v = do
        (lo, hi) <- buildInterval v
        GetUtxosFromAddressParams
          <$> v
            .: "address"
          <*> pure lo
          <*> pure hi
     in
      Aeson.withObject "GetUtxosFromAddressParams" parse
    where
      buildInterval v = do
        lo <-
          v .:? "createdAtOrAfterSlotNo"
            <|> fail "The 'createAfterSlotNo' param value must be a natural number"
        hi <-
          v .:? "unspentBeforeSlotNo"
            <|> fail "The 'unspentBeforeSlotNo' param value must be a natural number"
        if lo > hi
          then fail "The 'unspentBeforeSlotNo' param value must be larger than 'createAfterSlotNo'."
          else pure (lo, hi)

newtype GetUtxosFromAddressResult = GetUtxosFromAddressResult
  {unAddressUtxosResult :: [AddressUtxoResult]}
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

data AddressUtxoResult = AddressUtxoResult
  { slotNo :: !C.SlotNo
  , blockHeaderHash :: !(C.Hash C.BlockHeader)
  , epochNo :: !C.EpochNo
  , blockNo :: !C.BlockNo
  , txIndexInBlock :: !TxIndexInBlock
  , txId :: !C.TxId
  , txIx :: !C.TxIx
  , datumHash :: !(Maybe (C.Hash C.ScriptData))
  , datum :: !(Maybe C.ScriptData)
  , value :: !ValueWrapper
  , spentBy :: !(Maybe SpentInfoResult)
  , txInputs :: ![UtxoTxInput] -- List of inputs that were used in the transaction that created this UTxO.
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
