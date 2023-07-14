{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Marconi.ChainIndex.Extract.Datum where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Scripts.Data (Data, DataHash)
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Era (EraCrypto)
import Cardano.Ledger.Hashes ()
import Data.Either (rights)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Marconi.ChainIndex.Orphans ()

-- * Datums from script data

-- | Get a map of datum hash to datum from a list of transactions.
getPlutusDatumsFromTxs :: (Foldable f) => f (C.Tx era) -> Map (C.Hash C.ScriptData) C.ScriptData
getPlutusDatumsFromTxs = foldMap getPlutusDatumsFromTx

getPlutusDatumsFromTx :: C.Tx era -> Map (C.Hash C.ScriptData) C.ScriptData
getPlutusDatumsFromTx (C.Tx txBody _) = getPlutusDatumsFromTxBody txBody

-- | Get a map of datum hash to datum from transaction body.
getPlutusDatumsFromTxBody :: C.TxBody era -> Map (C.Hash C.ScriptData) C.ScriptData
getPlutusDatumsFromTxBody txBody = maybe Map.empty (Map.fromList . fmap toHashedData . Map.elems) $ getDatumMapFromTxBody txBody

-- TODO I'm recomputing the ScriptHash hash, because cardano-api doesn't provide the correct
-- functions to convert 'Ledger.DataHash' to 'C.Hash C.ScriptData'. This should go away once
-- we fully switch to `cardano-ledger` types.
toHashedData :: Data ledgerera -> (C.Hash C.ScriptData, C.ScriptData)
toHashedData alonzoDat = let d = C.fromAlonzoData alonzoDat in (C.hashScriptDataBytes d, C.getScriptData d)

getDatumMapFromTxBody
  :: C.TxBody era
  -> Maybe (Map (DataHash (EraCrypto (C.ShelleyLedgerEra era))) (Data (C.ShelleyLedgerEra era)))
getDatumMapFromTxBody = \case
  C.ShelleyTxBody _ _ _ (C.TxBodyScriptData _ (Ledger.TxDats' data_) _) _ _ -> Just data_
  _ -> Nothing

getFilteredTxOutDatumsFromTxs
  :: Maybe (C.Address C.ShelleyAddr -> Bool) -> [C.Tx era] -> Map (C.Hash C.ScriptData) C.ScriptData
getFilteredTxOutDatumsFromTxs addressFilter txs = Map.fromList $ rights $ map snd $ getFilteredAddressDatumsFromTxs addressFilter txs

getFilteredAddressDatumsFromTxs
  :: Maybe (C.Address C.ShelleyAddr -> Bool)
  -> [C.Tx era]
  -> [(C.AddressAny, Either (C.Hash C.ScriptData) (C.Hash C.ScriptData, C.ScriptData))]
getFilteredAddressDatumsFromTxs addressFilter txs = filter' $ concatMap getAddressDatumsFromTx txs
  where
    filter' :: [(C.AddressAny, a)] -> [(C.AddressAny, a)]
    filter' = case addressFilter of
      Nothing -> id
      Just f -> filter $ \(addr, _) -> case addr of
        -- Target addresses filter are only shelley addresses. Therefore, as we
        -- encounter Byron addresses with datum, we don't filter them. However, that
        -- is highly improbable as Byron addresses are almost never used anymore.
        C.AddressByron _ -> True
        C.AddressShelley addr' -> f addr'

-- | Get an association list from address to either datum hash or a pair of datum hash and datum itself.
getAddressDatumsFromTx
  :: C.Tx era
  -> [(C.AddressAny, Either (C.Hash C.ScriptData) (C.Hash C.ScriptData, C.ScriptData))]
getAddressDatumsFromTx (C.Tx (C.TxBody C.TxBodyContent{C.txOuts}) _) = mapMaybe maybePair txOuts
  where
    maybePair
      :: C.TxOut C.CtxTx era
      -> Maybe (C.AddressAny, Either (C.Hash C.ScriptData) (C.Hash C.ScriptData, C.ScriptData))
    maybePair (C.TxOut (C.AddressInEra _ addr) _ dat _) = (C.toAddressAny addr,) <$> getTxOutDatumOrHash dat

getTxOutsDatumsOrHashes
  :: [C.TxOut C.CtxTx era] -> [Either (C.Hash C.ScriptData) (C.Hash C.ScriptData, C.ScriptData)]
getTxOutsDatumsOrHashes = mapMaybe (getTxOutDatumOrHash . txOutDatum)
  where
    txOutDatum (C.TxOut (C.AddressInEra _ _) _ dat _) = dat

getTxOutDatumOrHash
  :: C.TxOutDatum C.CtxTx era -> Maybe (Either (C.Hash C.ScriptData) (C.Hash C.ScriptData, C.ScriptData))
getTxOutDatumOrHash = \case
  C.TxOutDatumHash _ dh -> Just $ Left dh
  C.TxOutDatumInTx _ d -> Just $ Right (C.hashScriptDataBytes d, C.getScriptData d)
  C.TxOutDatumInline _ d -> Just $ Right (C.hashScriptDataBytes d, C.getScriptData d)
  C.TxOutDatumNone -> Nothing
