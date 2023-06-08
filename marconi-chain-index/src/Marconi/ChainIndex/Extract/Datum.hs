{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Marconi.ChainIndex.Extract.Datum where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Data.Either (rights)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)

import Cardano.Ledger.Alonzo.Scripts.Data (Data, DataHash)
import Cardano.Ledger.Era (EraCrypto)
import Cardano.Ledger.Hashes ()
import Marconi.ChainIndex.Orphans ()

-- * Datums from script data

-- | Get a map of datum hash to datum from a list of transactions.
txsPlutusDatumsMap :: Foldable f => f (C.Tx era) -> Map (C.Hash C.ScriptData) C.ScriptData
txsPlutusDatumsMap txs = foldMap txPlutusDatumsMap txs

txPlutusDatumsMap :: C.Tx era -> Map (C.Hash C.ScriptData) C.ScriptData
txPlutusDatumsMap (C.Tx txBody _) = txBodyPlutusDatumsMap txBody

-- | Get a map of datum hash to datum from transaction body.
txBodyPlutusDatumsMap :: C.TxBody era -> Map (C.Hash C.ScriptData) C.ScriptData
txBodyPlutusDatumsMap txBody = maybe Map.empty (Map.fromList . fmap toHashedData . Map.elems) $ txScriptHashDatumMap txBody

-- TODO I'm recomputing the ScriptHash hash, because cardano-api doesn't provide the correct
-- functions to convert 'Ledger.DataHash' to 'C.Hash C.ScriptData'. This should go away once
-- we fully switch to `cardano-ledger` types.
toHashedData :: Data ledgerera -> (C.Hash C.ScriptData, C.ScriptData)
toHashedData alonzoDat = let d = C.fromAlonzoData alonzoDat in (C.hashScriptDataBytes d, C.getScriptData d)

txScriptHashDatumMap :: C.TxBody era -> Maybe (Map (DataHash (EraCrypto (C.ShelleyLedgerEra era))) (Data (C.ShelleyLedgerEra era)))
txScriptHashDatumMap = \case
  C.ShelleyTxBody _ _ _ (C.TxBodyScriptData _ (Ledger.TxDats' data_) _) _ _ -> Just data_
  _ -> Nothing

-- \^ Can't match on C.ByronTxBody as it's not exported.

-- * Datums from transaction outputs

filteredTxOutDatums :: Maybe (C.Address C.ShelleyAddr -> Bool) -> [C.Tx era] -> Map (C.Hash C.ScriptData) C.ScriptData
filteredTxOutDatums addressFilter txs = Map.fromList $ rights $ map snd $ filteredAddressDatums addressFilter txs

filteredAddressDatums
  :: Maybe (C.Address C.ShelleyAddr -> Bool)
  -> [C.Tx era]
  -> [(C.AddressAny, Either (C.Hash C.ScriptData) (C.Hash C.ScriptData, C.ScriptData))]
filteredAddressDatums addressFilter txs = filter' $ concatMap txAddressDatums txs
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
txAddressDatums
  :: C.Tx era
  -> [(C.AddressAny, Either (C.Hash C.ScriptData) (C.Hash C.ScriptData, C.ScriptData))]
txAddressDatums (C.Tx (C.TxBody C.TxBodyContent{C.txOuts}) _) = mapMaybe maybePair txOuts
  where
    maybePair :: C.TxOut C.CtxTx era -> Maybe (C.AddressAny, Either (C.Hash C.ScriptData) (C.Hash C.ScriptData, C.ScriptData))
    maybePair (C.TxOut (C.AddressInEra _ addr) _ dat _) = (C.toAddressAny addr,) <$> txOutDatumOrHash dat

txOutsDatumsOrHashes :: [C.TxOut C.CtxTx era] -> [Either (C.Hash C.ScriptData) (C.Hash C.ScriptData, C.ScriptData)]
txOutsDatumsOrHashes txOuts = mapMaybe (txOutDatumOrHash . txOutDatum) txOuts
  where
    txOutDatum (C.TxOut (C.AddressInEra _ _) _ dat _) = dat

txOutDatumOrHash :: C.TxOutDatum C.CtxTx era -> Maybe (Either (C.Hash C.ScriptData) (C.Hash C.ScriptData, C.ScriptData))
txOutDatumOrHash = \case
  C.TxOutDatumHash _ dh -> Just $ Left dh
  C.TxOutDatumInTx _ d -> Just $ Right (C.hashScriptDataBytes d, C.getScriptData d)
  C.TxOutDatumInline _ d -> Just $ Right (C.hashScriptDataBytes d, C.getScriptData d)
  C.TxOutDatumNone -> Nothing

-- * Sqlite

data DatumRow = DatumRow
  { datumRowDatumHash :: C.Hash C.ScriptData
  , datumRowDatum :: C.ScriptData
  }
  deriving (Show, Generic)

instance SQL.ToRow DatumRow where
  toRow (DatumRow dh d) = [SQL.toField dh, SQL.toField d]

deriving anyclass instance SQL.FromRow DatumRow

createTable :: SQL.Connection -> IO ()
createTable c =
  SQL.execute_
    c
    " CREATE TABLE IF NOT EXISTS datumhash_datum \
    \   ( datum_hash BLOB PRIMARY KEY \
    \   , datum BLOB \
    \   ) "

createIndex :: SQL.Connection -> IO ()
createIndex c =
  SQL.execute_
    c
    "CREATE INDEX IF NOT EXISTS datumhash_datum_index ON datumhash_datum (datum_hash)"

insertRows :: SQL.Connection -> [DatumRow] -> IO ()
insertRows c =
  SQL.executeMany
    c
    "INSERT OR IGNORE INTO datumhash_datum (datum_hash, datum) VALUES (?, ?)"
