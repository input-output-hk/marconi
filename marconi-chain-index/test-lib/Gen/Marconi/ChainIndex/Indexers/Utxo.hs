{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}

module Gen.Marconi.ChainIndex.Indexers.Utxo (
  genUtxoEvents,
  genShelleyEraUtxoEvents,
  genUtxoEventsWithTxs,
  genUtxoEventsWithTxs',
  Gen.genTxBodyContentFromTxIns,
  Gen.genTxBodyContentFromTxInsWithPhase2Validation,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad (forM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Gen.Marconi.ChainIndex.Mockchain (
  BlockHeader (BlockHeader),
  MockBlock (MockBlock),
 )
import Gen.Marconi.ChainIndex.Mockchain qualified as Gen
import Hedgehog (Gen)
import Marconi.ChainIndex.Extract.Datum qualified as Datum
import Marconi.ChainIndex.Indexers.Utxo (
  BlockInfo (BlockInfo),
  StorableEvent (UtxoEvent),
  Utxo (Utxo),
  UtxoHandle,
  _address,
 )
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types (TxIndexInBlock)
import Test.Gen.Cardano.Api.Typed qualified as CGen

{- | Generates a list of UTXO events.

 This generators has the following properties:

   * that any generated UTXO is unique
   * for any spent tx output, there must be a UTXO created in a previous event
-}
genUtxoEvents :: Gen [StorableEvent UtxoHandle]
genUtxoEvents = fmap fst <$> genUtxoEventsWithTxs

{- | Generates a list of UTXO events, along with the list of transactions that generated each of
 them.

 This generators has the following properties:

   * that any generated UTXO is unique
   * for any spent tx output, there must be a UTXO created in a previous event
-}
genUtxoEventsWithTxs :: Gen [(StorableEvent UtxoHandle, MockBlock C.BabbageEra)]
genUtxoEventsWithTxs = genUtxoEventsWithTxs' Gen.genTxBodyContentFromTxIns

genUtxoEventsWithTxs'
  :: ([C.TxIn] -> Gen (C.TxBodyContent C.BuildTx C.BabbageEra))
  -> Gen [(StorableEvent UtxoHandle, MockBlock C.BabbageEra)]
genUtxoEventsWithTxs' genTxBodyContent = do
  fmap (\block -> (getStorableEventFromBlock block, block))
    <$> Gen.genMockchainWithTxBodyGen genTxBodyContent
  where
    getStorableEventFromBlock :: MockBlock C.BabbageEra -> StorableEvent UtxoHandle
    getStorableEventFromBlock (MockBlock (BlockHeader slotNo blockHeaderHash blockNo) txs) =
      let (TxOutBalance utxos spentTxOuts) = foldMap txOutBalanceFromTx txs
          utxoMap = foldMap getUtxosFromTx $ zip txs [0 ..]
          resolvedUtxos = mapMaybe (`Map.lookup` utxoMap) $ Set.toList utxos

          plutusDatums :: Map (C.Hash C.ScriptData) C.ScriptData
          plutusDatums = Datum.getPlutusDatumsFromTxs txs
          filteredTxOutDatums :: Map (C.Hash C.ScriptData) C.ScriptData
          filteredTxOutDatums = Datum.getFilteredTxOutDatumsFromTxs Nothing txs
       in -- We don't care about the timestamp or the epochNo, so we put default values.
          UtxoEvent resolvedUtxos spentTxOuts (BlockInfo slotNo blockHeaderHash blockNo 0 1) $
            Map.union plutusDatums filteredTxOutDatums

    getUtxosFromTx :: (C.Tx C.BabbageEra, TxIndexInBlock) -> Map C.TxIn Utxo
    getUtxosFromTx (C.Tx txBody@(C.TxBody txBodyContent) _, txIndexInBlock) =
      let txId = C.getTxId txBody
       in Map.fromList
            $ fmap
              ( \(txIx, txOut) ->
                  ( C.TxIn txId (C.TxIx txIx)
                  , convertTxOutToUtxo txId txIndexInBlock (C.TxIx txIx) txOut
                  )
              )
            $ zip [0 ..]
            $ C.txOuts txBodyContent

-- | The effect of a transaction (or a number of them) on the tx output set.
data TxOutBalance = TxOutBalance
  { _tobUnspent :: !(Set C.TxIn)
  -- ^ Outputs newly added by the transaction(s)
  , _tobSpent :: !(Map C.TxIn C.TxId)
  -- ^ Outputs spent by the transaction(s)
  }
  deriving stock (Eq, Show, Generic)

instance Semigroup TxOutBalance where
  tobL <> tobR =
    TxOutBalance
      { _tobUnspent =
          _tobUnspent tobR
            <> (_tobUnspent tobL `Set.difference` Map.keysSet (_tobSpent tobR))
      , _tobSpent = _tobSpent tobL <> _tobSpent tobR
      }

instance Monoid TxOutBalance where
  mappend = (<>)
  mempty = TxOutBalance mempty mempty

txOutBalanceFromTx :: C.Tx era -> TxOutBalance
txOutBalanceFromTx (C.Tx txBody@(C.TxBody txBodyContent) _) =
  let txId = C.getTxId txBody
      txInputs = Map.fromList $ (,txId) . fst <$> C.txIns txBodyContent
      utxoRefs =
        Set.fromList $
          fmap (\(txIx, _) -> C.TxIn txId $ C.TxIx txIx) $
            zip [0 ..] $
              C.txOuts txBodyContent
   in TxOutBalance utxoRefs txInputs

convertTxOutToUtxo :: C.TxId -> TxIndexInBlock -> C.TxIx -> C.TxOut C.CtxTx C.BabbageEra -> Utxo
convertTxOutToUtxo txid txIndexInBlock txix (C.TxOut (C.AddressInEra _ addr) val txOutDatum refScript) =
  let (scriptDataHash, _scriptData) =
        case txOutDatum of
          C.TxOutDatumNone -> (Nothing, Nothing)
          C.TxOutDatumHash _ dh -> (Just dh, Nothing)
          C.TxOutDatumInTx _ d -> (Just $ C.hashScriptDataBytes d, Just d)
          C.TxOutDatumInline _ d -> (Just $ C.hashScriptDataBytes d, Just d)
      (scriptHash, script) =
        case refScript of
          C.ReferenceScriptNone -> (Nothing, Nothing)
          C.ReferenceScript _ scriptInAnyLang@(C.ScriptInAnyLang _ s) ->
            (Just $ C.hashScript s, Just scriptInAnyLang)
   in Utxo
        (C.toAddressAny addr)
        (C.TxIn txid txix)
        scriptDataHash
        (C.txOutValueToValue val)
        script
        scriptHash
        txIndexInBlock

-- | Generate Utxo events with Shelley ero addresses
genShelleyEraUtxoEvents :: Gen [StorableEvent Utxo.UtxoHandle]
genShelleyEraUtxoEvents = do
  events <- genUtxoEvents
  forM events $ \event -> do
    us <- forM (Utxo.ueUtxos event) $ \utxo -> do
      a <- CGen.genAddressShelley
      pure $ utxo{_address = C.toAddressAny a}
    pure event{Utxo.ueUtxos = us}
