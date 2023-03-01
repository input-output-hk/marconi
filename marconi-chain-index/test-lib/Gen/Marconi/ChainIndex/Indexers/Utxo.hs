{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Gen.Marconi.ChainIndex.Indexers.Utxo
    ( genUtxoEvents
    , genShelleyEraUtxoEvents
    , genEventWithShelleyAddressAtChainPoint
    )
where

import Control.Monad (foldM, forM)
import Data.Set (Set)
import Data.Set qualified as Set
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Gen.Cardano.Api.Typed qualified as CGen
import Gen.Marconi.ChainIndex.Types (genChainPoints, genTxOutTxContext, nonEmptySubset)
import Helpers (emptyTxBodyContent)
import Marconi.ChainIndex.Indexers.Utxo (StorableEvent (UtxoEvent), Utxo (Utxo), UtxoHandle, _address)
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo

-- | Generates a list of UTXO events.
--
-- This generators has the following properties:
--
--   * that any generated UTXO is unique
--   * for any spent tx output, there must be a UTXO created in a previous event
genUtxoEvents :: Gen [StorableEvent UtxoHandle]
genUtxoEvents = genUtxoEvents' convertTxOutToUtxo

genUtxoEvents'
  :: (C.TxId -> C.TxIx -> C.TxOut C.CtxTx C.BabbageEra -> Utxo)
  -> Gen [StorableEvent UtxoHandle]
genUtxoEvents' txOutToUtxo = do
    chainPoints <- genChainPoints 1 3
    txIns <- Set.singleton <$> CGen.genTxIn
    snd <$> foldM f (txIns, []) chainPoints
  where
    f :: (Set C.TxIn, [StorableEvent UtxoHandle])
      -> C.ChainPoint
      -> Gen (Set C.TxIn, [StorableEvent UtxoHandle])
    f (utxoSet, utxoEvents) cp = do
        utxosAsTxInput <- nonEmptySubset utxoSet
        txBodyContent <- genTxBodyContentFromTxIns $ Set.toList utxosAsTxInput
        txBody <- either (fail . show) pure $ C.makeTransactionBody txBodyContent
        let txId = C.getTxId txBody
        let newUtxos = fmap (\(txIx, txOut) -> txOutToUtxo txId (C.TxIx txIx) txOut)
                $ zip [0..]
                $ C.txOuts txBodyContent
            newUtxoRefs = Set.fromList $ fmap (\Utxo { _txId, _txIx } -> C.TxIn _txId _txIx) newUtxos
            newUtxoEvent = UtxoEvent (Set.fromList newUtxos) utxosAsTxInput cp
        pure (Set.union newUtxoRefs $ Set.difference utxoSet utxosAsTxInput, utxoEvents ++ [newUtxoEvent])

convertTxOutToUtxo :: C.TxId -> C.TxIx -> C.TxOut C.CtxTx C.BabbageEra -> Utxo
convertTxOutToUtxo txId txIx (C.TxOut (C.AddressInEra _ addr) val txOutDatum refScript) =
    let (scriptDataHash, scriptData) =
            case txOutDatum of
              C.TxOutDatumNone       -> (Nothing, Nothing)
              C.TxOutDatumHash _ dh  -> (Just dh, Nothing)
              C.TxOutDatumInTx _ d   -> (Just $ C.hashScriptData d, Just d)
              C.TxOutDatumInline _ d -> (Just $ C.hashScriptData d, Just d)
        (scriptHash, script) =
            case refScript of
              C.ReferenceScriptNone -> (Nothing, Nothing)
              C.ReferenceScript _ scriptInAnyLang@(C.ScriptInAnyLang _ s) ->
                  (Just $ C.hashScript s, Just scriptInAnyLang)
     in Utxo
            (C.toAddressAny addr)
            txId
            txIx
            scriptData
            scriptDataHash
            (C.txOutValueToValue val)
            script
            scriptHash
-- | Override the Utxo address with ShelleyEra address
-- This is used to generate ShelleyEra Utxo events
utxoAddressOverride
  :: C.Address C.ShelleyAddr
  -> Utxo
  -> Utxo
utxoAddressOverride addr utxo =  utxo {_address=C.toAddressAny addr}

-- | Generate ShelleyEra Utxo Events
genShelleyEraUtxoEvents :: Gen [StorableEvent UtxoHandle]
genShelleyEraUtxoEvents = do
  addr <- CGen.genAddressShelley
  genUtxoEvents' (\_id _ix _tx ->
                    utxoAddressOverride addr $ convertTxOutToUtxo _id _ix _tx)

genTxBodyContentFromTxIns
    :: [C.TxIn]
    -> Gen (C.TxBodyContent C.BuildTx C.BabbageEra)
genTxBodyContentFromTxIns inputs = do
    txBodyContent <-
        emptyTxBodyContent (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra)
            <$> CGen.genProtocolParameters
    txOuts <- Gen.list (Range.linear 1 5) $ genTxOutTxContext C.BabbageEra
    pure $ txBodyContent
        { C.txIns = fmap (, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) inputs
        , C.txOuts = txOuts
        }


-- TODO Must be reworked following implementation in 'genUtxoEvents'.
genEventWithShelleyAddressAtChainPoint :: C.ChainPoint -> Gen (Utxo.StorableEvent Utxo.UtxoHandle)
genEventWithShelleyAddressAtChainPoint ueChainPoint = do
  txOutRefs <- Gen.list (Range.linear 1 5) CGen.genTxIn
  ueUtxos <- Set.fromList <$> forM txOutRefs genUtxo
  ueInputs <- Gen.set (Range.linear 1 5) CGen.genTxIn
  pure $ Utxo.UtxoEvent {..}

genUtxo :: C.TxIn -> Gen Utxo.Utxo
genUtxo txOutRef = CGen.genAddressShelley >>= genUtxo' txOutRef . C.toAddressAny

genUtxo' :: C.TxIn -> C.AddressAny -> Gen Utxo.Utxo
genUtxo' (C.TxIn _txId _txIx) _address = do
  sc <- CGen.genTxOutDatumHashTxContext C.BabbageEra
  let (_datum, _datumHash)  = Utxo.getScriptDataAndHash sc
  script            <- CGen.genReferenceScript C.ShelleyEra
  _value            <- CGen.genValueForTxOut
  let (_inlineScript, _inlineScriptHash)=  Utxo.getRefScriptAndHash script
  pure $ Utxo.Utxo {..}
