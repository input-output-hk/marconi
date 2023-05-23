{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections      #-}

module Gen.Marconi.ChainIndex.Indexers.Utxo
    ( genUtxoEvents
    , genShelleyEraUtxoEvents
    , genUtxoEventsWithTxs
    , genTxBodyContentFromTxIns
    , genTxBodyContentFromTxinsWihtPhase2Validation
    , genTx
    , genTx'
    )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad (forM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Gen.Marconi.ChainIndex.Mockchain (BlockHeader (BlockHeader), MockBlock (MockBlock), genMockchain,
                                         genTxBodyContentFromTxIns, genTxBodyContentFromTxinsWihtPhase2Validation)
import GHC.Generics (Generic)
import Hedgehog (Gen)
import Marconi.ChainIndex.Indexers.Utxo (StorableEvent (UtxoEvent), Utxo (Utxo), UtxoHandle, _address)
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types (TxIndexInBlock)
import Test.Gen.Cardano.Api.Typed qualified as CGen

-- | Generates a list of UTXO events.
--
-- This generators has the following properties:
--
--   * that any generated UTXO is unique
--   * for any spent tx output, there must be a UTXO created in a previous event
genUtxoEvents :: Gen [StorableEvent UtxoHandle]
genUtxoEvents = fmap fst <$> genUtxoEventsWithTxs' convertTxOutToUtxo

-- | Generates a list of UTXO events, along with the list of transactions that generated each of
-- them.
--
-- This generators has the following properties:
--
--   * that any generated UTXO is unique
--   * for any spent tx output, there must be a UTXO created in a previous event
genUtxoEventsWithTxs :: Gen [(StorableEvent UtxoHandle, MockBlock C.BabbageEra)]
genUtxoEventsWithTxs = genUtxoEventsWithTxs' convertTxOutToUtxo

genUtxoEventsWithTxs'
    :: (C.TxId -> TxIndexInBlock -> C.TxIx -> C.TxOut C.CtxTx C.BabbageEra -> Utxo)
    -> Gen [(StorableEvent UtxoHandle, MockBlock C.BabbageEra)]
genUtxoEventsWithTxs' txOutToUtxo = do
    fmap (\block -> (getStorableEventFromBlock block, block)) <$> genMockchain
  where
    getStorableEventFromBlock :: MockBlock C.BabbageEra -> StorableEvent UtxoHandle
    getStorableEventFromBlock (MockBlock (BlockHeader slotNo blockHeaderHash _blockNo) txs) =
        let (TxOutBalance utxos spentTxOuts) = foldMap txOutBalanceFromTx txs
            utxoMap = foldMap getUtxosFromTx $ zip txs [0 ..]
            resolvedUtxos = Set.fromList
                          $ mapMaybe (`Map.lookup` utxoMap)
                          $ Set.toList utxos
         in UtxoEvent resolvedUtxos spentTxOuts (C.ChainPoint slotNo blockHeaderHash)

    getUtxosFromTx :: (C.Tx C.BabbageEra, TxIndexInBlock) -> Map C.TxIn Utxo
    getUtxosFromTx (C.Tx txBody@(C.TxBody txBodyContent) _, txIndexInBlock) =
        let txId = C.getTxId txBody
         in Map.fromList
                $ fmap (\(txIx, txOut) -> ( C.TxIn txId (C.TxIx txIx)
                                          , txOutToUtxo txId txIndexInBlock (C.TxIx txIx) txOut))
                $ zip [0..]
                $ C.txOuts txBodyContent

-- | The effect of a transaction (or a number of them) on the tx output set.
data TxOutBalance =
  TxOutBalance
    { _tobUnspent :: !(Set C.TxIn)
    -- ^ Outputs newly added by the transaction(s)
    , _tobSpent   :: !(Map C.TxIn C.TxId)
    -- ^ Outputs spent by the transaction(s)
    }
    deriving stock (Eq, Show, Generic)

instance Semigroup TxOutBalance where
    tobL <> tobR =
        TxOutBalance
            { _tobUnspent = _tobUnspent tobR
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
        utxoRefs = Set.fromList
                 $ fmap (\(txIx, _) -> C.TxIn txId $ C.TxIx txIx)
                 $ zip [0..]
                 $ C.txOuts txBodyContent
     in TxOutBalance utxoRefs txInputs

convertTxOutToUtxo :: C.TxId -> TxIndexInBlock -> C.TxIx -> C.TxOut C.CtxTx C.BabbageEra -> Utxo
convertTxOutToUtxo txid txIndexInBlock txix (C.TxOut (C.AddressInEra _ addr) val txOutDatum refScript) =
    let (scriptDataHash, scriptData) =
            case txOutDatum of
              C.TxOutDatumNone       -> (Nothing, Nothing)
              C.TxOutDatumHash _ dh  -> (Just dh, Nothing)
              C.TxOutDatumInTx _ d   -> (Just $ C.hashScriptDataBytes d, Just d)
              C.TxOutDatumInline _ d -> (Just $ C.hashScriptDataBytes d, Just d)
        (scriptHash, script) =
            case refScript of
              C.ReferenceScriptNone -> (Nothing, Nothing)
              C.ReferenceScript _ scriptInAnyLang@(C.ScriptInAnyLang _ s) ->
                  (Just $ C.hashScript s, Just scriptInAnyLang)
     in Utxo
            ( C.toAddressAny addr)
              (C.TxIn txid txix)
              (C.getScriptData <$> scriptData)
              scriptDataHash
              (C.txOutValueToValue val)
              script
              scriptHash
              txIndexInBlock

-- | Override the Utxo address with ShelleyEra address
-- This is used to generate ShelleyEra Utxo events
utxoAddressOverride
  :: C.Address C.ShelleyAddr
  -> Utxo
  -> Utxo
utxoAddressOverride addr utxo = utxo { _address = C.toAddressAny addr }

-- | Generate ShelleyEra Utxo Events
genShelleyEraUtxoEvents :: Gen [StorableEvent Utxo.UtxoHandle]
genShelleyEraUtxoEvents = do
  events <- genUtxoEvents
  forM events (\e -> do
                  let utxos = Utxo.ueUtxos e
                  us <- forM (Set.toList utxos) (\u -> do
                                 a <- CGen.genAddressShelley
                                 pure $ utxoAddressOverride a u)
                  pure e {Utxo.ueUtxos=Set.fromList us}
              )
-- | Generate Cardano TX
-- This generator may be used phase2-validation test cases
genTx :: Gen (C.Tx C.BabbageEra)
genTx = genTx' genTxBodyContentFromTxinsWihtPhase2Validation

-- | Generate Cardano TX
-- Given a TxBodyContent generator, generate a Cardano TX
genTx'
  :: ([C.TxIn] -> Gen (C.TxBodyContent C.BuildTx C.BabbageEra))
  -> Gen (C.Tx C.BabbageEra)
genTx' gen = do
  txIn <- CGen.genTxIn
  txBodyContent  <- gen [txIn]
  txBody <- either (fail . show) pure $ C.createAndValidateTransactionBody txBodyContent
  pure $ C.makeSignedTransaction [] txBody
