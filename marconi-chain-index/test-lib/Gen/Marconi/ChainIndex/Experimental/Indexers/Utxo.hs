{-# LANGUAGE DerivingStrategies #-}


-- |

module Gen.Marconi.ChainIndex.Experimental.Indexers.Utxo
  ( genUtxoEvents
    , genShelleyEraUtxoEvents
    , genShelleyEraUtxoEventsAtChainPoint
    , genUtxoEventsWithTxs
    , genTx
    , genTx'
    , genTxBodyContentFromTxIns
    , genTxBodyContentFromTxinsWihtPhase2Validation
  )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Marconi.ChainIndex.Experimental.Indexers.Utxo (Utxo (Utxo))
import Marconi.ChainIndex.Experimental.Indexers.Utxo qualified as Utxo
import Marconi.Core.Experiment qualified as Core

import Control.Lens (folded, (^..))
import Control.Monad (foldM, forM)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Gen.Marconi.ChainIndex.Mockchain (BlockHeader (BlockHeader), MockBlock (MockBlock), genMockchain,
                                         genTxBodyContentFromTxIns, genTxBodyContentFromTxinsWihtPhase2Validation)
import GHC.Generics (Generic)
import Hedgehog (Gen)
import Test.Gen.Cardano.Api.Typed qualified as CGen

--
--   * that any generated UTXO is unique
--   * for any spent tx output, there must be a UTXO created in a previous event
genUtxoEvents :: Gen (Core.ListIndexer Utxo.UtxoEvent)
genUtxoEvents = genUtxoEvents' convertTxOutToUtxo

genUtxoEvents'
  :: (C.TxIn -> C.TxOut C.CtxTx C.BabbageEra -> Utxo)
  -> Gen (Core.ListIndexer Utxo.UtxoEvent)
genUtxoEvents' txOutToUtxo = do
  timedEvents <- fmap fst <$> genUtxoEventsWithTxs' txOutToUtxo
  foldM (flip Core.index) Core.listIndexer timedEvents

-- | Generate ShelleyEra UtxoEvent
genShelleyEraUtxoEvents :: Gen (Core.TimedEvent Utxo.UtxoEvent)
genShelleyEraUtxoEvents = do
  events :: [Core.TimedEvent Utxo.UtxoEvent] <-  genUtxoEventsWithTxs <&> fmap fst
  utxoEvents' :: [Utxo.UtxoEvent] <-
    forM events (\(Core.TimedEvent _ uev@(Utxo.UtxoEvent utxos _)) -> do
                    utxos' <- forM (Set.toList utxos) (\u -> CGen.genAddressShelley <&> flip utxoAddressOverride u)
                    pure $ uev {Utxo._ueUtxos = Set.fromList utxos'})
  let
    cp :: C.ChainPoint
    cp = foldr max C.ChainPointAtGenesis (events ^.. folded . Core.point)
  pure $ Core.TimedEvent cp (fold utxoEvents')
  -- foldM (flip Core.index) Core.listIndexer shelleyEvents

genShelleyEraUtxoEventsAtChainPoint :: C.ChainPoint -> Gen (Core.TimedEvent Utxo.UtxoEvent)
genShelleyEraUtxoEventsAtChainPoint  cp = do
  events :: [Core.TimedEvent Utxo.UtxoEvent] <-
    genUtxoEventsWithTxs <&> fmap fst
  utxoEvent' :: [Utxo.UtxoEvent] <-
    forM events (\(Core.TimedEvent _ uev@(Utxo.UtxoEvent utxos _)) -> do
                    utxos' <- forM (Set.toList utxos) (\u -> CGen.genAddressShelley <&> flip utxoAddressOverride u)
                    pure $ uev{Utxo._ueUtxos = Set.fromList utxos'})
  pure $ Core.TimedEvent cp (fold utxoEvent')

genUtxoEventsWithTxs :: Gen [(Core.TimedEvent Utxo.UtxoEvent, MockBlock C.BabbageEra)]
genUtxoEventsWithTxs = genUtxoEventsWithTxs' convertTxOutToUtxo

genUtxoEventsWithTxs'
    :: (C.TxIn  -> C.TxOut C.CtxTx C.BabbageEra -> Utxo)
    -> Gen [(Core.TimedEvent Utxo.UtxoEvent, MockBlock C.BabbageEra)]
genUtxoEventsWithTxs' txOutToUtxo =
    fmap (\block -> (getTimedEventFromBlock block, block)) <$> genMockchain
  where
    getTimedEventFromBlock :: MockBlock C.BabbageEra -> Core.TimedEvent Utxo.UtxoEvent
    getTimedEventFromBlock (MockBlock (BlockHeader slotNo blockHeaderHash _blockNo) txs) =
        let
          (TxOutBalance utxos spentTxOuts) = foldMap txOutBalanceFromTx txs
          utxoMap = foldMap getUtxosFromTx txs
          resolvedUtxos :: Set Utxo.Utxo = Set.fromList
            $ mapMaybe (`Map.lookup` utxoMap)
            $ Set.toList utxos
          cp = C.ChainPoint slotNo blockHeaderHash
          spents :: Set Utxo.Spent = Set.map Utxo.Spent spentTxOuts
        in
          Core.TimedEvent  cp (Utxo.UtxoEvent resolvedUtxos spents)
    getUtxosFromTx :: C.Tx C.BabbageEra -> Map C.TxIn Utxo
    getUtxosFromTx (C.Tx txBody@(C.TxBody txBodyContent) _) =
        let
          txId = C.getTxId txBody
        in
          Map.fromList $ fmap (\(txIx, txOut) ->
                                 (C.TxIn txId (C.TxIx txIx), txOutToUtxo (C.TxIn txId (C.TxIx txIx)) txOut))
          $ zip [0..]
          $ C.txOuts txBodyContent

-- | The effect of a transaction (or a number of them) on the tx output set.
data TxOutBalance =
  TxOutBalance
    { _tobUnspent :: !(Set C.TxIn)
    -- ^ Outputs newly added by the transaction(s)
    , _tobSpent   :: !(Set C.TxIn)
    -- ^ Outputs spent by the transaction(s)
    }
    deriving stock (Eq, Show, Generic)

instance Semigroup TxOutBalance where
    tobL <> tobR =
        TxOutBalance
            { _tobUnspent = _tobUnspent tobR
                         <> (_tobUnspent tobL `Set.difference` _tobSpent tobR)
            , _tobSpent = _tobSpent tobL <> _tobSpent tobR
            }

instance Monoid TxOutBalance where
    mappend = (<>)
    mempty = TxOutBalance mempty mempty

txOutBalanceFromTx :: C.Tx era -> TxOutBalance
txOutBalanceFromTx (C.Tx txBody@(C.TxBody txBodyContent) _) =
    let txId = C.getTxId txBody
        txInputs = Set.fromList $ fst <$> C.txIns txBodyContent
        utxoRefs = Set.fromList
                 $ fmap (\(txIx, _) -> C.TxIn txId $ C.TxIx txIx)
                 $ zip [0..]
                 $ C.txOuts txBodyContent
     in TxOutBalance utxoRefs txInputs

convertTxOutToUtxo :: C.TxIn -> C.TxOut C.CtxTx C.BabbageEra -> Utxo
convertTxOutToUtxo txin (C.TxOut (C.AddressInEra _ addr) val txOutDatum refScript) =
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
              txin
              (C.getScriptData <$> scriptData)
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
utxoAddressOverride addr utxo = utxo { Utxo._utxoAddress = C.toAddressAny addr }
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
