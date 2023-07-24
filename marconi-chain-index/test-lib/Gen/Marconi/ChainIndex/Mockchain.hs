{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Marconi.ChainIndex.Mockchain (
  Mockchain,
  C.BlockHeader (..),
  MockBlock (..),
  genMockchain,
  genMockchainWithTxBodyGen,
  genTxBodyContentFromTxIns,
  genTxBodyContentFromTxInsWithPhase2Validation,
  DatumLocation (..),
  getDatumHashFromDatumLocation,
  getDatumFromDatumLocation,
  genAddressesWithDatum,
  genTxsWithAddresses,
  genTxBodyWithAddresses,
  genTxBodyContentWithAddresses,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad (foldM, forM)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Gen.Marconi.ChainIndex.Types (genHashBlockHeader, genTxOutTxContext, nonEmptySubset)
import Gen.Marconi.ChainIndex.Types qualified as Gen
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helpers (emptyTxBodyContent)
import Test.Gen.Cardano.Api.Typed qualified as CGen

type Mockchain era = [MockBlock era]

deriving stock instance Show C.BlockHeader

data MockBlock era = MockBlock
  { mockBlockChainPoint :: !C.BlockHeader
  , mockBlockTxs :: ![C.Tx era]
  }
  deriving (Show)

-- | Generate a Mockchain
genMockchain :: Gen (Mockchain C.BabbageEra)
genMockchain = genMockchainWithTxBodyGen genTxBodyContentFromTxIns

-- | Generate a Mockchain
genMockchainWithTxBodyGen
  :: ([C.TxIn] -> Gen (C.TxBodyContent C.BuildTx C.BabbageEra)) -- function that know how generate TxBodyContent
  -> Gen (Mockchain C.BabbageEra)
genMockchainWithTxBodyGen genTxBody = do
  maxSlots <- Gen.word64 (Range.linear 2 6)
  blockHeaderHash <- genHashBlockHeader
  let blockHeaders =
        fmap
          (\s -> C.BlockHeader (C.SlotNo s) blockHeaderHash (C.BlockNo s))
          [1 .. maxSlots]
  txIns <- Set.singleton <$> CGen.genTxIn
  snd <$> foldM f (txIns, []) blockHeaders
  where
    f
      :: (Set C.TxIn, Mockchain C.BabbageEra)
      -> C.BlockHeader
      -> Gen (Set C.TxIn, Mockchain C.BabbageEra)
    f (utxoSet, mockchain) bh = do
      utxosAsTxInput <- nonEmptySubset utxoSet
      txBodyContent <- genTxBody $ Set.toList utxosAsTxInput
      txBody <- either (fail . show) pure $ C.createAndValidateTransactionBody txBodyContent
      let newTx = C.makeSignedTransaction [] txBody
      let txId = C.getTxId txBody
      let newUtxoRefs =
            Set.fromList $
              fmap (\(txIx, _) -> C.TxIn txId (C.TxIx txIx)) $
                zip [0 ..] $
                  C.txOuts txBodyContent
      pure
        ( Set.union newUtxoRefs $ Set.difference utxoSet utxosAsTxInput
        , mockchain ++ [MockBlock bh [newTx]]
        )

genTxBodyContentFromTxIns
  :: [C.TxIn] -> Gen (C.TxBodyContent C.BuildTx C.BabbageEra)
genTxBodyContentFromTxIns inputs = do
  initialPP <- CGen.genProtocolParameters C.BabbageEra
  let pp =
        initialPP
          { C.protocolParamUTxOCostPerByte = Just 1
          , C.protocolParamPrices = Just $ C.ExecutionUnitPrices 1 1
          , C.protocolParamMaxTxExUnits = Just $ C.ExecutionUnits 1 1
          , C.protocolParamMaxBlockExUnits = Just $ C.ExecutionUnits 1 1
          , C.protocolParamMaxValueSize = Just 1
          , C.protocolParamCollateralPercent = Just 1
          , C.protocolParamMaxCollateralInputs = Just 1
          }
      txBodyContent =
        emptyTxBodyContent
          (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra)
          pp

  txOuts <- Gen.list (Range.linear 1 5) $ genTxOutTxContext C.BabbageEra
  pure $
    txBodyContent
      { C.txIns = fmap (,C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) inputs
      , C.txOuts = txOuts
      }

{- | Generates TxBodyContent that may or may not have Collateral
 This generator is use for phase-2 validation test cases
-}
genTxBodyContentFromTxInsWithPhase2Validation
  :: [C.TxIn]
  -> Gen (C.TxBodyContent C.BuildTx C.BabbageEra)
genTxBodyContentFromTxInsWithPhase2Validation inputs = do
  initialTxBodyContent <- genTxBodyContentFromTxIns inputs
  txInsCollateral <- genTxInsCollateral C.BabbageEra
  txReturnCollateral <- genTxReturnCollateral C.BabbageEra
  txScriptValidity <- genTxScriptValidity C.BabbageEra
  pure $
    initialTxBodyContent
      { C.txInsCollateral = txInsCollateral
      , C.txReturnCollateral = txReturnCollateral
      , C.txScriptValidity = txScriptValidity
      }

data DatumLocation
  = NoDatumLocation
  | TxOutDatumHashLocation (C.Hash C.ScriptData) C.HashableScriptData
  | TxOutDatumInTxLocation (C.Hash C.ScriptData) C.HashableScriptData
  | TxOutDatumInlineLocation (C.Hash C.ScriptData) C.HashableScriptData
  | PlutusScriptDatumLocation (C.Hash C.ScriptData) C.HashableScriptData
  deriving (Show)

getDatumHashFromDatumLocation :: DatumLocation -> Maybe (C.Hash C.ScriptData)
getDatumHashFromDatumLocation NoDatumLocation = Nothing
getDatumHashFromDatumLocation (TxOutDatumHashLocation dh _) = Just dh
getDatumHashFromDatumLocation (TxOutDatumInTxLocation dh _) = Just dh
getDatumHashFromDatumLocation (TxOutDatumInlineLocation dh _) = Just dh
getDatumHashFromDatumLocation (PlutusScriptDatumLocation dh _) = Just dh

getDatumFromDatumLocation :: DatumLocation -> Maybe C.HashableScriptData
getDatumFromDatumLocation NoDatumLocation = Nothing
getDatumFromDatumLocation (TxOutDatumHashLocation _ d) = Just d
getDatumFromDatumLocation (TxOutDatumInTxLocation _ d) = Just d
getDatumFromDatumLocation (TxOutDatumInlineLocation _ d) = Just d
getDatumFromDatumLocation (PlutusScriptDatumLocation _ d) = Just d

genAddressesWithDatum :: Gen DatumLocation -> Gen [(C.AddressInEra C.BabbageEra, DatumLocation)]
genAddressesWithDatum genDatumLocation = do
  addresses <- Gen.list (Range.linear 1 3) $ CGen.genAddressInEra C.BabbageEra
  -- We do 'addresses ++ addresses' to generate duplicate addresses so that we can test that we
  -- correctly index different datums for the same address.
  forM (addresses ++ addresses) $ \addr -> do
    datLocation <- genDatumLocation
    pure (addr, datLocation)

genTxsWithAddresses :: [(C.AddressInEra C.BabbageEra, DatumLocation)] -> Gen [C.Tx C.BabbageEra]
genTxsWithAddresses addrsWithDatum =
  Gen.list (Range.linear 1 3) $
    C.makeSignedTransaction [] <$> genTxBodyWithAddresses addrsWithDatum

genTxBodyWithAddresses
  :: [(C.AddressInEra C.BabbageEra, DatumLocation)] -> Gen (C.TxBody C.BabbageEra)
genTxBodyWithAddresses addresses = do
  res <- C.createAndValidateTransactionBody <$> genTxBodyContentWithAddresses addresses
  case res of
    Left err -> fail (C.displayError err)
    Right txBody -> pure txBody

genTxBodyContentWithAddresses
  :: [(C.AddressInEra C.BabbageEra, DatumLocation)]
  -> Gen (C.TxBodyContent C.BuildTx C.BabbageEra)
genTxBodyContentWithAddresses addressesDatumLocation = do
  exUnits <- CGen.genExecutionUnits
  scriptTxIns <- fmap catMaybes <$> forM addressesDatumLocation $
    \case
      (_, PlutusScriptDatumLocation _ d) -> do
        txIn <- CGen.genTxIn
        let witness =
              C.ScriptWitness C.ScriptWitnessForSpending $
                C.PlutusScriptWitness
                  C.PlutusScriptV1InBabbage
                  C.PlutusScriptV1
                  (C.PScript $ C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn)
                  (C.ScriptDatumForTxIn d)
                  d
                  exUnits
        pure $ Just (txIn, C.BuildTxWith witness)
      (_, _) -> pure Nothing

  txOuts <- forM addressesDatumLocation $
    \case
      (addr, NoDatumLocation) -> do
        let txOutGen =
              C.TxOut addr
                <$> CGen.genTxOutValue C.BabbageEra
                <*> pure C.TxOutDatumNone
                <*> pure C.ReferenceScriptNone
        Gen.list (Range.linear 1 2) txOutGen
      (addr, TxOutDatumHashLocation hd _) -> do
        let txOutGen =
              C.TxOut addr
                <$> CGen.genTxOutValue C.BabbageEra
                <*> pure (C.TxOutDatumHash C.ScriptDataInBabbageEra hd)
                <*> pure C.ReferenceScriptNone
        Gen.list (Range.linear 1 2) txOutGen
      (addr, TxOutDatumInTxLocation _ d) -> do
        let txOutGen =
              C.TxOut addr
                <$> CGen.genTxOutValue C.BabbageEra
                <*> pure (C.TxOutDatumInTx C.ScriptDataInBabbageEra d)
                <*> pure C.ReferenceScriptNone
        Gen.list (Range.linear 1 2) txOutGen
      (addr, TxOutDatumInlineLocation _ d) -> do
        let txOutGen =
              C.TxOut addr
                <$> CGen.genTxOutValue C.BabbageEra
                <*> pure (C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra d)
                <*> pure C.ReferenceScriptNone
        Gen.list (Range.linear 1 2) txOutGen
      (_, _) -> pure []

  txBody <- Gen.genTxBodyContentForPlutusScripts
  pure $
    txBody
      { C.txIns = C.txIns txBody <> scriptTxIns
      , C.txOuts = concat txOuts
      }

-------------------------------------------------------------------------------------
----- The following are whole sale copy/paste from https://github.com/input-output-hk/cardano-node/blob/master/cardano-api/gen/Test/Gen/Cardano/Api/Typed.hs
----- TODO remove when we upgrade to newer version of cardano-api as the generators below are exposed in later version of cardano-api
-------------------------------------------------------------------------------------
genTxInsCollateral :: C.CardanoEra era -> Gen (C.TxInsCollateral era)
genTxInsCollateral era =
  case C.collateralSupportedInEra era of
    Nothing -> pure C.TxInsCollateralNone
    Just supported ->
      Gen.choice
        [ pure C.TxInsCollateralNone
        , C.TxInsCollateral supported <$> Gen.list (Range.linear 0 10) CGen.genTxIn
        ]
genTxReturnCollateral :: C.CardanoEra era -> Gen (C.TxReturnCollateral C.CtxTx era)
genTxReturnCollateral era =
  case C.totalAndReturnCollateralSupportedInEra era of
    Nothing -> return C.TxReturnCollateralNone
    Just supp ->
      C.TxReturnCollateral supp <$> genTxOutTxContext era

genTxScriptValidity :: C.CardanoEra era -> Gen (C.TxScriptValidity era)
genTxScriptValidity era = case C.txScriptValiditySupportedInCardanoEra era of
  Nothing -> pure C.TxScriptValidityNone
  Just witness -> C.TxScriptValidity witness <$> genScriptValidity

genScriptValidity :: Gen C.ScriptValidity
genScriptValidity = Gen.element [C.ScriptInvalid, C.ScriptValid]
