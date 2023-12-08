{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | Generators related to 'MintTokenEvent' indexer and queries.
module Test.Gen.Marconi.Cardano.Indexers.MintTokenEvent where

import Cardano.Api.Shelley qualified as C
import Control.Monad (replicateM)
import Data.Bifunctor (first)
import Data.List.NonEmpty (nonEmpty)
import Data.Map qualified as Map
import Hedgehog qualified as H
import Hedgehog.Gen qualified as H.Gen
import Hedgehog.Range qualified as Range
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance (WithDistance))
import Marconi.Cardano.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.Core qualified as Core
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusTx qualified
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Mockchain

getTimedMintTokentEventsWithDistance
  :: Mockchain.MockchainWithDistance era
  -> [Core.Timed C.ChainPoint (WithDistance (Maybe MintTokenEvent.MintTokenBlockEvents))]
getTimedMintTokentEventsWithDistance = map op
  where
    op (WithDistance d block) = WithDistance d <$> getMintTokenEvents block

getTimedMintTokenEvents
  :: Mockchain.Mockchain era
  -> [Core.Timed C.ChainPoint (Maybe MintTokenEvent.MintTokenBlockEvents)]
getTimedMintTokenEvents = map getMintTokenEvents

getMintTokenEvents
  :: Mockchain.MockBlock era
  -> Core.Timed C.ChainPoint (Maybe MintTokenEvent.MintTokenBlockEvents)
getMintTokenEvents (Mockchain.MockBlock (C.BlockHeader slotNo blockHeaderHash blockNo) txs) =
  Core.Timed
    (C.ChainPoint slotNo blockHeaderHash)
    $ fmap MintTokenEvent.MintTokenBlockEvents
    $ nonEmpty
    $ concat
    $ zipWith
      (\i -> MintTokenEvent.extractEventsFromTx blockNo i . Mockchain.getTxBody)
      [0 ..]
      txs

mkMintValue
  :: MintingPolicy
  -> [(C.AssetName, C.Quantity)]
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint C.BabbageEra, C.Value)
mkMintValue policy policyAssets = (policyId, policyWitness, mintedValues)
  where
    serialisedPolicyScript :: C.PlutusScript C.PlutusScriptV1
    serialisedPolicyScript = C.PlutusScriptSerialised $ PlutusV2.serialiseCompiledCode policy

    policyId :: C.PolicyId
    policyId = C.scriptPolicyId $ C.PlutusScript C.PlutusScriptV1 serialisedPolicyScript :: C.PolicyId

    executionUnits :: C.ExecutionUnits
    executionUnits = C.ExecutionUnits{C.executionSteps = 300000, C.executionMemory = 1000}
    redeemer :: C.ScriptRedeemer
    redeemer = C.unsafeHashableScriptData $ C.fromPlutusData $ PlutusV1.toData ()
    policyWitness :: C.ScriptWitness C.WitCtxMint C.BabbageEra
    policyWitness =
      C.PlutusScriptWitness
        C.PlutusScriptV1InBabbage
        C.PlutusScriptV1
        (C.PScript serialisedPolicyScript)
        C.NoScriptDatumForMint
        redeemer
        executionUnits

    mintedValues :: C.Value
    mintedValues = C.valueFromList $ map (first (C.AssetId policyId)) policyAssets

type MintingPolicy = PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())

commonMintingPolicy :: MintingPolicy
commonMintingPolicy = $$(PlutusTx.compile [||\_ _ -> ()||])

-- | Get the @Cardano.Api.TxBody.'Value'@ from the @Cardano.Api.TxBody.'TxMintValue'@.
getValueFromTxMintValue :: C.TxMintValue build era -> C.Value
getValueFromTxMintValue (C.TxMintValue _ v _) = v
getValueFromTxMintValue _ = mempty

{- GENERATORS -}

-- | Generate a MockchainWithInfoAndDistance guaranteeing mint/burn events.
genMockchainWithMintsWithInfoAndDistance
  :: H.Gen (Mockchain.MockchainWithInfoAndDistance C.BabbageEra)
genMockchainWithMintsWithInfoAndDistance = Mockchain.attachDistanceToMockChainWithInfo <$> genMockchainWithMintsWithInfo

-- | Generate a MockchainWithInfo guaranteeing mint/burn events.
genMockchainWithMintsWithInfo :: H.Gen (Mockchain.MockchainWithInfo C.BabbageEra)
genMockchainWithMintsWithInfo = genMockchainWithMints >>= Mockchain.genMockchainWithInfoFromMockchain

-- | Generate a Mockchain guaranteeing mint/burn events.
genMockchainWithMints :: H.Gen (Mockchain.Mockchain C.BabbageEra)
genMockchainWithMints = Mockchain.genMockchainWithTxBodyGen genTxBodyContentFromTxInsWithMints

{- | Version of @Mockchain.'genTxBodyContentFromTxIns'@ that always generates mint/burn
events instead of the default of not generating any.
-}
genTxBodyContentFromTxInsWithMints :: [C.TxIn] -> H.Gen (C.TxBodyContent C.BuildTx C.BabbageEra)
genTxBodyContentFromTxInsWithMints txIns =
  C.setTxMintValue
    <$> genTxMintValue
    <*> Mockchain.genTxBodyContentFromTxIns txIns

genTxMintValue :: H.Gen (C.TxMintValue C.BuildTx C.BabbageEra)
genTxMintValue = genTxMintValueRange 1 100

genTxMintValueRange :: Integer -> Integer -> H.Gen (C.TxMintValue C.BuildTx C.BabbageEra)
genTxMintValueRange min' max' = do
  n :: Int <- H.Gen.integral (Range.constant 1 5)
  policyAssets <- replicateM n genAsset
  let (policyId, policyWitness, mintedValues) = mkMintValue commonMintingPolicy policyAssets
      buildInfo = C.BuildTxWith $ Map.singleton policyId policyWitness
  pure $ C.TxMintValue C.MultiAssetInBabbageEra mintedValues buildInfo
  where
    genAsset :: H.Gen (C.AssetName, C.Quantity)
    genAsset = (,) <$> genAssetName <*> genQuantity
    genQuantity = C.Quantity <$> H.Gen.integral (Range.constant min' max')

genAssetName :: H.Gen C.AssetName
genAssetName = C.AssetName <$> H.Gen.bytes (Range.constant 1 5)
