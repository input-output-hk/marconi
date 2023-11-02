{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | Utilities for integration tests using tools from the `cardano-node-emulator` project.
module Test.Integration where

import Cardano.Api.Extended.IPC qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Node.Emulator.Generators (knownAddresses, knownXPrvs)
import Cardano.Node.Emulator.Internal.Node.TimeSlot qualified as E.TimeSlot
import Cardano.Node.Socket.Emulator qualified as E
import Cardano.Node.Socket.Emulator.Types qualified as E.Types
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Default (def)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import Hedgehog qualified as H
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Gen qualified as H.Gen
import Hedgehog.Range qualified as Range
import Ledger.Test (testnet)
import Marconi.ChainIndex.Types (MarconiTrace)
import Marconi.Core qualified as Core
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusTx qualified
import System.Exit (exitFailure)
import Test.Helpers qualified as Helpers

-- TODO: PLT-8098 suppress logging in node emulator? generates lots of noise.
-- TODO: PLT-8098 keep getting could not connect with node-server.sock does not exist
-- TODO: PLT-8098 want to run only certain number of slots in emulator
-- TODO: PLT-8098 need to shut down the node emulator properly when the test is done.

{- Node emulator setup and helpers -}

-- | Start a testnet using the node emulator, within the @H.'Integration'@ context.
startTestnet
  :: FilePath
  -> H.Integration (C.LocalNodeConnectInfo C.CardanoMode, C.NetworkId, FilePath)
startTestnet tempAbsBasePath = do
  let socketPathAbs = tempAbsBasePath <> "/node-server.sock"
      -- TODO: PLT-8098 leftover from legacy code: "any other networkId doesn't work"
      networkId = testnet
      -- TODO: PLT-8098 revise after investigating
      -- In milliseconds, shorter than the default to make the tests go faster
      -- slotLength = 10
      slotLength = 100
      localNodeConnectInfo = C.mkLocalNodeConnectInfo networkId socketPathAbs
  liftIO $ startTestnetIntegration socketPathAbs slotLength networkId
  pure (localNodeConnectInfo, networkId, socketPathAbs)

-- | TODO: PLT-8098 added to start addressing a few issues in node start
startTestnetIntegration :: FilePath -> Integer -> C.NetworkId -> IO ()
startTestnetIntegration socketPath slotLength networkId = do
  now <- getPOSIXTime
  let
    config =
      def
        { E.Types.nscSlotConfig =
            def
              { E.TimeSlot.scSlotLength = slotLength
              , E.TimeSlot.scSlotZeroTime = E.TimeSlot.nominalDiffTimeToPOSIXTime now
              }
        , E.Types.nscSocketPath = socketPath
        , E.Types.nscNetworkId = networkId
        }
  E.main E.prettyTrace config

-- TODO: PLT-8098 check cardano-node-emulator to see if this already exists.

-- | Take the first @C.'ShelleyAddr'@ of @Cardano.Node.Emulator.Generators.'knownAddresses'@.
knownShelleyAddress :: Maybe (C.Address C.ShelleyAddr)
knownShelleyAddress = listToMaybe knownAddresses >>= getShelleyAddr
  where
    getShelleyAddr :: C.AddressInEra C.BabbageEra -> Maybe (C.Address C.ShelleyAddr)
    getShelleyAddr (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) addr) = Just addr
    getShelleyAddr _ = Nothing

-- | Take the first @C.'WitnessPaymentExtendedKey'@ from 'knownXPrvs'.
knownWitnessSigningKey :: Maybe C.ShelleyWitnessSigningKey
knownWitnessSigningKey = C.WitnessPaymentExtendedKey . C.PaymentExtendedSigningKey <$> listToMaybe knownXPrvs

-- | Unbounded validity range for transaction validation in the Babbage era.
unboundedValidityRange :: (C.TxValidityLowerBound C.BabbageEra, C.TxValidityUpperBound C.BabbageEra)
unboundedValidityRange = (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra)

{- Transaction operations -}

validateAndSubmitTx
  :: C.LocalNodeConnectInfo C.CardanoMode
  -> C.LedgerProtocolParameters C.BabbageEra
  -> C.NetworkId
  -> C.Address C.ShelleyAddr
  -> C.ShelleyWitnessSigningKey
  -> C.TxBodyContent C.BuildTx C.BabbageEra
  -> C.Lovelace
  -- ^ Lovelace before applying fees
  -> H.Integration ()
validateAndSubmitTx localNodeConnectInfo ledgerPP networkId address witnessSigningKey txbodyc lovelace = do
  txbodyValid <-
    H.leftFail $
      mkValidatedTxBodyWithFee ledgerPP networkId address txbodyc lovelace 1
  let keyWitness = C.makeShelleyKeyWitness txbodyValid witnessSigningKey
  signAndSubmitTx localNodeConnectInfo [keyWitness] txbodyValid

-- TODO: PLT-8098 did the legacy impl mix up parameter order for number of Byron/Shelley key witnesses?
-- compare with estimateTransactionFee in cardano-api, where Shelley witness number comes first.
-- If so, why does it still validate?

-- TODO: PLT-8098 make a note about how this is a shortcut for testing that does not do the
-- full iterative transaction fee calculation. apparantly there is a utility in cardano-node for
-- that.

{- | Validate the transaction, calculate the fee and build the transaction body with txOut
 - and fee adjustments applied.
-}
mkValidatedTxBodyWithFee
  :: C.LedgerProtocolParameters C.BabbageEra
  -> C.NetworkId
  -> C.Address C.ShelleyAddr
  -> C.TxBodyContent C.BuildTx C.BabbageEra
  -> C.Lovelace
  -- ^ Lovelace to fix in in TxOut, before applying fees
  -> Word
  -- ^ The number of Shelley key witnesses
  -> Either C.TxBodyError (C.TxBody C.BabbageEra)
mkValidatedTxBodyWithFee ledgerPP networkid address txbodyc lovelace nKeywitnesses =
  C.createAndValidateTransactionBody txbodyc
    >>= C.createAndValidateTransactionBody . updateTxWithFee txbodyc
  where
    -- TODO: PLT-8098 need to get address from txbody or somewhere
    -- Take the txMintValue and add the fee-adjusted lovace to build a single TxOut.
    rebuildTxOutWithFee
      :: C.TxBodyContent C.BuildTx C.BabbageEra -> C.Lovelace -> C.TxOut C.CtxTx C.BabbageEra
    rebuildTxOutWithFee txbodyc' fee =
      Helpers.mkTxOut address $
        C.lovelaceToValue (lovelace - fee) <> Helpers.getValueFromTxMintValue (C.txMintValue txbodyc')
    -- TODO: PLT-8098 this consistently produces a "fee too small" error. the fee is off by < 1k
    -- It uses cardano-ledger's transaction fee calculation function. does that differ from node
    -- emulators?
    -- calculateFee :: C.TxBody C.BabbageEra -> C.Lovelace
    -- calculateFee txbody' = C.evaluateTransactionFee (C.unLedgerProtocolParameters ledgerPP) txbody' nKeywitnesses 0
    updateTxWithFee
      :: C.TxBodyContent C.BuildTx C.BabbageEra
      -> C.TxBody C.BabbageEra
      -> C.TxBodyContent C.BuildTx C.BabbageEra
    updateTxWithFee txbodyc' txbody' =
      C.setTxOuts [rebuildTxOutWithFee txbodyc' fee] $
        C.setTxFee (C.TxFeeExplicit C.TxFeesExplicitInBabbageEra fee) txbodyc'
      where
        fee = calculateFee ledgerPP (length $ C.txIns txbodyc') 1 0 1 networkid txbody'

mkUnbalancedTxBodyContentFromTxMintValue
  :: (C.TxValidityLowerBound C.BabbageEra, C.TxValidityUpperBound C.BabbageEra)
  -> C.LedgerProtocolParameters C.BabbageEra
  -> C.Address C.ShelleyAddr
  -> [C.TxIn]
  -> C.TxMintValue C.BuildTx C.BabbageEra
  -> C.TxBodyContent C.BuildTx C.BabbageEra
mkUnbalancedTxBodyContentFromTxMintValue validityRange ledgerPP address txIns txMintValue =
  (Helpers.emptyTxBodyContent validityRange ledgerPP)
    { C.txIns = map (,C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) txIns
    , C.txOuts = [Helpers.mkTxOut address $ Helpers.getValueFromTxMintValue txMintValue]
    , C.txMintValue = txMintValue
    , C.txInsCollateral = C.TxInsCollateral C.CollateralInBabbageEra txIns
    }

-- | TODO: PLT-8098 blocks until you get one. use the MarconiTrace and provide a better message.
queryFirstResultWithRetry :: Int -> Word64 -> H.Integration [a] -> H.Integration a
queryFirstResultWithRetry ntries _ _
  | ntries <= 0 =
      liftIO $
        putStrLn "Max integration test query tries reached" >> exitFailure
queryFirstResultWithRetry ntries delay action = do
  res <- listToMaybe <$> action
  case res of
    Nothing ->
      liftIO (threadDelay $ fromIntegral delay)
        >> queryFirstResultWithRetry (ntries - 1) delay action
    Just x -> pure x

-- TODO: PLT-8098 confirm that txOuts are not used for fee calculation and modify this
-- and or mkValidatedTxBodyWithFee.

-- TODO: PLT-8098 taken from legacy. review and remove if not needed
calculateFee
  :: (C.IsShelleyBasedEra era)
  => C.LedgerProtocolParameters era
  -> Int
  -> Int
  -> Int
  -> Int
  -> C.NetworkId
  -> C.TxBody era
  -> C.Lovelace
calculateFee ledgerPP nInputs nOutputs nByronKeyWitnesses nShelleyKeyWitnesses networkId txBody =
  let apiPP = C.fromLedgerPParams C.shelleyBasedEra $ C.unLedgerProtocolParameters ledgerPP
   in C.estimateTransactionFee
        networkId
        (C.protocolParamTxFeeFixed apiPP)
        (C.protocolParamTxFeePerByte apiPP)
        (C.makeSignedTransaction [] txBody)
        nInputs
        nOutputs
        -- TODO: PLT-8098 note the swap
        nShelleyKeyWitnesses
        nByronKeyWitnesses

{- | TODO: PLT-8098 sign the validated tx body and submit to node emulator within H.Integration
context, and wait for it to be submitted over the protocol.
-}
signAndSubmitTx
  :: C.LocalNodeConnectInfo C.CardanoMode
  -> [C.KeyWitness C.BabbageEra]
  -> C.TxBody C.BabbageEra
  -> H.Integration ()
signAndSubmitTx info witnesses txbody = Helpers.submitAwaitTx info (tx, txbody)
  where
    tx = C.makeSignedTransaction witnesses txbody

-- TODO: PLT-8098 All below copied from legacy. Reevaluate if needed.
-- They should go in a different module.

-- | TODO: PLT-8098
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
