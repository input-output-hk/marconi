{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.Contract.Tx.Constraints.MustReferenceOutput(tests) where

import Control.Lens (At (at), filtered, has, makeClassyPrisms, non, (??), (^.))
import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Cardano.Api qualified as C
import Cardano.Node.Emulator.Params qualified as Params
import Data.Default (Default (def))
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as Text
import Data.Void (Void)
import Ledger qualified as L
import Ledger.Constraints qualified as Cons
import Ledger.Constraints.OnChain.V1 qualified as Cons.V1
import Ledger.Constraints.OnChain.V2 qualified as Cons.V2
import Ledger.Test (asDatum, asRedeemer, someCardanoAddress, someValidatorHash)
import Ledger.Tx qualified as Tx
import Ledger.Tx.Constraints qualified as Tx.Cons
import Ledger.Tx.Constraints qualified as TxCons
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value.CardanoAPI qualified as Value
import Plutus.ChainIndex.Emulator (diskState)
import Plutus.ChainIndex.Emulator.DiskState (addressMap, unCredentialMap)
import Plutus.Contract as Con
import Plutus.Contract.Test (assertEvaluationError, assertFailedTransaction, assertValidatedTransactionCount,
                             checkPredicateOptions, defaultCheckOptions, emulatorConfig, w1)
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Scripts qualified as PSU
import Plutus.Script.Utils.Typed (Any)
import Plutus.Script.Utils.V2.Address qualified as PSU.V2
import Plutus.Script.Utils.V2.Scripts qualified as PSU.V2
import Plutus.Trace qualified as Trace
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Spec.Contract.Error (cardanoLedgerErrorContaining)
import Wallet.Emulator.Wallet (WalletState, chainIndexEmulatorState)
import Wallet.Emulator.Wallet qualified as Wallet

makeClassyPrisms ''L.ScriptError

tests :: TestTree
tests =
    testGroup "MustReferenceOutput"
      [ testGroup "ledger constraints" $ [v1Tests, v2Tests] ?? submitTxConstraintsWith
      , testGroup "cardano constraints" $ [v1Tests, v2Tests] ?? submitCardanoTxConstraintsWith
      ]

v1Tests :: SubmitTx -> TestTree
v1Tests sub = testGroup "Plutus V1" $
   [ v1FeaturesNotAvailableTests
   ] ?? sub ?? PSU.PlutusV1

v2Tests :: SubmitTx -> TestTree
v2Tests sub = testGroup "Plutus V2" $
  [ v2FeaturesTests
  ] ?? sub ?? PSU.PlutusV2

v1FeaturesNotAvailableTests :: SubmitTx -> PSU.Language -> TestTree
v1FeaturesNotAvailableTests sub t = testGroup "Plutus V1 features" $
    [ ledgerValidationtErrorWhenUsingV1Script
    , phase2FailureWhenUsingV1Script
    ] ?? sub ?? t

v2FeaturesTests :: SubmitTx -> PSU.Language -> TestTree
v2FeaturesTests sub t = testGroup "Plutus V2 features" $
    [ mustReferenceOutputWithSinglePubkeyOutput
    , mustReferenceOutputWithMultiplePubkeyOutputs
    , mustReferenceOutputWithSingleScriptOutput
    , ledgerValidationErrorWhenReferencingNonExistingTxo
    , phase2FailureWhenUsingV2Script
    ] ?? sub ?? t

tknValue :: PSU.Language -> C.Value
tknValue l = Value.singleton (Value.policyId $ getVersionedScript MustReferenceOutputPolicy l) "mint-me" 1

nonExistentTxoRef :: Tx.TxOutRef
nonExistentTxoRef =
    Tx.TxOutRef "bcf4064aed337a2d7d481d046b03c43457a020df7ae6ff182d1387979a30abcd" 123

defTrace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
defTrace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 1

-- | Contract to a single transaction with mustSpendScriptOutputs offchain
-- constraint and mint with policy using matching onchain constraint.
mustReferenceOutputContract
    :: SubmitTx
    -> PSU.Language
    -> [Tx.TxOutRef]
    -> [Tx.TxOutRef]
    -> Contract () Empty ContractError ()
mustReferenceOutputContract submitTxFromConstraints l offChainTxoRefs onChainTxoRefs = do
    lookups <- traverse (\ref -> fmap (ref ,) <$> unspentTxOutFromRef ref) offChainTxoRefs
    let lookups1 = Cons.mintingPolicy (getVersionedScript MustReferenceOutputPolicy l)
            <> Cons.unspentOutputs (M.fromList $ catMaybes lookups)
        tx1 = mconcat mustReferenceOutputs
           <> Cons.mustMintValueWithRedeemer (asRedeemer onChainTxoRefs) (Value.fromCardanoValue $ tknValue l)
    ledgerTx1 <- submitTxFromConstraints lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1
        where
        mustReferenceOutputs :: [Cons.TxConstraints [Tx.TxOutRef] ()]
        mustReferenceOutputs = Cons.mustReferenceOutput <$> offChainTxoRefs

txoRefsFromWalletState :: WalletState -> Set Tx.TxOutRef
txoRefsFromWalletState w = let
  pkCred = L.cardanoAddressCredential $ Wallet.ownAddress w
  in w ^. chainIndexEmulatorState . diskState . addressMap . unCredentialMap . at pkCred . non mempty

-- | Ledger validation error occurs when attempting use of offchain mustReferenceOutput
--   constraint with V1 script
ledgerValidationtErrorWhenUsingV1Script :: SubmitTx -> PSU.Language -> TestTree
ledgerValidationtErrorWhenUsingV1Script submitTxFromConstraints l =
    let contract = mustReferenceOutputContract submitTxFromConstraints l
                    [nonExistentTxoRef] [nonExistentTxoRef]

    in checkPredicateOptions defaultCheckOptions
    ("Ledger validation error occurs when attempting use of offchain mustReferenceOutput " ++
     "constraint with V1 script")
    (assertFailedTransaction (const $ cardanoLedgerErrorContaining "ReferenceInputsNotSupported"))
    (void $ defTrace contract)

-- | Phase-2 validation error occurs when attempting to use onchain mustReferenceOutput
-- constraint with V1 script
phase2FailureWhenUsingV1Script :: SubmitTx -> PSU.Language -> TestTree
phase2FailureWhenUsingV1Script = phase2FailureWithMustReferenceOutput
    ("Phase-2 validation error occurs when attempting to use onchain mustReferenceOutput " ++
    "constraint with V1 script")

-- | Phase-2 validation error occurs when using onchain mustReferenceOutput
-- constraint with V2 script and and reference input is not in txbody
phase2FailureWhenUsingV2Script :: SubmitTx -> PSU.Language -> TestTree
phase2FailureWhenUsingV2Script = phase2FailureWithMustReferenceOutput
    ("Phase-2 validation error occurs when using onchain mustReferenceOutput" ++
    "constraint with V2 script and and reference input is not in txbody")

phase2FailureWithMustReferenceOutput :: String -> SubmitTx -> PSU.Language -> TestTree
phase2FailureWithMustReferenceOutput testDescription submitTxFromConstraints l =
    let contractWithoutOffchainConstraint = do
            let lookups1 = Cons.mintingPolicy (getVersionedScript MustReferenceOutputPolicy l)
                tx1 = Cons.mustMintValueWithRedeemer (asRedeemer [nonExistentTxoRef]) (Value.fromCardanoValue $ tknValue l)
            ledgerTx1 <- submitTxFromConstraints lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicateOptions defaultCheckOptions
    testDescription
    (assertEvaluationError "Lf")
    (void $ defTrace contractWithoutOffchainConstraint)

-- | Valid scenario using offchain and onchain constraint
-- mustReferenceOutput once for a single pubkey output.
mustReferenceOutputWithSinglePubkeyOutput :: SubmitTx -> PSU.Language -> TestTree
mustReferenceOutputWithSinglePubkeyOutput submitTxFromConstraints l =
    let trace = do
            w1State <- Trace.agentState w1
            let w1TxoRefs = txoRefsFromWalletState w1State
                w1MiddleTxoRef = [S.elemAt (length w1TxoRefs `div` 2) w1TxoRefs]
                contract =
                    mustReferenceOutputContract submitTxFromConstraints l
                        w1MiddleTxoRef w1MiddleTxoRef
            void $ Trace.activateContractWallet w1 contract
            void $ Trace.waitNSlots 1

    in checkPredicateOptions defaultCheckOptions
    ("Valid scenario using offchain and onchain constraint " ++
    "mustReferenceOutput once for a single pubkey output")
    (assertValidatedTransactionCount 1)
    (void trace)

-- | Valid scenario using offchain and onchain constraint
-- mustReferenceOutput once for multiple pubkey outputs.
mustReferenceOutputWithMultiplePubkeyOutputs :: SubmitTx -> PSU.Language -> TestTree
mustReferenceOutputWithMultiplePubkeyOutputs submitTxFromConstraints l =
    let trace = do
            w1State <- Trace.agentState w1
            let w1TxoRefs = S.toList $ txoRefsFromWalletState w1State
                contract =
                    mustReferenceOutputContract submitTxFromConstraints l w1TxoRefs w1TxoRefs
            void $ Trace.activateContractWallet w1 contract
            void $ Trace.waitNSlots 1

    in checkPredicateOptions defaultCheckOptions
    ("Valid scenario using offchain and onchain constraint " ++
    "mustReferenceOutput once for multiple pubkey outputs.")
    (assertValidatedTransactionCount 1)
    (void trace)

-- | Valid scenario using offchain and onchain constraint
-- mustReferenceOutput once for a single script output.
mustReferenceOutputWithSingleScriptOutput :: SubmitTx -> PSU.Language -> TestTree
mustReferenceOutputWithSingleScriptOutput submitTxFromConstraints l =
    let contractWithScriptOutput = do
            params <- getParams
            let tx1 = Cons.mustPayToOtherScriptWithDatumHash someValidatorHash
                      (asDatum $ PlutusTx.toBuiltinData ()) (Ada.lovelaceValueOf 2_000_000)
            ledgerTx1 <- submitTx tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            scriptUtxos <- utxosAt $ someCardanoAddress $ Params.pNetworkId params
            let scriptUtxo = head $ M.keys scriptUtxos
                lookups2 = Cons.mintingPolicy (getVersionedScript MustReferenceOutputPolicy l)
                        <> Cons.unspentOutputs scriptUtxos
                tx2 = Cons.mustReferenceOutput scriptUtxo
                   <> Cons.mustMintValueWithRedeemer (asRedeemer [scriptUtxo]) (Value.fromCardanoValue $ tknValue l)
            ledgerTx2 <- submitTxFromConstraints lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

    in checkPredicateOptions defaultCheckOptions
    ("Valid scenario using offchain and onchain constraint " ++
    "mustReferenceOutput once for a single script output")
    (assertValidatedTransactionCount 2)
    (void $ defTrace contractWithScriptOutput)

-- | Ledger validation error occurs when attempting use of offchain mustReferenceOutput
--   constraint with a txo that doesn't exist
ledgerValidationErrorWhenReferencingNonExistingTxo :: SubmitTx -> PSU.Language -> TestTree
ledgerValidationErrorWhenReferencingNonExistingTxo submitTxFromConstraints l =
    let contract = mustReferenceOutputContract submitTxFromConstraints l
                    [nonExistentTxoRef] [nonExistentTxoRef]

    in checkPredicateOptions defaultCheckOptions
    ("Ledger validation error occurs when using offchain mustReferenceOutput " ++
     "constraint with a txo that doesn't exist")
    (assertFailedTransaction (const $ has
        $ L._CardanoLedgerValidationError . filtered (Text.isInfixOf "TranslationLogicMissingInput"))
    )
    (void $ defTrace contract)

data UnitTest
instance Scripts.ValidatorTypes UnitTest where
    type instance DatumType UnitTest = ()
    type instance RedeemerType UnitTest = [Tx.TxOutRef]

mkMustReferenceOutputPolicy :: (Cons.TxConstraints () () -> sc -> Bool)
                            -> [Tx.TxOutRef ] -> sc -> Bool
mkMustReferenceOutputPolicy checkScriptContext txOutRefs =
    checkScriptContext (P.mconcat mustReferenceOutputs)
    where
        mustReferenceOutputs = Cons.mustReferenceOutput P.<$> txOutRefs

mustReferenceOutputPolicyV1 :: L.MintingPolicy
mustReferenceOutputPolicyV1 = L.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustPayToOtherScriptPolicy = mkMustReferenceOutputPolicy Cons.V1.checkScriptContext
        wrap = Scripts.mkUntypedMintingPolicy checkedMkMustPayToOtherScriptPolicy

mustReferenceOutputPolicyV2 :: L.MintingPolicy
mustReferenceOutputPolicyV2 = L.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustPayToOtherScriptPolicy = mkMustReferenceOutputPolicy Cons.V2.checkScriptContext
        wrap = Scripts.mkUntypedMintingPolicy checkedMkMustPayToOtherScriptPolicy
data Script a where
   MustReferenceOutputPolicy :: Script L.MintingPolicy

getScript :: Script a -> PSU.Language -> a
getScript MustReferenceOutputPolicy PSU.PlutusV1 = mustReferenceOutputPolicyV1
getScript MustReferenceOutputPolicy PSU.PlutusV2 = mustReferenceOutputPolicyV2

getVersionedScript :: Script a -> PSU.Language -> PSU.Versioned a
getVersionedScript script l = PSU.Versioned (getScript script l) l

type SubmitTx
  =  Cons.ScriptLookups UnitTest
  -> Cons.TxConstraints (Scripts.RedeemerType UnitTest) (Scripts.DatumType UnitTest)
  -> Contract () Empty ContractError Tx.CardanoTx

cardanoSubmitTx :: SubmitTx
cardanoSubmitTx lookups tx = let
  p = defaultCheckOptions ^. emulatorConfig . Trace.params
  in submitUnbalancedTx $ either (error . show) id $ Tx.Cons.mkTx @UnitTest p lookups tx

ledgerSubmitTx :: SubmitTx
ledgerSubmitTx = submitTxConstraintsWith


mkMustReferenceOutputV2Validator :: Tx.TxOutRef -> () -> PV2.ScriptContext -> Bool
mkMustReferenceOutputV2Validator txOutRef _ =
    Cons.V2.checkScriptContext @Void @Void (Cons.mustReferenceOutput txOutRef)

{-# INLINABLE mustReferenceOutputV2Validator #-}
mustReferenceOutputV2Validator :: PV2.Validator
mustReferenceOutputV2Validator = PV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = Scripts.mkUntypedValidator mkMustReferenceOutputV2Validator

mustReferenceOutputV2ValidatorAddress :: L.CardanoAddress
mustReferenceOutputV2ValidatorAddress =
    PSU.V2.mkValidatorCardanoAddress Params.testnet mustReferenceOutputV2Validator

mustReferenceOutputTxV2Contract :: Contract () EmptySchema ContractError ()
mustReferenceOutputTxV2Contract = do
    let mkTx lookups constraints = either (error . show) id $ TxCons.mkTx @Any def lookups constraints

    utxos <- ownUtxos
    let ((utxoRef, utxo), (utxoRefForBalance1, _), (utxoRefForBalance2, _)) = get3 $ M.toList utxos
        vh = PSU.V2.validatorHash mustReferenceOutputV2Validator
        lookups1 = Cons.unspentOutputs utxos
        datum = L.Datum $ PlutusTx.toBuiltinData utxoRef
        tx1 = Cons.mustPayToOtherScriptWithDatumInTx vh datum (Ada.adaValueOf 5)
          <> Cons.mustIncludeDatumInTx datum
          <> Cons.mustSpendPubKeyOutput utxoRefForBalance1
          <> Cons.mustUseOutputAsCollateral utxoRefForBalance1
    submitTxConfirmed $ mkTx lookups1 tx1

    -- Trying to unlock the Ada in the script address
    scriptUtxos <- utxosAt mustReferenceOutputV2ValidatorAddress
    let
        scriptUtxo = fst . head . M.toList $ scriptUtxos
        lookups2 = Cons.unspentOutputs (M.singleton utxoRef utxo <> scriptUtxos)
               <> Cons.plutusV2OtherScript mustReferenceOutputV2Validator
               <> Cons.unspentOutputs utxos
        tx2 = Cons.mustReferenceOutput utxoRef
          <> Cons.mustSpendScriptOutput scriptUtxo L.unitRedeemer
          <> Cons.mustSpendPubKeyOutput utxoRefForBalance2
          <> Cons.mustUseOutputAsCollateral utxoRefForBalance2
    submitTxConfirmed $ mkTx lookups2 tx2

get3 :: [a] -> (a, a, a)
get3 (a:b:c:_) = (a, b, c)
get3 _         = error "Spec.Contract.TxConstraints.get3: not enough inputs"
