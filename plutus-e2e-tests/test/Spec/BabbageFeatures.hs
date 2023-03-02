{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- Not using all CardanoEra

module Spec.BabbageFeatures(tests) where

import Cardano.Api qualified as C
import Data.Map qualified as Map
import Test.Tasty (TestTree, testGroup)

import Hedgehog qualified as H
import Hedgehog.Extras.Test qualified as HE
import Test.Base qualified as H
import Test.Tasty.Hedgehog (testProperty)

import CardanoTestnet qualified as TN
import Helpers.Common (makeAddress)
import Helpers.Query qualified as Q
import Helpers.Testnet (testnetOptionsBabbage7, testnetOptionsBabbage8)
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.Utils qualified as U (workspace)
import PlutusScripts.AlwaysSucceeds qualified as PS
import PlutusScripts.Helpers qualified as PS

tests :: TestTree
tests = testGroup "Babbage Features"
  [ testGroup "reference script"
    [ testProperty "mint a token with a reference script in Babbage PV7" (referenceScriptMintTest testnetOptionsBabbage7)
    , testProperty "mint a token with a reference script in Babbage PV8" (referenceScriptMintTest testnetOptionsBabbage8)
    --, testProperty "mint a token with a reference script in Babbage PV8" (referenceScriptMintTest localNodeOptionsPreview) -- uncomment to use local node on preview testnet

    , testProperty "spend locked funds with a reference script using inline datum in Babbage PV7" (referenceScriptInlineDatumSpendTest testnetOptionsBabbage7)
    , testProperty "spend locked funds with a reference script using inline datum in Babbage PV8" (referenceScriptInlineDatumSpendTest testnetOptionsBabbage8)
    --, testProperty "spend locked funds with a reference script using inline datum in Babbage PV8" (referenceScriptInlineDatumSpendTest localNodeOptionsPreview) -- uncomment to use local node on preview testnet

    , testProperty "spend locked funds with a reference script providing datum in txbody in Babbage PV7" (referenceScriptDatumHashSpendTest testnetOptionsBabbage7)
    , testProperty "spend locked funds with a reference script providing datum in txbody in Babbage PV8" (referenceScriptDatumHashSpendTest testnetOptionsBabbage8)
    --, testProperty "spend locked funds with a reference script providing datum in txbody in Babbage PV8" (referenceScriptDatumHashSpendTest localNodeOptionsPreview) -- uncomment to use local node on preview testnet
    ]
  ]

referenceScriptMintTest :: Either TN.LocalNodeOptions TN.TestnetOptions -> H.Property
referenceScriptMintTest networkOptions = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions

  -- 1: spin up a testnet or use local node connected to public testnet
  (localNodeConnectInfo, pparams, networkId) <- TN.setupTestEnvironment networkOptions tempAbsPath
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  -- 2: build a transaction to hold reference script

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    refScriptTxOut = Tx.txOutWithRefScript era (C.lovelaceToValue 20_000_000) w1Address
      (PS.unPlutusScriptV2 PS.alwaysSucceedPolicyScriptV2)
    otherTxOut = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txOuts = [refScriptTxOut, otherTxOut]
      }

  signedTx <- Tx.buildTx era txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let refScriptTxIn = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn   = Tx.txIn (Tx.txId signedTx) 1
  Q.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn "Tx.waitForTxInAtAddress"

  -- 3: build a transaction to mint token using reference script

  let
    tokenValues = C.valueFromList [(PS.alwaysSucceedAssetIdV2, 6)]
    mintWitnesses = Map.fromList [PS.alwaysSucceedMintWitnessV2 era (Just refScriptTxIn)]
    collateral = Tx.txInsCollateral era [otherTxIn]
    txOut = Tx.txOut era (C.lovelaceToValue 3_000_000 <> tokenValues) w1Address

    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [otherTxIn]
      , C.txInsCollateral = collateral
      , C.txInsReference = Tx.txInsReference era [refScriptTxIn]
      , C.txMintValue = Tx.txMintValue era tokenValues mintWitnesses
      , C.txOuts = [txOut]
      }

  signedTx2 <- Tx.buildTx era txBodyContent2 w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "Tx.getTxOutAtAddress"
  txOutHasTokenValue <- Q.txOutHasValue resultTxOut tokenValues
  H.assert txOutHasTokenValue
  H.success


referenceScriptInlineDatumSpendTest :: Either TN.LocalNodeOptions TN.TestnetOptions -> H.Property
referenceScriptInlineDatumSpendTest networkOptions = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions

  -- 1: spin up a testnet or use local node connected to public testnet
  (localNodeConnectInfo, pparams, networkId) <- TN.setupTestEnvironment networkOptions tempAbsPath
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  -- 2: build a transaction to hold reference script

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    refTxOut      = Tx.txOutWithRefScript era (C.lovelaceToValue 20_000_000) w1Address
                     (PS.unPlutusScriptV2 PS.alwaysSucceedSpendScriptV2)
    otherTxOut    = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address
    scriptAddress = makeAddress (Right PS.alwaysSucceedSpendScriptHashV2) networkId
    scriptTxOut   = Tx.txOutWithInlineDatum era (C.lovelaceToValue 10_000_000) scriptAddress (PS.toScriptData ())

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txOuts = [refTxOut, otherTxOut, scriptTxOut]
      }

  signedTx <- Tx.buildTx era txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let refScriptTxIn = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn     = Tx.txIn (Tx.txId signedTx) 1
      txInAtScript  = Tx.txIn (Tx.txId signedTx) 2
  Q.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn "Tx.waitForTxInAtAddress"

  -- 3: build a transaction to mint token using reference script

  let
    scriptTxIn = Tx.txInWitness txInAtScript (PS.alwaysSucceedSpendWitnessV2 era (Just refScriptTxIn) Nothing)
    collateral = Tx.txInsCollateral era [otherTxIn]
    adaValue = C.lovelaceToValue 4_200_000
    txOut = Tx.txOut era adaValue w1Address

    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = [scriptTxIn]
      , C.txInsReference = Tx.txInsReference era [refScriptTxIn]
      , C.txInsCollateral = collateral
      , C.txOuts = [txOut]
      }

  signedTx2 <- Tx.buildTx era txBodyContent2 w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "Tx.getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut adaValue
  H.assert txOutHasAdaValue
  H.success


referenceScriptDatumHashSpendTest :: Either TN.LocalNodeOptions TN.TestnetOptions -> H.Property
referenceScriptDatumHashSpendTest networkOptions = H.integration . HE.runFinallies . U.workspace "." $ \tempAbsPath -> do

  C.AnyCardanoEra era <- TN.eraFromOptions networkOptions

  -- 1: spin up a testnet or use local node connected to public testnet
  (localNodeConnectInfo, pparams, networkId) <- TN.setupTestEnvironment networkOptions tempAbsPath
  (w1SKey, _, w1Address) <- TN.w1 tempAbsPath networkId

  -- 2: build a transaction to hold reference script

  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address

  let
    refTxOut      = Tx.txOutWithRefScript era (C.lovelaceToValue 20_000_000) w1Address
                     (PS.unPlutusScriptV2 PS.alwaysSucceedSpendScriptV2)
    otherTxOut    = Tx.txOut era (C.lovelaceToValue 5_000_000) w1Address
    scriptAddress = makeAddress (Right PS.alwaysSucceedSpendScriptHashV2) networkId
    datum         = PS.toScriptData ()
    scriptTxOut   = Tx.txOutWithDatumHash era (C.lovelaceToValue 10_000_000) scriptAddress datum

    txBodyContent = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = Tx.pubkeyTxIns [txIn]
      , C.txOuts = [refTxOut, otherTxOut, scriptTxOut]
      }

  signedTx <- Tx.buildTx era txBodyContent w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx
  let refScriptTxIn = Tx.txIn (Tx.txId signedTx) 0
      otherTxIn     = Tx.txIn (Tx.txId signedTx) 1
      txInAtScript  = Tx.txIn (Tx.txId signedTx) 2
  Q.waitForTxInAtAddress era localNodeConnectInfo w1Address refScriptTxIn "Tx.waitForTxInAtAddress"

  -- 3: build a transaction to mint token using reference script

  let
    scriptTxIn = Tx.txInWitness txInAtScript $ PS.alwaysSucceedSpendWitnessV2 era (Just refScriptTxIn) (Just datum)
    collateral = Tx.txInsCollateral era [otherTxIn]
    adaValue = C.lovelaceToValue 4_200_000
    txOut = Tx.txOut era adaValue w1Address

    txBodyContent2 = (Tx.emptyTxBodyContent era pparams)
      { C.txIns = [scriptTxIn]
      , C.txInsReference = Tx.txInsReference era [refScriptTxIn]
      , C.txInsCollateral = collateral
      , C.txOuts = [txOut]
      }

  signedTx2 <- Tx.buildTx era txBodyContent2 w1Address w1SKey networkId
  Tx.submitTx era localNodeConnectInfo signedTx2
  let expectedTxIn = Tx.txIn (Tx.txId signedTx2) 0
  -- Query for txo and assert it contains newly minted token
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expectedTxIn "Tx.getTxOutAtAddress"
  txOutHasAdaValue <- Q.txOutHasValue resultTxOut adaValue
  H.assert txOutHasAdaValue
  H.success

  -- TODO: inlineDatumSpendTest (no reference script)
  -- TODO: Check V2 TxInfo