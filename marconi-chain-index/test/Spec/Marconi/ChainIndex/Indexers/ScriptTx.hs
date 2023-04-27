{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -Wno-orphans    #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Spec.Marconi.ChainIndex.Indexers.ScriptTx (tests) where

import Control.Concurrent qualified as IO
import Control.Concurrent.STM qualified as IO
import Control.Exception (catch)
import Control.Monad (replicateM, void)
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as S
import Streaming.Prelude qualified as S
import System.Directory qualified as IO
import System.FilePath ((</>))

import GHC.Stack (HasCallStack)
import Hedgehog (Property, assert, forAll, property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing (defaultConfigStdout)
import Cardano.Streaming (ChainSyncEventException (NoIntersectionFound), withChainSyncEventStream)
import Gen.Marconi.ChainIndex.Types (genTxBodyWithTxIns, genWitnessAndHashInEra)
import Marconi.ChainIndex.Indexers qualified as M
import Marconi.ChainIndex.Indexers.ScriptTx qualified as ScriptTx
import Marconi.ChainIndex.Logging ()
import Marconi.Core.Storable qualified as Storable
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusTx qualified
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import Test.Gen.Cardano.Api.Typed qualified as CGen

import Cardano.Testnet qualified as TN
import Helpers qualified as TN
-- ^ Although these are defined in this cabal component, they are
-- helpers for interacting with the testnet, thus TN

integration :: HasCallStack => H.Integration () -> H.Property
integration = H.withTests 1 . H.propertyOnce


tests :: TestTree
tests = testGroup "Spec.Marconi.ChainIndex.Indexers.ScriptTx"
  [ testPropertyNamed
    "Transaction with script hash survives `createAndValidateTransactionBody`"
    "propGetTxBodyScriptsRoundtrip" propGetTxBodyScriptsRoundtrip
  , testPropertyNamed
    "Submitted transactions with script address show up in indexer"
    "propEndToEndScriptTx" propEndToEndScriptTx
  ]

-- | Create @nScripts@ scripts, add them to a transaction body, then
-- generate a transaction with @createAndValidateTransactionBody@ and check if the
-- scripts put in are present in the generated transaction.
propGetTxBodyScriptsRoundtrip :: Property
propGetTxBodyScriptsRoundtrip = property $ do
  nScripts <- forAll $ Gen.integral (Range.linear 5 500)
  C.AnyCardanoEra (era :: C.CardanoEra era) <- forAll $
    Gen.enum (C.AnyCardanoEra C.AllegraEra) maxBound

  txIns <- replicateM nScripts $ forAll CGen.genTxIn
  witnessesHashes <- replicateM nScripts $ forAll $ genWitnessAndHashInEra era

  let (witnesses, scriptHashes) = unzip witnessesHashes
      collateral = case C.collateralSupportedInEra era of
        Just yes -> C.TxInsCollateral yes txIns
        _        -> C.TxInsCollateralNone

  txBody <- forAll $ genTxBodyWithTxIns era (zip txIns $ map C.BuildTxWith witnesses) collateral
  let hashesFound = map coerce $ ScriptTx.getTxBodyScripts txBody :: [C.ScriptHash]
  assert $ S.fromList scriptHashes == S.fromList hashesFound

{- | We test the script transaction indexer by setting up a testnet,
   adding a script to it and then spending it, and then see if the
   script shows up on the indexer.

   Specifically, we:

    - spin up a testnet and the script transaction indexer

    - create a plutus script, then submit a transaction which contains
      an UTxO where this script is the validator

    - submit a second transaction that spends this UTxO, then query
      the indexer to see if it was indexed properly
-}
propEndToEndScriptTx :: Property
propEndToEndScriptTx = integration $ (liftIO TN.setDarwinTmpdir >>) $ HE.runFinallies $ H.workspace "." $ \tempAbsPath -> do
  base <- HE.noteM $ liftIO . IO.canonicalizePath =<< HE.getProjectBase

  (localNodeConnectInfo, conf, runtime) <- TN.startTestnet (TN.BabbageOnlyTestnetOptions TN.babbageDefaultTestnetOptions) base tempAbsPath
  let networkId = TN.getNetworkId runtime
  socketPathAbs <- TN.getPoolSocketPathAbs conf runtime

  -- Create a channel that is passed into the indexer, such that it
  -- can write index updates to it and we can await for them (also
  -- making us not need threadDelay)
  indexedTxs <- liftIO IO.newChan
  let writeScriptUpdate (ScriptTx.ScriptTxEvent txScripts _slotNo) = case txScripts of
        (x : xs) -> IO.writeChan indexedTxs $ x :| xs
        _        -> pure ()

  -- Start indexer
  let sqliteDb = tempAbsPath </> "script-tx.db"
  indexer <- liftIO $ do

    coordinator <- M.initialCoordinator 1
    ch <- IO.atomically . IO.dupTChan $ M._channel coordinator
    (loop, indexer) <- M.scriptTxWorker_ (\update -> writeScriptUpdate update $> []) (ScriptTx.Depth 1) coordinator ch sqliteDb

    -- Receive ChainSyncEvents and pass them on to indexer's channel
    void $ IO.forkIO $ do
      let chainPoint = C.ChainPointAtGenesis :: C.ChainPoint
      c <- defaultConfigStdout
      withTrace c "marconi" $ \trace -> let
        indexerWorker = withChainSyncEventStream socketPathAbs networkId [chainPoint] $ S.mapM_ $
          \chainSyncEvent -> IO.atomically $ IO.writeTChan ch chainSyncEvent
        handleException NoIntersectionFound = logError trace $ renderStrict $ layoutPretty defaultLayoutOptions $
          "No intersection found for chain point" <+> pretty chainPoint <> "."
        in indexerWorker `catch` handleException :: IO ()

    -- Start indexer worker loop
    void $ IO.forkIO loop

    return indexer

  let
    -- Create an always succeeding validator script
    plutusScript :: C.PlutusScript C.PlutusScriptV1
    plutusScript = C.PlutusScriptSerialised $ PlutusV2.serialiseCompiledCode validator
      where
        validator :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
        validator = $$(PlutusTx.compile [|| \_ _ _ -> () ||])

    plutusScriptHash = C.hashScript $ C.PlutusScript C.PlutusScriptV1 plutusScript :: C.ScriptHash
    plutusScriptAddr :: C.Address C.ShelleyAddr
    plutusScriptAddr = C.makeShelleyAddress networkId (C.PaymentCredentialByScript plutusScriptHash) C.NoStakeAddress

  -- Step 1: Create a tx ouput with a datum hash at the script address. In order for a tx ouput to be locked
  -- by a plutus script, it must have a datahash. We also need collateral tx inputs so we split the utxo
  -- in order to accomodate this.

  genesisVKey :: C.VerificationKey C.GenesisUTxOKey <-
    TN.readAs (C.AsVerificationKey C.AsGenesisUTxOKey) $ tempAbsPath </> "utxo-keys/utxo1.vkey"
  genesisSKey :: C.SigningKey C.GenesisUTxOKey <-
    TN.readAs (C.AsSigningKey C.AsGenesisUTxOKey) $ tempAbsPath </> "utxo-keys/utxo1.skey"

  let
    paymentKey = C.castVerificationKey genesisVKey :: C.VerificationKey C.PaymentKey
    address :: C.Address C.ShelleyAddr
    address = C.makeShelleyAddress
      networkId
      (C.PaymentCredentialByKey (C.verificationKeyHash paymentKey :: C.Hash C.PaymentKey))
      C.NoStakeAddress :: C.Address C.ShelleyAddr

  (tx1in, C.TxOut _ v _ _) <- do
    utxo <- TN.findUTxOByAddress @C.BabbageEra localNodeConnectInfo address
    H.headM $ Map.toList $ C.unUTxO utxo
  let totalLovelace = C.txOutValueToLovelace v

  pparams <- TN.getProtocolParams @C.BabbageEra localNodeConnectInfo
  let scriptDatum = C.ScriptDataNumber 42 :: C.ScriptData
      scriptDatumHash = C.hashScriptDataBytes $ C.unsafeHashableScriptData scriptDatum
      amountPaid = 10_000_000 :: C.Lovelace -- 10 ADA
      -- Must return everything that was not paid to script and that didn't went to fees:
      amountReturned = totalLovelace - amountPaid :: C.Lovelace
      txOut1 :: C.TxOut ctx C.BabbageEra
      txOut1 = C.TxOut
        (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) plutusScriptAddr)
        (C.TxOutValue C.MultiAssetInBabbageEra $ C.lovelaceToValue amountPaid)
        (C.TxOutDatumHash C.ScriptDataInBabbageEra scriptDatumHash)
        C.ReferenceScriptNone
      mkTxOut2 :: C.Lovelace -> C.TxOut ctx C.BabbageEra
      mkTxOut2 lovelace = TN.mkAddressAdaTxOut address lovelace
      keyWitnesses = [C.WitnessPaymentKey $ C.castSigningKey genesisSKey]
      tx1ins = [(tx1in, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)]
      validityRange = (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra)

  (tx1fee, txbc0) <- TN.calculateAndUpdateTxFee pparams networkId (length tx1ins) (length keyWitnesses) (TN.emptyTxBodyContent validityRange pparams)
    { C.txIns = tx1ins
    , C.txOuts = [txOut1, mkTxOut2 amountReturned]
    , C.txProtocolParams = C.BuildTxWith $ Just pparams
    }
  let txbc1 = txbc0 { C.txOuts = [txOut1, mkTxOut2 $ amountReturned - tx1fee] }
  tx1body :: C.TxBody C.BabbageEra <- H.leftFail $ C.createAndValidateTransactionBody txbc1
  TN.submitTx localNodeConnectInfo $ C.makeSignedTransaction (map (C.makeShelleyKeyWitness tx1body) keyWitnesses) tx1body



  -- Second transaction: spend the UTXO specified in the first transaction

  _ <- liftIO $ IO.readChan indexedTxs -- wait for the first transaction to be accepted

  tx2collateralTxIn <- H.headM . Map.keys . C.unUTxO =<< TN.findUTxOByAddress @C.BabbageEra localNodeConnectInfo address

  (scriptTxIn, C.TxOut _ valueAtScript _ _) <- do
    scriptUtxo <- TN.findUTxOByAddress @C.BabbageEra localNodeConnectInfo plutusScriptAddr
    H.headM $ Map.toList $ C.unUTxO scriptUtxo

  let lovelaceAtScript = C.txOutValueToLovelace valueAtScript
  assert $ lovelaceAtScript == 10_000_000 -- script has the 10 ADA we put there in tx1

  redeemer <- H.forAll CGen.genHashableScriptData -- The script always returns true so any redeemer will do
  let
      -- The following execution unit and fee values were found by
      -- trial and error. When the transaction which we are in the
      -- process of creating, fails, then it will report the values it
      -- wants. And although they change again after you correct them,
      -- then the procedure converges quickly.
      executionUnits = C.ExecutionUnits {C.executionSteps = 500_000, C.executionMemory = 10_000 }

      scriptWitness :: C.Witness C.WitCtxTxIn C.BabbageEra
      scriptWitness = C.ScriptWitness C.ScriptWitnessForSpending $
        C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1 (C.PScript plutusScript)
        (C.ScriptDatumForTxIn $ C.unsafeHashableScriptData scriptDatum) redeemer executionUnits

      tx2ins = [(scriptTxIn, C.BuildTxWith scriptWitness)]
      mkTx2Outs lovelace = [TN.mkAddressAdaTxOut address lovelace]
      tx2witnesses = [C.WitnessGenesisUTxOKey genesisSKey]
      tx2fee = 1000303 :: C.Lovelace

      tx2bodyContent = (TN.emptyTxBodyContent validityRange pparams)
        { C.txIns              = tx2ins
        , C.txInsCollateral    = C.TxInsCollateral C.CollateralInBabbageEra [tx2collateralTxIn]
        , C.txOuts             = mkTx2Outs $ lovelaceAtScript - tx2fee
        , C.txFee              = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra tx2fee
        }
  tx2body :: C.TxBody C.BabbageEra <- H.leftFail $ C.createAndValidateTransactionBody tx2bodyContent
  let tx2 = C.signShelleyTransaction tx2body tx2witnesses
  TN.submitTx localNodeConnectInfo tx2

  {- Test if what the indexer got is what we sent.

  We test both of (1) what we get from `onInsert` callback and (2)
  with `query` (what ends up in the sqlite database). For the `query`
  we currently need to use threadDelay and poll to query the database
  because the indexer runs in a separate thread and there is no way
  of awaiting the data to be flushed into the database. -}

  indexedWithScriptHashes <- liftIO $ IO.readChan indexedTxs

  -- We have to filter out the txs the empty scripts hashes because
  -- sometimes the RollForward event contains a block with the first transaction 'tx1'
  -- which has no scripts. The test fails because of that in 'headM indexedScriptHashes'.
  -- For more details see https://github.com/input-output-hk/plutus-apps/issues/775
  let (ScriptTx.TxCbor tx, indexedScriptHashes) = head $ NE.filter (\(_, hashes) -> hashes /= []) indexedWithScriptHashes

  ScriptTx.ScriptTxAddress indexedScriptHash <- H.headM indexedScriptHashes

  indexedTx2 :: C.Tx C.BabbageEra <- H.leftFail $ C.deserialiseFromCBOR (C.AsTx C.AsBabbageEra) tx

  plutusScriptHash === indexedScriptHash
  tx2 === indexedTx2

  queriedTx2 :: C.Tx C.BabbageEra <- do
    ScriptTx.ScriptTxResult (ScriptTx.TxCbor txCbor : _) <- liftIO $ do
      ix <- IO.readMVar indexer
      Storable.query Storable.QEverything ix (ScriptTx.ScriptTxAddress plutusScriptHash)
    H.leftFail $ C.deserialiseFromCBOR (C.AsTx C.AsBabbageEra) txCbor

  tx2 === queriedTx2
