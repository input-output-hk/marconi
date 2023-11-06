{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

-- | Utilities for integration tests using tools from the `cardano-node-emulator` project.
module Test.Integration (
  module Test.Integration,
  E.Types.nscSocketPath,
  E.Types.nscNetworkId,
) where

import Cardano.Api.Extended.IPC qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Data.LogItem (LOContent (LogMessage), loContent)
import Cardano.Node.Emulator.Generators (knownAddresses, knownXPrvs)
import Cardano.Node.Emulator.Internal.Node.Chain (ChainEvent (SlotAdd))
import Cardano.Node.Emulator.Internal.Node.TimeSlot qualified as E.TimeSlot
import Cardano.Node.Emulator.LogMessages (EmulatorMsg (ChainEvent))
import Cardano.Node.Socket.Emulator qualified as E
import Cardano.Node.Socket.Emulator.Types qualified as E.Types
import Control.Monad.IO.Class (MonadIO)
import Control.Tracer (condTracing)
import Data.Default (def)
import Data.Maybe (listToMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Ledger.Test (testnet)
import Test.Gen.Marconi.ChainIndex.MintTokenEvent qualified as Gen.MintTokenEvent
import Test.Helpers qualified as Helpers

-- TODO: PLT-8098 need to shut down the node emulator properly when the test is done.

{- Node emulator setup and helpers -}

-- | Start a testnet using the node emulator, omitting 'SlotAdd' messages.
startTestnet
  :: E.Types.NodeServerConfig
  -> IO ()
startTestnet = E.main (condTracing (not . isSlotAddLogObject . snd) E.prettyTrace)
  where
    isSlotAddLogObject = isSlotAddMsg . loContent

-- | Predicate used to suppress 'SlotAdd' messages in the logger.
isSlotAddMsg :: LOContent E.Types.CNSEServerLogMsg -> Bool
isSlotAddMsg (LogMessage (E.Types.ProcessingEmulatorMsg (ChainEvent (SlotAdd _)))) = True
isSlotAddMsg _ = False

{- | Construct local node configuration for node emulator from a temporary directory path and slot
length parameter, for use in integration tests.
-}
mkLocalNodeInfo
  :: FilePath
  -- ^ Absolute path of temporary directory for test.
  -> Integer
  -- ^ Slot length.
  -> IO (E.Types.NodeServerConfig, C.LocalNodeConnectInfo C.CardanoMode)
mkLocalNodeInfo tempAbsBasePath slotLength = do
  now <- getPOSIXTime
  let socketPathAbs = tempAbsBasePath <> "/node-server.sock"
      -- Testnet is the only one that works
      networkId = testnet
      localNodeConnectInfo = C.mkLocalNodeConnectInfo networkId socketPathAbs
      config =
        def
          { E.Types.nscSlotConfig =
              def
                { E.TimeSlot.scSlotLength = slotLength
                , E.TimeSlot.scSlotZeroTime = E.TimeSlot.nominalDiffTimeToPOSIXTime now
                }
          , E.Types.nscSocketPath = socketPathAbs
          , E.Types.nscNetworkId = networkId
          }
  pure (config, localNodeConnectInfo)

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
  :: (MonadIO m)
  => C.LocalNodeConnectInfo C.CardanoMode
  -> C.LedgerProtocolParameters C.BabbageEra
  -> C.NetworkId
  -> C.Address C.ShelleyAddr
  -> C.ShelleyWitnessSigningKey
  -> C.TxBodyContent C.BuildTx C.BabbageEra
  -> C.Lovelace
  -- ^ Lovelace before applying fees
  -> m ()
validateAndSubmitTx localNodeConnectInfo ledgerPP networkId address witnessSigningKey txbodyc lovelace = do
  txbodyValid <-
    Helpers.leftFail $
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
  -> Int
  -- ^ The number of Shelley key witnesses
  -> Either C.TxBodyError (C.TxBody C.BabbageEra)
mkValidatedTxBodyWithFee ledgerPP networkid address txbodyc lovelace nKeywitnesses =
  C.createAndValidateTransactionBody txbodyc
    >>= C.createAndValidateTransactionBody . updateTxWithFee txbodyc
  where
    rebuildTxOutWithFee
      :: C.TxBodyContent C.BuildTx C.BabbageEra -> C.Lovelace -> C.TxOut C.CtxTx C.BabbageEra
    rebuildTxOutWithFee txbodyc' fee =
      Helpers.mkTxOut address $
        C.lovelaceToValue (lovelace - fee)
          <> Gen.MintTokenEvent.getValueFromTxMintValue (C.txMintValue txbodyc')
    updateTxWithFee
      :: C.TxBodyContent C.BuildTx C.BabbageEra
      -> C.TxBody C.BabbageEra
      -> C.TxBodyContent C.BuildTx C.BabbageEra
    updateTxWithFee txbodyc' txbody' =
      C.setTxOuts [rebuildTxOutWithFee txbodyc' fee] $
        C.setTxFee (C.TxFeeExplicit C.TxFeesExplicitInBabbageEra fee) txbodyc'
      where
        fee = calculateFee ledgerPP (length $ C.txIns txbodyc') 1 0 nKeywitnesses networkid txbody'

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
    , C.txOuts = [Helpers.mkTxOut address $ Gen.MintTokenEvent.getValueFromTxMintValue txMintValue]
    , C.txMintValue = txMintValue
    , C.txInsCollateral = C.TxInsCollateral C.CollateralInBabbageEra txIns
    }

{- Indexers and queries -}

-- | Wrapper for @C.'estimateTransactionFee'@.
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
        nShelleyKeyWitnesses
        nByronKeyWitnesses

{- | Sign the validated tx body, submit to node emulator
 and wait for it to be submitted over the protocol.
-}
signAndSubmitTx
  :: (MonadIO m)
  => C.LocalNodeConnectInfo C.CardanoMode
  -> [C.KeyWitness C.BabbageEra]
  -> C.TxBody C.BabbageEra
  -> m ()
signAndSubmitTx info witnesses txbody = Helpers.submitAwaitTx info (tx, txbody)
  where
    tx = C.makeSignedTransaction witnesses txbody
