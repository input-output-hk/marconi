{-# LANGUAGE TupleSections #-}

-- | Utilities for integration tests using tools from the `cardano-node-emulator` project.
module Test.Integration where

import Cardano.Api.Extended.IPC qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Node.Emulator.Generators (knownAddresses, knownXPrvs)
import Cardano.Node.Socket.Emulator qualified as E
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import Hedgehog qualified as H
import Hedgehog.Extras.Test.Base qualified as H
import Ledger.Test (testnet)
import Test.Helpers qualified as Helpers

-- | Start a testnet using the node emulator, within the @H.'Integration'@ context.
startTestnet
  :: FilePath
  -> H.Integration (C.LocalNodeConnectInfo C.CardanoMode, C.NetworkId, FilePath)
startTestnet tempAbsBasePath = do
  let socketPathAbs = tempAbsBasePath <> "/node-server.sock"
      -- TODO: PLT-8098 leftover from legacy code: "any other networkId doesn't work"
      networkId = testnet
      -- In milliseconds, shorter than the default to make the tests go faster
      slotLength = 10
      localNodeConnectInfo = C.mkLocalNodeConnectInfo networkId socketPathAbs
  liftIO $ E.startTestnet socketPathAbs slotLength networkId
  pure (localNodeConnectInfo, networkId, socketPathAbs)

-- | Take the first @C.'ShelleyAddr'@ of @Cardano.Node.Emulator.Generators.'knownAddresses'@.
knownShelleyAddress :: Maybe (C.Address C.ShelleyAddr)
knownShelleyAddress = undefined

-- | Take the first @C.'WitnessPaymentExtendedKey'@ from 'knownXPrvs'.
knownKeyWitness :: Maybe C.ShelleyWitnessSigningKey
knownKeyWitness = C.WitnessPaymentExtendedKey . C.PaymentExtendedSigningKey <$> listToMaybe knownXPrvs

-- | Unbounded validity range for transaction validation in the Babbage era.
unboundedValidityRange :: (C.TxValidityLowerBound C.BabbageEra, C.TxValidityUpperBound C.BabbageEra)
unboundedValidityRange = (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra)

-- TODO: PLT-8098 confirm that txOuts are not used for fee calculation and modify this
-- and or mkValidatedTxBodyWithFee.

{- | TODO: PLT-8098 docs and annotate inputs. Note lovelace not included yet in txout.
Does this need to be generalized over  C.IsShelleyBasedEra era?
-}
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
  -> C.Address C.ShelleyAddr
  -> C.TxBodyContent C.BuildTx C.BabbageEra
  -> C.Lovelace
  -- ^ Lovelace to fix in in TxOut, before applying fees
  -> Word
  -- ^ The number of Shelley key witnesses
  -> Either C.TxBodyError (C.TxBody C.BabbageEra)
mkValidatedTxBodyWithFee ledgerPP address txbodyc lovelace nKeywitnesses =
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
    calculateFee :: C.TxBody C.BabbageEra -> C.Lovelace
    calculateFee txbody' = C.evaluateTransactionFee (C.unLedgerProtocolParameters ledgerPP) txbody' nKeywitnesses 0
    -- Sets the txFee field as well as the txOuts field to a single TxOut with the fee-adjusted
    -- lovelace
    updateTxWithFee
      :: C.TxBodyContent C.BuildTx C.BabbageEra
      -> C.TxBody C.BabbageEra
      -> C.TxBodyContent C.BuildTx C.BabbageEra
    updateTxWithFee txbodyc' txbody' =
      C.setTxOuts [rebuildTxOutWithFee txbodyc' (calculateFee txbody')] $
        C.setTxFee (C.TxFeeExplicit C.TxFeesExplicitInBabbageEra $ calculateFee txbody') txbodyc'

--

{- | TODO: PLT-8098 sign the validated tx body and submit to node emulator within H.Integration
context
-}
signAndSubmitTx
  :: C.LocalNodeConnectInfo C.CardanoMode
  -> [C.KeyWitness C.BabbageEra]
  -> C.TxBody C.BabbageEra
  -> H.Integration ()
signAndSubmitTx info witnesses = Helpers.submitTx info . C.makeSignedTransaction witnesses

-- | TODO: PLT-8098 put generators in a Gen submodule as with legacy
genTxMintValue :: H.Gen (C.TxMintValue C.BuildTx C.BabbageEra)
genTxMintValue = undefined
