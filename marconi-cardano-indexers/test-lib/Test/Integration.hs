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
import Cardano.Api.Extended.Streaming.ChainSyncEvent (ChainSyncEvent)
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Data.LogItem (LOContent (LogMessage), loContent)
import Cardano.Node.Emulator.Generators (knownAddresses, knownXPrvs)
import Cardano.Node.Emulator.Internal.Node.Chain (ChainEvent (SlotAdd))
import Cardano.Node.Emulator.Internal.Node.TimeSlot qualified as E.TimeSlot
import Cardano.Node.Emulator.LogMessages (EmulatorMsg (ChainEvent))
import Cardano.Node.Socket.Emulator qualified as E
import Cardano.Node.Socket.Emulator.Types qualified as E.Types
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO)
import Control.Tracer (condTracing)
import Data.Default (def)
import Data.Maybe (listToMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Ledger.Test (testnet)
import Marconi.Cardano.Core.Extract.WithDistance qualified as Distance
import Marconi.Cardano.Core.Logger qualified as Logger
import Marconi.Cardano.Core.Runner qualified as Runner
import Marconi.Cardano.Core.Types (AnyTxBody (AnyTxBody))
import Marconi.Cardano.Core.Types qualified as Types
import Marconi.Core qualified as Core
import Test.Gen.Marconi.Cardano.Core.Helpers qualified as Core.Helpers
import Test.Gen.Marconi.Cardano.Indexers.MintTokenEvent qualified as Gen.MintTokenEvent

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
    Core.Helpers.leftFail $
      mkValidatedTxBodyWithFee ledgerPP networkId address txbodyc lovelace 1
  let keyWitness = C.makeShelleyKeyWitness txbodyValid witnessSigningKey
  signAndSubmitTx localNodeConnectInfo [keyWitness] txbodyValid

{- | Validate the transaction, calculate the fee and build the transaction body with txOut
 - and fee adjustments applied. This rebuilds a new single TxOut with the txMintValue from the body
 - and the fee-adjusted Ada asset.
 -
 - This is a cheat for building a balanced transaction in the limited context of the integration
 - tests sending a single transaction to the network.
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
      Core.Helpers.mkTxOut address $
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

mkUnbalancedTxBodyContent
  :: (C.TxValidityLowerBound C.BabbageEra, C.TxValidityUpperBound C.BabbageEra)
  -> C.LedgerProtocolParameters C.BabbageEra
  -> [C.TxIn]
  -> [C.TxIn]
  -- ^ Collateral. Ada assets only.
  -> [C.TxOut C.CtxTx C.BabbageEra]
  -> C.TxMintValue C.BuildTx C.BabbageEra
  -> C.TxBodyContent C.BuildTx C.BabbageEra
mkUnbalancedTxBodyContent validityRange ledgerPP txIns collateral txOuts txMintValue =
  (Core.Helpers.emptyTxBodyContent validityRange ledgerPP)
    { C.txIns = map (,C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) txIns
    , C.txOuts = txOuts
    , C.txMintValue = txMintValue
    , C.txInsCollateral = C.TxInsCollateral C.CollateralInBabbageEra collateral
    }

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

-- | Sign the validated tx body, submit to node emulator.
signAndSubmitTx
  :: (MonadIO m)
  => C.LocalNodeConnectInfo C.CardanoMode
  -> [C.KeyWitness C.BabbageEra]
  -> C.TxBody C.BabbageEra
  -> m ()
signAndSubmitTx info witnesses txbody = Core.Helpers.submitTx info tx
  where
    tx = C.makeSignedTransaction witnesses txbody

-- | Set the quantity of all elements in Value to 0.
quantityToZero :: C.Value -> C.Value
quantityToZero = C.valueFromList . map (fmap (const 0)) . C.valueToList

{- Indexers and queries -}

{- | @Core.'CatchupConfig'@ with values suitable for end-to-end tests. The current values are taken
from those hard-coded in the marconi-cardano-chain-index application.
-}
mkEndToEndCatchupConfig :: Core.CatchupConfig indexer event
mkEndToEndCatchupConfig = Core.mkCatchupConfig 5000 100

{- | Build a @Runner.'RunIndexerConfig'@ with values suitable for end-to-end tests with
 - cardano-node-emulator. This assumes there is no rollback and sets the securityParam to 1.
 - Starts at genesis.
-}
mkEndToEndRunIndexerConfig
  :: Logger.MarconiTrace IO
  -> E.Types.NodeServerConfig
  -> Runner.RunIndexerEventPreprocessing (ChainSyncEvent Types.BlockEvent) event
  -> Runner.RunIndexerConfig (ChainSyncEvent Types.BlockEvent) event
mkEndToEndRunIndexerConfig marconiTrace nscConfig preprocessor =
  Runner.RunIndexerConfig
    marconiTrace
    preprocessor
    (Types.RetryConfig 30 (Just 120))
    -- No rollbacks so security param is arbitrary
    1
    (E.Types.nscNetworkId nscConfig)
    C.ChainPointAtGenesis
    (E.Types.nscSocketPath nscConfig)

anyTxBodyWithDistancePreprocessor
  :: Runner.RunIndexerEventPreprocessing
      (ChainSyncEvent Types.BlockEvent)
      (Distance.WithDistance [AnyTxBody])
anyTxBodyWithDistancePreprocessor =
  Runner.RunIndexerEventPreprocessing
    (map (fmap (fmap toTxBodys)) . basePreprocessor)
    (fmap (\(AnyTxBody bn _ _) -> bn) . listToMaybe . Distance.getEvent)
    (Just . fromIntegral . Distance.chainDistance)
  where
    -- Taken from 'buildIndexers'
    getTxBody :: (C.IsCardanoEra era) => C.BlockNo -> Types.TxIndexInBlock -> C.Tx era -> AnyTxBody
    getTxBody blockNo ix tx = AnyTxBody blockNo ix (C.getTxBody tx)
    toTxBodys :: Types.BlockEvent -> [AnyTxBody]
    toTxBodys (Types.BlockEvent (C.BlockInMode (C.Block (C.BlockHeader _ _ bn) txs) _) _ _) =
      zipWith (getTxBody bn) [0 ..] txs
    basePreprocessor = Runner.withDistancePreprocessor ^. Runner.runIndexerPreprocessEvent
