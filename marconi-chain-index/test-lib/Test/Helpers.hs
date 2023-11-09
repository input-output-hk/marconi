{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}

module Test.Helpers where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Exception (errorCallException, errorCallWithCallStackException)
import GHC.Stack qualified as GHC
import Hedgehog (MonadTest)
import Hedgehog qualified as H
import Hedgehog.Extras.Stock.CallStack qualified as H
import Hedgehog.Extras.Test qualified as HE
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (SubmitFail, SubmitSuccess))
import System.Directory qualified as IO
import System.Environment qualified as IO
import System.IO qualified as IO
import System.IO.Temp qualified as IO
import System.Info qualified as IO

{- Protocol / transaction helpers -}

-- | An empty transaction
emptyTxBodyContent
  :: (C.IsShelleyBasedEra era)
  => (C.TxValidityLowerBound era, C.TxValidityUpperBound era)
  -> C.LedgerProtocolParameters era
  -> C.TxBodyContent C.BuildTx era
emptyTxBodyContent validityRange ledgerPP =
  C.TxBodyContent
    { C.txIns = []
    , C.txInsCollateral = C.TxInsCollateralNone
    , C.txInsReference = C.TxInsReferenceNone
    , C.txOuts = []
    , C.txTotalCollateral = C.TxTotalCollateralNone
    , C.txReturnCollateral = C.TxReturnCollateralNone
    , C.txFee = C.TxFeeExplicit (txFeesExplicitInShelleyBasedEra C.shelleyBasedEra) 0
    , C.txValidityRange = validityRange
    , C.txMetadata = C.TxMetadataNone
    , C.txAuxScripts = C.TxAuxScriptsNone
    , C.txExtraKeyWits = C.TxExtraKeyWitnessesNone
    , C.txProtocolParams = C.BuildTxWith $ Just ledgerPP
    , C.txWithdrawals = C.TxWithdrawalsNone
    , C.txCertificates = C.TxCertificatesNone
    , C.txUpdateProposal = C.TxUpdateProposalNone
    , C.txMintValue = C.TxMintNone
    , C.txScriptValidity = C.TxScriptValidityNone
    , C.txProposalProcedures = Nothing
    , C.txVotingProcedures = Nothing
    }

getLedgerProtocolParams
  :: forall era m
   . (C.IsShelleyBasedEra era, MonadIO m)
  => C.LocalNodeConnectInfo C.CardanoMode
  -> m (C.LedgerProtocolParameters era)
getLedgerProtocolParams localNodeConnectInfo = do
  eraInMode <-
    nothingFail eraInModeFailMsg $
      C.toEraInMode (C.shelleyBasedToCardanoEra (C.shelleyBasedEra @era)) C.CardanoMode
  fmap C.LedgerProtocolParameters . leftFailM . leftFailM . liftIO $
    C.queryNodeLocalState localNodeConnectInfo Nothing $
      C.QueryInEra eraInMode $
        C.QueryInShelleyBasedEra C.shelleyBasedEra C.QueryProtocolParameters

findUTxOByAddress
  :: (C.IsShelleyBasedEra era, MonadIO m)
  => C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address a
  -> m (C.UTxO era)
findUTxOByAddress localNodeConnectInfo address = do
  let query =
        C.QueryInShelleyBasedEra C.shelleyBasedEra $
          C.QueryUTxO $
            C.QueryUTxOByAddress $
              Set.singleton (C.toAddressAny address)
  eraInMode <-
    nothingFail eraInModeFailMsg $
      C.toEraInMode (C.shelleyBasedToCardanoEra C.shelleyBasedEra) C.CardanoMode
  leftFailM . leftFailM . liftIO $
    C.queryNodeLocalState localNodeConnectInfo Nothing $
      C.QueryInEra eraInMode query

-- | Get [TxIn] and total value for an address.
getAddressTxInsValue
  :: forall era m a
   . (C.IsShelleyBasedEra era, MonadIO m)
  => C.LocalNodeConnectInfo C.CardanoMode
  -> C.Address a
  -> m ([C.TxIn], C.Lovelace)
getAddressTxInsValue con address = do
  utxo <- findUTxOByAddress @era con address
  let (txIns, txOuts) = unzip $ Map.toList $ C.unUTxO utxo
      values = map (\case C.TxOut _ v _ _ -> C.txOutValueToLovelace v) txOuts
  pure (txIns, sum values)

submitTx
  :: (C.IsCardanoEra era, MonadIO m)
  => C.LocalNodeConnectInfo C.CardanoMode
  -> C.Tx era
  -> m ()
submitTx localNodeConnectInfo tx = do
  eraInMode <-
    nothingFail eraInModeFailMsg $
      C.toEraInMode C.cardanoEra C.CardanoMode
  submitResult :: SubmitResult (C.TxValidationErrorInMode C.CardanoMode) <-
    liftIO $ C.submitTxToNodeLocal localNodeConnectInfo $ C.TxInMode tx eraInMode
  failOnTxSubmitFail submitResult
  where
    failOnTxSubmitFail :: (Show a, MonadIO m) => SubmitResult a -> m ()
    failOnTxSubmitFail = \case
      SubmitFail reason ->
        liftIO $
          throwIO $
            flip errorCallWithCallStackException GHC.callStack $
              "Transaction failed: " <> show reason
      SubmitSuccess -> liftIO $ putStrLn "Transaction submitted successfully"

mkAddressValueTxOut
  :: (C.IsShelleyBasedEra era)
  => C.Address C.ShelleyAddr
  -> C.TxOutValue era
  -> C.TxOut ctx era
mkAddressValueTxOut address value =
  C.TxOut
    (C.AddressInEra (C.ShelleyAddressInEra C.shelleyBasedEra) address)
    value
    C.TxOutDatumNone
    C.ReferenceScriptNone

mkAddressAdaTxOut
  :: (C.IsShelleyBasedEra era)
  => C.Address C.ShelleyAddr
  -> C.Lovelace
  -> C.TxOut ctx era
mkAddressAdaTxOut address lovelace =
  let txOutValue =
        case C.multiAssetSupportedInEra $ C.shelleyBasedToCardanoEra C.shelleyBasedEra of
          Left adaOnlyInEra -> C.TxOutAdaOnly adaOnlyInEra lovelace
          Right multiAssetInEra -> C.TxOutValue multiAssetInEra $ C.lovelaceToValue lovelace
   in mkAddressValueTxOut address txOutValue

-- | Create a single @C.'TxOut'@ from a Babbage-era @C.'TxOutValue'@.
mkTxOut :: C.Address C.ShelleyAddr -> C.Value -> C.TxOut ctx C.BabbageEra
mkTxOut address' = mkAddressValueTxOut address' . C.TxOutValue C.MultiAssetInBabbageEra

{- | Adapted from:
 https://github.com/input-output-hk/cardano-node/blob/d15ff2b736452857612dd533c1ddeea2405a2630/cardano-cli/src/Cardano/CLI/Shelley/Run/Transaction.hs#L1105-L1112
 https://github.com/input-output-hk/cardano-node/blob/d15ff2b736452857612dd533c1ddeea2405a2630/cardano-cli/src/Cardano/CLI/Shelley/Run/Transaction.hs#L1121-L1128
-}
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
        nByronKeyWitnesses
        nShelleyKeyWitnesses

{- | Add fee to transaction body, return transaction with the fee
 applied, and also the fee in lovelace.
-}
calculateAndUpdateTxFee
  :: (H.MonadTest m)
  => C.LedgerProtocolParameters C.BabbageEra
  -> C.NetworkId
  -> Int
  -> Int
  -> C.TxBodyContent C.BuildTx C.BabbageEra
  -> m (C.Lovelace, C.TxBodyContent C.BuildTx C.BabbageEra)
calculateAndUpdateTxFee ledgerPP networkId lengthTxIns lengthKeyWitnesses txbc = do
  txb <- HE.leftFail $ C.createAndValidateTransactionBody txbc
  let feeLovelace =
        calculateFee ledgerPP lengthTxIns (length $ C.txOuts txbc) 0 lengthKeyWitnesses networkId txb
          :: C.Lovelace
      fee = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra feeLovelace
      txbc' = txbc{C.txFee = fee}
  return (feeLovelace, txbc')

{- Hedgehog helpers -}

{- | This is a copy of the workspace from
 hedgehog-extras:Hedgehog.Extras.Test.Base, which for darwin sets
 the systemTemp folder to /tmp.

 It creates a temporary folder with @prefixPath@, which is removed
 after the supplied function @f@ returns.
-}
workspace :: (MonadTest m, MonadIO m, GHC.HasCallStack) => FilePath -> (FilePath -> m ()) -> m ()
workspace prefixPath f = GHC.withFrozenCallStack $ do
  systemTemp <- case IO.os of
    "darwin" -> pure "/tmp"
    _ -> H.evalIO IO.getCanonicalTemporaryDirectory
  maybeKeepWorkspace <- H.evalIO $ IO.lookupEnv "KEEP_WORKSPACE"
  let systemPrefixPath = systemTemp <> "/" <> prefixPath
  H.evalIO $ IO.createDirectoryIfMissing True systemPrefixPath
  ws <- H.evalIO $ IO.createTempDirectory systemPrefixPath "test"
  H.annotate $ "Workspace: " <> ws
  liftIO $ IO.writeFile (ws <> "/module") H.callerModuleName
  f ws
  when (IO.os /= "mingw32" && maybeKeepWorkspace /= Just "1") $ do
    H.evalIO $ IO.removeDirectoryRecursive ws

setDarwinTmpdir :: IO ()
setDarwinTmpdir = when (IO.os == "darwin") $ IO.setEnv "TMPDIR" "/tmp"

{- | Run a unit test with the hedgehog API in the @HE.'Integration'@ context,
with a temporary working directory.
-}
withUnitTestWithTmpDir
  :: (GHC.HasCallStack)
  => FilePath
  -- ^ Prefix path
  -> (FilePath -> HE.Integration ())
  -- ^ Test to run with temporary working directory
  -> H.Property
withUnitTestWithTmpDir prefixPath =
  H.withShrinks 0
    . HE.propertyOnce
    . (liftIO setDarwinTmpdir >>)
    . HE.runFinallies
    . workspace prefixPath

{- Failure-wrapping functions analogous to those of hedgehog-extras but in MonadIO
 - for greater flexibility. -}
nothingFail :: (MonadIO m) => String -> Maybe a -> m a
nothingFail err Nothing = liftIO $ throwIO $ errorCallException err
nothingFail _ (Just x) = pure x

leftFail :: (MonadIO m, Show e) => Either e a -> m a
leftFail (Left err) = liftIO $ throwIO $ errorCallException $ show err
leftFail (Right x) = pure x

leftFailM :: (MonadIO m, Show e) => m (Either e a) -> m a
leftFailM f = f >>= leftFail

eraInModeFailMsg :: String
eraInModeFailMsg = "Inconsistent arguments passed to Cardano.Api.toEraInMode"

{- Miscellaneous accessors -}

bimTxIds :: C.BlockInMode mode -> [C.TxId]
bimTxIds (C.BlockInMode block _) = blockTxIds block

blockTxIds :: C.Block era -> [C.TxId]
blockTxIds (C.Block (C.BlockHeader _slotNo _ _blockNo) txs) = map (C.getTxId . C.getTxBody) txs

bimSlotNo :: C.BlockInMode mode -> C.SlotNo
bimSlotNo (C.BlockInMode (C.Block (C.BlockHeader slotNo _ _blockNo) _txs) _era) = slotNo

bimBlockNo :: C.BlockInMode mode -> C.BlockNo
bimBlockNo (C.BlockInMode (C.Block (C.BlockHeader _slotNo _ blockNo) _txs) _era) = blockNo

-- | Should probably move in `cardano-api`.
txFeesExplicitInShelleyBasedEra
  :: C.ShelleyBasedEra era
  -> C.TxFeesExplicitInEra era
txFeesExplicitInShelleyBasedEra shelleyBased =
  case shelleyBased of
    C.ShelleyBasedEraShelley -> C.TxFeesExplicitInShelleyEra
    C.ShelleyBasedEraAllegra -> C.TxFeesExplicitInAllegraEra
    C.ShelleyBasedEraMary -> C.TxFeesExplicitInMaryEra
    C.ShelleyBasedEraAlonzo -> C.TxFeesExplicitInAlonzoEra
    C.ShelleyBasedEraBabbage -> C.TxFeesExplicitInBabbageEra
    C.ShelleyBasedEraConway -> C.TxFeesExplicitInConwayEra

addressAnyToShelley
  :: C.AddressAny
  -> Maybe (C.Address C.ShelleyAddr)
addressAnyToShelley (C.AddressShelley a) = Just a
addressAnyToShelley _ = Nothing
