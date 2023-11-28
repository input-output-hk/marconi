{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Helpers where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Stack qualified as GHC
import Hedgehog (MonadTest)
import Hedgehog qualified as H
import Hedgehog.Extras.Stock.CallStack qualified as H
import Hedgehog.Extras.Test qualified as HE
import System.Directory qualified as IO
import System.Environment qualified as IO
import System.IO qualified as IO
import System.IO.Temp qualified as IO
import System.Info qualified as IO
import Test.Gen.Marconi.Cardano.Core.Helpers qualified as Core.Helpers

{- Protocol / transaction helpers -}

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
        Core.Helpers.calculateFee
          ledgerPP
          lengthTxIns
          (length $ C.txOuts txbc)
          0
          lengthKeyWitnesses
          networkId
          txb
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
unitTestWithTmpDir
  :: (GHC.HasCallStack)
  => FilePath
  -- ^ Prefix path
  -> (FilePath -> HE.Integration ())
  -- ^ Test to run with temporary working directory
  -> H.Property
unitTestWithTmpDir prefixPath =
  H.withShrinks 0
    . HE.propertyOnce
    . (liftIO setDarwinTmpdir >>)
    . HE.runFinallies
    . workspace prefixPath
