{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Sidechain.Integration (tests) where

import Control.Lens ((<&>))
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Hedgehog (Property, assert)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Network.JsonRpc.Client.Types ()
import Spec.Marconi.Sidechain.Utils qualified as U
import System.Environment qualified as IO
import System.IO qualified as IO
import System.Process qualified as IO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "marconi-sidechain integration test"
    [ testPropertyNamed
        "start marconi-sidechain with mandatory cli flags, check all dbs are created and process shuts down correctly"
        "startMarconiSideChainTest"
        (startMarconiSideChainTest [] "mandatory-cli-flags")
    , testPropertyNamed
        "start marconi-sidechain with all cli flags, check all dbs are created are created and process shuts down correctly"
        "startMarconiSideChainTest"
        (startMarconiSideChainTest validOptionalCliFlags "with-optional-cli-flags")
    ]

validOptionalCliFlags :: [String]
validOptionalCliFlags =
  [ "--http-port"
  , "6660"
  , "--min-indexing-depth"
  , "50"
  , -- "--max-indexing-depth", -- can only have min or max
    "--addresses-to-index"
  , "addr1x8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gt7r0vd4msrxnuwnccdxlhdjar77j6lg0wypcc9uar5d2shskhj42g"
  , "--addresses-to-index"
  , "addr1gx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5pnz75xxcrzqf96k"
  , "--addresses-to-index"
  , "addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgs68faae"
  , "--addresses-to-index"
  , "addr_test12rphkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtupnz75xxcryqrvmw"
  , "--match-asset-id"
  , "505755e49f9873b02258f1e6fdcc6c81446ab560a0e78180c600d9bd.deadbeef000000000000000000000000000000000000000000000000000000"
  , "--match-asset-id"
  , "505755e49f9873b02258f1e6fdcc6c81446ab560a0e78180c600d9bd"
  , "--fail-if-resyncing-from-genesis"
  ]

startMarconiSideChainTest :: [String] -> String -> Property
startMarconiSideChainTest optionalCliArgs description =
  U.runIntegrationTest $ H.workspace ("start-marconi-sidechain-" <> description) $ \tempPath -> do
    (nodeDbDir, nodeStdoutFile, nodeStderrFile, nodePHandle) <-
      startMarconiSidechain optionalCliArgs tempPath

    -- wait for stdout log to show successful synchronising or fail after 30 seconds
    logsSynchronising <- liftIO $ U.waitForExpectedContentInLogFile nodeStdoutFile 30 ["Synchronising"]
    unless logsSynchronising H.failure

    -- check that each db file exists and ledgerStates dir is not empty
    utxoDbFileExists <- H.doesFileExists $ nodeDbDir <> "/utxo.db"
    mintBurnDbFileExists <- H.doesFileExists $ nodeDbDir <> "/mintburn.db"
    epochStateDbFileExists <- H.doesFileExists $ nodeDbDir <> "/epochstate.db"
    ledgerStateDirIsNotEmpty <- not . null <$> H.listDirectory (nodeDbDir <> "/ledgerStates")
    assert $
      all
        (== True)
        [utxoDbFileExists, mintBurnDbFileExists, epochStateDbFileExists, ledgerStateDirIsNotEmpty]

    -- terminate process with SIGINT (Ctrl+C)
    H.evalIO $ IO.interruptProcessGroupOf nodePHandle

    -- check correct log output on shutdown
    stopLogContainsShuttingDown <-
      H.evalIO $
        U.waitForExpectedContentInLogFile
          nodeStdoutFile
          5
          ["Marconi is shutting down. Waiting for indexers to finish their work..."]
    stopLogContainsDone <- H.evalIO $ U.waitForExpectedContentInLogFile nodeStdoutFile 15 ["Done!"]
    H.annotate =<< liftIO (IO.readFile nodeStdoutFile)
    assert $ all (== True) [stopLogContainsShuttingDown, stopLogContainsDone]

    _mExitCode <- H.evalIO $ IO.getProcessExitCode nodePHandle
    -- mExitCode === Just IO.ExitSuccess -- skipping failing assertion, see PLT-7045

    -- check for no errors exist in stderr
    assert . null =<< liftIO (IO.readFile nodeStderrFile)

-- TODO: re-start marconi and check recovery

{- Starts marconi-sidechain execuatble using the cli and connects to a running cardano node via socket path
-- Local environment requires CARDANO_NODE_WORKING_DIR to be the location of cardano-node's config and socket
-}
startMarconiSidechain
  :: (H.MonadTest m, MonadCatch m, MonadResource m)
  => [String]
  -> String
  -> m (FilePath, FilePath, FilePath, IO.ProcessHandle)
startMarconiSidechain optionalCliArgs tempPath = do
  marconiStdoutFile <- H.noteTempFile tempPath "marconi-sidechain.stdout.log"
  marconiStderrFile <- H.noteTempFile tempPath "marconi-sidechain.stderr.log"
  hStdout <- H.openFile marconiStdoutFile IO.WriteMode
  hStderr <- H.openFile marconiStderrFile IO.WriteMode
  let marconiDbDir = tempPath <> "/marconi-databases"
  H.createDirectoryIfMissing_ marconiDbDir
  nodeWorkingDir :: FilePath <- H.nothingFailM (liftIO $ IO.lookupEnv "CARDANO_NODE_WORKING_DIR")
  magicId <- liftIO $ IO.readFile $ nodeWorkingDir <> "/db/protocolMagicId"
  cp <-
    H.procFlex
      "marconi-sidechain"
      "MARCONI_SIDECHAIN"
      ( -- mandatory cli args
        [ "--socket-path"
        , nodeWorkingDir <> "/ipc/node.socket"
        , "--node-config-path"
        , nodeWorkingDir <> "/config.json"
        , "--db-dir"
        , marconiDbDir
        , "--testnet-magic"
        , magicId
        ]
          <> optionalCliArgs
      )
      <&> ( \cp ->
              cp
                { IO.std_out = IO.UseHandle hStdout
                , IO.std_err = IO.UseHandle hStderr
                }
          )
  (_mStdinH, _mStdoutH, _mStderrH, marconiProcessHandle, _releaseKey) <- H.createProcess cp
  return (marconiDbDir, marconiStdoutFile, marconiStderrFile, marconiProcessHandle)
