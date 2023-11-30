{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Sidechain.Integration (tests) where

import Cardano.Api qualified as C
import Control.Lens ((<&>))
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Hedgehog (Property, assert, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Helpers qualified as Help
import Network.JsonRpc.Client.Types ()
import Spec.Marconi.Sidechain.Utils qualified as U
import System.Exit qualified as IO
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
    (_, networkId, socketPath) <- Help.startTestnet tempPath 1000

    (marconiDbDir, marconiStdoutFile, marconiStderrFile, marconiPHandle) <-
      startMarconiSidechain optionalCliArgs tempPath socketPath networkId

    -- wait for stdout log to show successful synchronising or fail after 30 seconds
    logsSynchronising <-
      liftIO $
        U.waitForExpectedContentInLogFile marconiStdoutFile 30 ["Synchronising", "Fully synchronised."]
    unless logsSynchronising H.failure

    socketExists <- H.doesFileExists socketPath
    assert socketExists

    -- check that each db file exists
    utxoDbFileExists <- H.doesFileExists $ marconiDbDir <> "/utxo.db"
    mintBurnDbFileExists <- H.doesFileExists $ marconiDbDir <> "/mintburn.db"
    assert $
      all
        (== True)
        [utxoDbFileExists, mintBurnDbFileExists]

    -- terminate process with SIGINT (Ctrl+C)
    H.evalIO $ IO.interruptProcessGroupOf marconiPHandle

    -- check correct log output on shutdown
    stopLogContainsShuttingDown <-
      H.evalIO $
        U.waitForExpectedContentInLogFile
          marconiStdoutFile
          5
          ["Stopping indexing. Waiting for indexers to finish their work"]
    stopLogContainsDone <- H.evalIO $ U.waitForExpectedContentInLogFile marconiStdoutFile 15 ["Done!"]
    H.annotate =<< liftIO (IO.readFile marconiStdoutFile)
    assert $ all (== True) [stopLogContainsShuttingDown, stopLogContainsDone]

    mExitCode <- H.evalIO $ IO.getProcessExitCode marconiPHandle
    mExitCode === Just (IO.ExitFailure 130)

    -- check for no errors exist in stderr
    assert . null =<< liftIO (IO.readFile marconiStderrFile)

-- TODO: re-start marconi and check recovery

{- Starts marconi-sidechain execuatble using the cli and connects to a running cardano node via socket path
-- Local environment requires CARDANO_NODE_WORKING_DIR to be the location of cardano-node's config and socket
-}
startMarconiSidechain
  :: (H.MonadTest m, MonadCatch m, MonadResource m)
  => [String]
  -> FilePath
  -> FilePath
  -> C.NetworkId
  -> m (FilePath, FilePath, FilePath, IO.ProcessHandle)
startMarconiSidechain optionalCliArgs tempPath socketPath networkId = do
  magicId <- case networkId of
    C.Testnet (C.NetworkMagic magic) -> pure $ show magic
    _ -> do
      H.annotate "Expected a testnet network ID"
      H.failure

  let nodeConfigFile = "../config/cardano-node/preview/config.json"
  marconiStdoutFile <- H.noteTempFile tempPath "marconi-sidechain.stdout.log"
  marconiStderrFile <- H.noteTempFile tempPath "marconi-sidechain.stderr.log"
  hStdout <- H.openFile marconiStdoutFile IO.WriteMode
  hStderr <- H.openFile marconiStderrFile IO.WriteMode
  let marconiDbDir = tempPath <> "/marconi-databases"
  H.createDirectoryIfMissing_ marconiDbDir
  cp <-
    H.procFlex
      "marconi-sidechain"
      "MARCONI_SIDECHAIN"
      ( -- mandatory cli args
        [ "--socket-path"
        , socketPath
        , "--node-config-path"
        , nodeConfigFile
        , "--db-dir"
        , marconiDbDir
        , "--testnet-magic"
        , magicId
        , -- because the epoch state indexer is too strict for the cardano node emulator:
          "--disable-epoch-stakepool-size"
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
