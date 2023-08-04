{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Sidechain.Integration (tests) where

import Control.Concurrent qualified as IO
import Control.Lens ((<&>))
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Hedgehog (Property, assert)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Helpers qualified as Help
import Network.JsonRpc.Client.Types ()
import System.Environment qualified as IO
import System.IO qualified as IO
import System.Process qualified as IO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

integrationTest :: H.Integration () -> H.Property
integrationTest =
  H.withShrinks 0
    . H.withTests 1
    . H.propertyOnce
    . (liftIO Help.setDarwinTmpdir >>)
    . H.runFinallies

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
        (startMarconiSideChainTest optionalCliFlags "with-optional-cli-flags")
    ]

optionalCliFlags :: [String]
optionalCliFlags =
  [ "--http-port"
  , "6660"
  , "--min-indexing-depth"
  , "50"
  , -- "--max-indexing-depth", -- can only have min or max
    "--addresses-to-index"
  , "addr1x8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gt7r0vd4msrxnuwnccdxlhdjar77j6lg0wypcc9uar5d2shskhj42g"
  , "--addresses-to-index"
  , "addr1gx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5pnz75xxcrzqf96k"
  , "--match-asset-id"
  , "505755e49f9873b02258f1e6fdcc6c81446ab560a0e78180c600d9bd.deadbeef"
  , "--match-asset-id"
  , "505755e49f9873b02258f1e6fdcc6c81446ab560a0e78180c600d9bd"
  , "--fail-if-resyncing-from-genesis"
  ]

startMarconiSideChainTest :: [String] -> String -> Property
startMarconiSideChainTest optionalCliArgs description =
  integrationTest $ H.workspace ("start-marconi-sidechain-" <> description) $ \tempPath -> do
    nodeStdoutFile <- H.noteTempFile tempPath "node.stdout.log"
    nodeStderrFile <- H.noteTempFile tempPath "node.stderr.log"

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    let dbDir = tempPath <> "/marconi-databases"
    H.createDirectoryIfMissing_ dbDir
    nodeWorkingDir :: FilePath <- H.nothingFailM (liftIO $ IO.lookupEnv "CARDANO_NODE_WORKING_DIR")
    magicId <- liftIO $ IO.readFile $ nodeWorkingDir <> "/db/protocolMagicId"
    cp <-
      H.procFlex
        "marconi-sidechain"
        "MARCONI_SIDECHAIN"
        ( [ "--socket-path"
          , nodeWorkingDir <> "/ipc/node.socket"
          , "--node-config-path"
          , nodeWorkingDir <> "/config.json"
          , "--db-dir"
          , dbDir
          , "--testnet-magic"
          , magicId
          ]
            <> optionalCliArgs
        )
        <&> ( \cp ->
                cp
                  { IO.std_out = IO.UseHandle hNodeStdout
                  , IO.std_err = IO.UseHandle hNodeStderr
                  }
            )

    -- start marconi
    (_mStdin, _mStdout, _mStderr, pHandle, _releaseKey) <- H.createProcess cp

    -- wait for stdout log to show successful synchronising or fail after 30 seconds
    logsSynchronising <- liftIO $ waitForLog nodeStdoutFile 30 "Synchronising"
    unless logsSynchronising H.failure

    -- check that each db file exists and ledgerStates dir is not empty
    utxoDbFileExists <- H.doesFileExists $ dbDir <> "/utxo.db"
    mintBurnDbFileExists <- H.doesFileExists $ dbDir <> "/mintburn.db"
    epochStateDbFileExists <- H.doesFileExists $ dbDir <> "/epochstate.db"
    ledgerStateDirIsNotEmpty <- not . null <$> H.listDirectory (dbDir <> "/ledgerStates")
    assert $
      all
        (== True)
        [utxoDbFileExists, mintBurnDbFileExists, epochStateDbFileExists, ledgerStateDirIsNotEmpty]

    -- terminate process with SIGINT (Ctrl+C)
    H.evalIO $ IO.interruptProcessGroupOf pHandle

    -- check correct log output on shutdown
    stopLogContainsShuttingDown <-
      H.evalIO $
        waitForLog nodeStdoutFile 5 "Marconi is shutting down. Waiting for indexers to finish their work..."
    -- may need to wait up to 3 minutes for "Done!" to be logged due to timeout
    stopLogContainsDone <- H.evalIO $ waitForLog nodeStdoutFile 185 "Done!"
    H.annotate =<< liftIO (IO.readFile nodeStdoutFile) -- Debug
    assert $ all (== True) [stopLogContainsShuttingDown, stopLogContainsDone]

    _mExitCode <- H.evalIO $ IO.getProcessExitCode pHandle
    -- mExitCode === Just IO.ExitSuccess -- skipping failing assertion, see PLT-7045

    -- check for no errors exist in stderr
    assert . null =<< liftIO (IO.readFile nodeStderrFile)

-- TODO: re-start marconi and check recovery

waitForLog :: FilePath -> Int -> String -> IO Bool
waitForLog file t str
  | t <= 0 = return False
  | otherwise = do
      contents <- IO.readFile file
      if str `isInfixOf` contents
        then return True
        else do
          IO.threadDelay 1_000_000 -- wait 1 second
          waitForLog file (t - 1) str
