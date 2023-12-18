{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Cardano.Snapshot (tests) where

import Control.Concurrent (readMVar)
import Control.Concurrent.Async (wait, withAsync)
import Control.Lens qualified as Lens
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Runner qualified as Core
import Marconi.Cardano.Indexers ()
import Marconi.Core qualified as Core
import Marconi.Core.Indexer.ListIndexer qualified as ListIndexer
import System.Environment (getEnv)
import System.FilePath ((</>))
import Test.Marconi.Cardano.Snapshot (
  setupSnapshot,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.Cardano.Snapshot"
    -- [ testDeserialiseSnapshot
    -- , testRunSnapshotIndexer
    -- ]
    [testRunSnapshotIndexer]

-- testDeserialiseSnapshot :: TestTree
-- testDeserialiseSnapshot =
--   goldenVsFileDiff
--     "TestDeserialiseSnapshot"
--     (\expected actual -> ["diff", "--color=always", expected, actual])
--     "test/Spec/Golden/Snapshot/preprod-5-10-subchain.golden"
--     "test/Spec/Golden/Snapshot/preprod-5-10-subchain.out"
--     run
--   where
--     run = do
--       configFile <- getNodeConfigPath "preprod"
--       snapshot <- internaliseSnapshot configFile "test/Spec/Golden/Snapshot/preprod-5-10"
--       withFile "test/Spec/Golden/Snapshot/preprod-5-10-subchain.out" WriteMode $ \handle -> do
--         hPrint handle (snapshotPreviousLedgerState snapshot)
--         Stream.toHandle handle (Stream.map show $ snapshotBlockStream snapshot)

-- TODO:
--   - Test1: run just with listindexer, see if block events from stream = block events from indexer
--   - Test2: run with ledgerstate coordinator which has list indexer underneath

testRunSnapshotIndexer :: TestTree
testRunSnapshotIndexer =
  goldenVsFileDiff
    "TestRunSnapshotIndexer"
    (\expected actual -> ["diff", "--color=always", expected, actual])
    "test/Spec/Golden/Snapshot/preprod-5-10-index.golden"
    "test/Spec/Golden/Snapshot/preprod-5-10-index.out"
    run
  where
    run = do
      let dbPath = "test/Spec/Golden/Snapshot/preprod-5-10-db/"
          config =
            Core.RunSnapshotIndexerConfig
              Core.withNoPreprocessorOnSnapshot
              0
      configFile <- getNodeConfigPath "preprod"
      blockStream <- setupSnapshot configFile "test/Spec/Golden/Snapshot/preprod-5-10" dbPath
      let listIndexer = Core.mkListIndexer
      result <-
        withAsync (Core.runSnapshotIndexer config listIndexer blockStream) $ \runner -> do
          mVar <- wait runner
          indexer <- readMVar mVar
          return (Lens.view ListIndexer.events indexer)
      putStrLn (show result)

getNodeConfigPath :: String -> IO FilePath
getNodeConfigPath nodeType = do
  configDir <- getEnv "CARDANO_NODE_CONFIG"
  return $ configDir </> "cardano-node" </> nodeType </> "config.json"
