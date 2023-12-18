{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Cardano.Snapshot (tests) where

import Cardano.Api.Extended.Streaming (BlockEvent)
import Cardano.Api.Shelley qualified as C
import Control.Concurrent (readMVar)
import Control.Concurrent.Async (wait, withAsync)
import Control.Lens qualified as Lens
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Except (runExceptT)
import Marconi.Cardano.Core.Indexer.Worker (
  StandardWorker (StandardWorker),
  StandardWorkerConfig (StandardWorkerConfig),
  mkStandardWorker,
 )
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Runner qualified as Core
import Marconi.Cardano.Indexers ()
import Marconi.Cardano.Indexers.ExtLedgerStateCoordinator (
  ExtLedgerStateCoordinatorConfig (ExtLedgerStateCoordinatorConfig),
  ExtLedgerStateEvent,
  mkExtLedgerStateCoordinator,
  readGenesisFile,
 )
import Marconi.Core qualified as Core
import Marconi.Core.Indexer.ListIndexer qualified as ListIndexer
import Marconi.Core.Type (event)
import Streaming.Prelude qualified as Stream
import System.Directory (removeDirectoryRecursive)
import System.Environment (getEnv)
import System.FilePath ((</>))
import Test.Marconi.Cardano.Snapshot (
  setupSnapshot,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.Cardano.Snapshot"
    [testRunSnapshotIndexer]

-- TODO:
--   - Test1: run just with listindexer, see if block events from stream = block events from indexer
--   - Test2: run with ledgerstate coordinator which has list indexer underneath

testRunSnapshotIndexer :: TestTree
testRunSnapshotIndexer =
  testCase "TestRunSnapshotIndexer" run
  where
    run = do
      let dbPath = "test/Spec/Golden/Snapshot/preprod-5-10-db/"
          config =
            Core.RunSnapshotIndexerConfig
              Core.withNoPreprocessorOnSnapshot
              1 -- why does it get stuck if it's 0?
      configFile <- getNodeConfigPath "preprod"
      blockStream <- setupSnapshot configFile "test/Spec/Golden/Snapshot/preprod-5-10" dbPath
      let listIndexer = Core.mkListIndexer
      actualResult <-
        withAsync (Core.runSnapshotIndexer config listIndexer blockStream) $ \runner -> do
          mVar <- wait runner
          indexer <- readMVar mVar
          removeDirectoryRecursive dbPath
          return (Lens.view ListIndexer.events indexer)
      expectedResult <- Stream.toList_ blockStream
      assertEqual "" (show <$> expectedResult) (reverse $ show . Lens.view event <$> actualResult)

type instance Core.Point (ExtLedgerStateEvent, BlockEvent) = C.ChainPoint

testRunSnapshotIndexer2 :: TestTree
testRunSnapshotIndexer2 =
  testCase "TestRunSnapshotIndexer2" run
  where
    run = do
      let dbPath = "test/Spec/Golden/Snapshot/preprod-5-10-db/"
          config =
            Core.RunSnapshotIndexerConfig
              Core.withNoPreprocessorOnSnapshot
              1 -- why does it get stuck if it's 0?
      configFile <- getNodeConfigPath "preprod"
      blockStream <- setupSnapshot configFile "test/Spec/Golden/Snapshot/preprod-5-10" dbPath
      genesisConfig <- toRuntimeException $ readGenesisFile configFile
      let ledgerConfig =
            ExtLedgerStateCoordinatorConfig
              snd
              genesisConfig
              1
              1
      Core.WorkerIndexer listIndexerMVar listWorker <-
        Core.createWorker "TestListIndexerWorker" Just ListIndexer.mkListIndexer
      coordinator <- toRuntimeException $ mkExtLedgerStateCoordinator ledgerConfig dbPath [listWorker]
      actualResult <-
        withAsync (Core.runSnapshotIndexer config coordinator blockStream) $ \runner -> do
          _ <- wait runner
          indexer <- readMVar listIndexerMVar
          removeDirectoryRecursive dbPath
          return (Lens.view ListIndexer.events indexer)
      putStrLn (show $ Lens.view event <$> actualResult)

-- expectedResult <- Stream.toList_ blockStream
-- assertEqual "" (show <$> expectedResult) (reverse $ show . Lens.view event <$> actualResult)

getNodeConfigPath :: String -> IO FilePath
getNodeConfigPath nodeType = do
  configDir <- getEnv "CARDANO_NODE_CONFIG"
  return $ configDir </> "cardano-node" </> nodeType </> "config.json"

toRuntimeException :: (Monad m, Show e) => ExceptT e m b -> m b
toRuntimeException action = do
  e <- runExceptT action
  case e of
    Left err -> error . show $ err
    Right r -> return r
