{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Cardano.Snapshot (tests) where

import Cardano.Api.Extended.Streaming (BlockEvent)
import Cardano.Api.Shelley qualified as C
import Control.Concurrent (readMVar)
import Control.Concurrent qualified as Concurrent
import Control.Concurrent.Async (wait, withAsync)
import Control.Lens qualified as Lens
import Control.Monad.Except (ExceptT (ExceptT), MonadTrans (lift))
import Control.Monad.Trans.Except (runExceptT)
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance (WithDistance))
import Marconi.Cardano.Core.Extract.WithDistance qualified as WithDistance
import Marconi.Cardano.Core.Indexer.Worker (
  StandardWorker (StandardWorker),
  StandardWorkerConfig (StandardWorkerConfig),
  mkStandardWorker,
 )
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Runner (RunSnapshotIndexerConfig)
import Marconi.Cardano.Core.Runner qualified as Core
import Marconi.Cardano.Indexers (buildBlockEventCoordinator)
import Marconi.Cardano.Indexers.ExtLedgerStateCoordinator (
  ExtLedgerStateCoordinator,
  ExtLedgerStateCoordinatorConfig (ExtLedgerStateCoordinatorConfig),
  ExtLedgerStateEvent,
  ExtLedgerStateWorkerConfig (ExtLedgerStateWorkerConfig),
  extLedgerStateWorker,
  mkExtLedgerStateCoordinator,
  readGenesisFile,
 )
import Marconi.Core qualified as Core
import Marconi.Core.Indexer.ListIndexer qualified as ListIndexer
import Marconi.Core.Preprocessor qualified as Preprocessor
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
    -- [testRunSnapshotIndexer]
    []

-- TODO:
--   - Test1: run just with listindexer, see if block events from stream = block events from indexer
--   - Test2: run with ledgerstate coordinator which has list indexer underneath, and calls
--   buildNextExtLedgerStateEvent to fold the list; test2 checks whether the last ledger state is
--   equal to the fold

-- testRunSnapshotIndexer :: TestTree
-- testRunSnapshotIndexer =
--   testCase "TestRunSnapshotIndexer" run
--   where
--     run = do
--       let dbPath = "test/Spec/Golden/Snapshot/preprod-5-10-db/"
--           config =
--             Core.RunSnapshotIndexerConfig
--               Core.withNoPreprocessorOnSnapshot
--               1 -- why does it get stuck if it's 0?
--       configFile <- getNodeConfigPath "preprod"
--       blockStream <- setupSnapshot configFile "test/Spec/Golden/Snapshot/preprod-5-10" dbPath
--       let listIndexer = Core.mkListIndexer
--       actualResult <-
--         withAsync (Core.runSnapshotIndexer config listIndexer blockStream) $ \runner -> do
--           mVar <- wait runner
--           indexer <- readMVar mVar
--           removeDirectoryRecursive dbPath
--           return (Lens.view ListIndexer.events indexer)
--       expectedResult <- Stream.toList_ blockStream
--       assertEqual "" (show <$> expectedResult) (reverse $ show . Lens.view event <$> actualResult)

type instance Core.Point (ExtLedgerStateEvent, WithDistance BlockEvent) = C.ChainPoint

-- TODO: fix type error, I think it's because instead of WithDistance BlockEvent we have BlockEvent
-- which means that Core.IsIndex (ExceptT Core.IndexerError IO) event indexer isn't true

-- TODO: use last event indexer?
testRunSnapshotIndexer2 :: TestTree
testRunSnapshotIndexer2 =
  testCase "TestRunSnapshotIndexer2" run
  where
    run = do
      let dbPath = "test/Spec/Golden/Snapshot/preprod-5-10-db/"
          config =
            Core.RunSnapshotIndexerConfig
              (Core.withDistanceAndTipPreprocessorOnSnapshot $ C.ChainTip undefined undefined 10)
              1 -- why does it get stuck if it's 0?
      configFile <- getNodeConfigPath "preprod"
      blockStream <- setupSnapshot configFile "test/Spec/Golden/Snapshot/preprod-5-10" dbPath
      genesisConfig <- toRuntimeException $ readGenesisFile configFile
      let trace = undefined
          ledgerConfig =
            ExtLedgerStateCoordinatorConfig
              (WithDistance.getEvent . snd)
              genesisConfig
              1
              1
          ledgerWorkerConfig :: ExtLedgerStateWorkerConfig IO (WithDistance BlockEvent)
          ledgerWorkerConfig =
            ExtLedgerStateWorkerConfig
              WithDistance.getEvent
              trace
              configFile
              100
              1
      Core.WorkerIndexer listIndexerMVar listWorker <-
        Core.createWorkerWithPreprocessing
          "TestListIndexerWorker"
          (Preprocessor.mapEvent id)
          ListIndexer.mkListIndexer
      -- (coordinator :: ExtLedgerStateCoordinator (ExtLedgerStateEvent, WithDistance BlockEvent)) <-
      --   toRuntimeException $ mkExtLedgerStateCoordinator ledgerConfig dbPath [listWorker]
      Core.WorkerIndexer _ ledgerCoordinatorWorker <-
        toRuntimeException $ extLedgerStateWorker ledgerWorkerConfig [listWorker] dbPath
      blockCoordinator <-
        buildBlockEventCoordinator
          trace
          [ledgerCoordinatorWorker]
      actualResult <-
        -- TODO: how do I create a config with the right type?
        withAsync (Core.runSnapshotIndexer config blockCoordinator blockStream) $ \runner -> do
          _ <- wait runner
          indexer <- readMVar listIndexerMVar
          removeDirectoryRecursive dbPath
          return (Lens.view ListIndexer.events indexer)
      putStrLn (show $ Lens.view event <$> actualResult)

runIndexer
  :: RunSnapshotIndexerConfig BlockEvent (ExtLedgerStateEvent, WithDistance BlockEvent)
  -> ExtLedgerStateCoordinator (ExtLedgerStateEvent, WithDistance BlockEvent)
  -> Stream.Stream (Stream.Of BlockEvent) IO ()
  -> IO (Concurrent.MVar (ExtLedgerStateCoordinator (ExtLedgerStateEvent, WithDistance BlockEvent)))
runIndexer = Core.runSnapshotIndexer

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
