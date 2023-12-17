{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.Cardano.Snapshot (tests) where

import Cardano.Api.Extended qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent)
import Control.Arrow ((<<<))
import Control.Monad.Except (ExceptT, MonadError, MonadTrans (lift), runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance)
import Marconi.Cardano.Core.Extract.WithDistance qualified as Distance
import Marconi.Cardano.Core.Indexer.Worker qualified as Core
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Runner qualified as Core
import Marconi.Cardano.Indexers ()
import Marconi.Cardano.Indexers.ExtLedgerStateCoordinator qualified as ExtLedgerStateCoordinator
import Marconi.Core qualified as Core
import Marconi.Core.Preprocessor qualified as Core
import Streaming.Prelude qualified as Stream
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hPrint, withFile)
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
      eCoordinator <-
        runExceptT $
          buildIndexer (ledgerConfig configFile) dbPath
      case eCoordinator of
        Left err -> error (show err)
        Right coordinator ->
          Core.runSnapshotIndexer config coordinator blockStream

getNodeConfigPath :: String -> IO FilePath
getNodeConfigPath nodeType = do
  configDir <- getEnv "CARDANO_NODE_CONFIG"
  return $ configDir </> "cardano-node" </> nodeType </> "config.json"

type TopCoordinator =
  Core.WorkerM
    IO
    (WithDistance BlockEvent)
    C.ChainPoint -- (Core.Point (ExtLedgerStateCoordinator.ExtLedgerStateEvent, WithDistance BlockEvent))

newtype TestEvent = TestEvent C.BlockNo

type instance Core.Point TestEvent = C.ChainPoint

buildIndexer
  :: ExtLedgerStateCoordinator.ExtLedgerStateWorkerConfig IO (WithDistance BlockEvent)
  -> FilePath
  -> ExceptT Core.IndexerError IO TopCoordinator
buildIndexer epochStateConfig dbPath = do
  Core.WorkerIndexer _ testWorker' <-
    buildTestIndexer
  Core.WorkerIndexer _epochStateMVar epochStateWorker <-
    ExtLedgerStateCoordinator.extLedgerStateWorker
      epochStateConfig
      [testWorker']
      dbPath
  return epochStateWorker

buildTestIndexer
  :: ExceptT
      Core.IndexerError
      IO
      ( Core.WorkerIndexer
          IO
          (ExtLedgerStateCoordinator.ExtLedgerStateEvent, WithDistance BlockEvent)
          TestEvent
          Core.ListIndexer
      )
buildTestIndexer =
  let testWorkerConfig =
        Core.StandardWorkerConfig
          "test"
          0
          undefined
          (pure . Just . TestEvent . ExtLedgerStateCoordinator.blockNo . fst)
          undefined -- (BM.appendName indexerName indexerEventLogger)
   in testWorker testWorkerConfig

testWorker
  :: (MonadIO io, Monad m)
  => Core.StandardWorkerConfig m input TestEvent
  -> io (Core.WorkerIndexer m input TestEvent Core.ListIndexer)
testWorker workerConfig = do
  let indexer = Core.mkListIndexer
      preprocessor =
        Core.traverseMaybeEvent (lift . Core.eventExtractor workerConfig)
          <<< getBlockNoPreprocessor id
  Core.createWorkerWithPreprocessing "test" preprocessor indexer

getBlockNoPreprocessor :: (Monad m) => (a -> b) -> Core.Preprocessor m point a b
getBlockNoPreprocessor h =
  let g = pure . Just . h
   in Core.scanMaybeEvent g Nothing

ledgerConfig nodeConfigPath =
  ExtLedgerStateCoordinator.ExtLedgerStateWorkerConfig
    Distance.getEvent
    undefined -- trace
    nodeConfigPath
    0 -- volatileEpochStateSnapshotInterval
    0 -- securityParam
