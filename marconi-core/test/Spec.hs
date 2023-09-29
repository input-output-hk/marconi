import Test.Tasty (TestTree, defaultMain, testGroup)

import Marconi.CoreSpec qualified as Core

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Marconi.Core"
    [ testGroup
        "Indexing"
        [ Core.indexingTestGroup "ListIndexer" Core.listIndexerRunner
        , Core.indexingTestGroup "SQLiteIndexer" Core.mkSqliteIndexerRunner
        , Core.indexingTestGroup "MixedIndexer - low memory" Core.mixedLowMemoryIndexerRunner
        , Core.indexingTestGroup "MixedIndexer - high memory" Core.mixedHighMemoryIndexerRunner
        , Core.indexingTestGroup "WithTracer" $ Core.withTracerRunner Core.listIndexerRunner
        , Core.indexingTestGroup "Coordinator" $ Core.coordinatorIndexerRunner Core.listIndexerRunner
        ]
    , Core.cacheTestGroup
    , testGroup
        "WithDelay"
        [ Core.delayTestGroup "ListIndexer" Core.listIndexerRunner
        , Core.delayTestGroup "SQLiteIndexer" Core.mkSqliteIndexerRunner
        ]
    , testGroup
        "WithCatchup"
        [ Core.catchupTestGroup "ListIndexer" Core.listIndexerRunner
        , Core.catchupTestGroup "SQLiteIndexer" Core.mkSqliteIndexerRunner
        ]
    , testGroup
        "WithTransform"
        [ Core.withTransformTest
        ]
    , testGroup
        "WithAggregate"
        [ Core.withAggregateTest
        ]
    , testGroup
        "WithResume"
        [ Core.withResumeTest
        ]
    , testGroup
        "Performance"
        [ Core.indexingPerformanceTest "ListIndexer" Core.listIndexerRunner
        , Core.indexingPerformanceTest "MixedIndexer" Core.mixedHighMemoryIndexerRunner
        ]
    , testGroup
        "Error handling"
        [ Core.stopCoordinatorTest Core.listIndexerRunner
        , Core.withRollbackFailureTest
        ]
    , testGroup
        "Resuming last synced points"
        [ Core.resumeSQLiteLastSyncTest
        , Core.resumeMixedLastSyncTest
        ]
    , testGroup
        "Configuration"
        [ Core.memorySizeUpdateTest
        ]
    ]
