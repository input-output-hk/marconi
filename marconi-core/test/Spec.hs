import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty, withMaxSuccess)

import Marconi.Core.Model qualified as Ix
import Marconi.Core.Spec.Experiment qualified as E
import Marconi.Core.Spec.Sqlite qualified as S
import Marconi.Core.Spec.TracedSqlite qualified as TS
import Marconi.Core.Trace qualified as Ix

tests :: TestTree
tests = testGroup "Everything" [ indexTests, traceTests, experimentTests ]

indexTests :: TestTree
indexTests = testGroup "Index" [ ixProperties, sProperties, tProperties ]

traceTests :: TestTree
traceTests = testGroup "Trace" [ traceModelProperties, traceIndexerProperties ]

experimentTests :: TestTree
experimentTests = testGroup "Experiment"
    [ testGroup "Indexing"
        [ E.indexingTestGroup "ListIndexer" E.listIndexerRunner
        , E.indexingTestGroup "SQLiteIndexer" E.sqliteIndexerRunner
        , E.indexingTestGroup "MixedIndexer - low memory" E.mixedLowMemoryIndexerRunner
        , E.indexingTestGroup "MixedIndexer - high memory" E.mixedHighMemoryIndexerRunner
        , E.indexingTestGroup "WithTracer" $ E.withTracerRunner E.listIndexerRunner
        , E.indexingTestGroup "Coordinator" $ E.coordinatorIndexerRunner E.listIndexerRunner
        ]
    , E.cacheTestGroup
    , testGroup "WithDelay"
        [ E.delayTestGroup "ListIndexer" E.listIndexerRunner
        , E.delayTestGroup "SQLiteIndexer" E.sqliteIndexerRunner
        ]
    , testGroup "WithTransform"
        [ E.withTransformTest
        ]
    , testGroup "WithAggregate"
        [ E.withAggregateTest
        ]
    , testGroup "Performance"
        [ E.indexingPerformanceTest "ListIndexer" E.listIndexerRunner
        , E.indexingPerformanceTest "MixedIndexer" E.mixedHighMemoryIndexerRunner
        ]
    , testGroup "Error handling"
        [ E.stopCoordinatorTest  E.listIndexerRunner
        ]
    , testGroup "Resuming"
        [ E.resumeSQLiteLastSyncTest E.sqliteIndexerRunner
        , E.resumeMixedLastSyncTest E.mixedNoMemoryIndexerRunner
        ]
    , testGroup "Configuration"
        [ E.memorySizeUpdateTest
        ]
    ]

traceModelProperties :: TestTree
traceModelProperties = testGroup "Model traces"
  [ testProperty "Weak bisimilarity (observed builder)" $
      withMaxSuccess 10000 $ Ix.prop_WeakBisimilarity  @Int @Int @Int Ix.modelConversion
  , testProperty "Weak bisimilarity (grammar builder)" $
      withMaxSuccess 300  $ Ix.prop_WeakBisimilarity' @Int @Int @Int Ix.modelConversion
  ]

traceIndexerProperties :: TestTree
traceIndexerProperties = testGroup "Implementation traces"
  [ testProperty "Weak bisimilarity (observed builder)" $
      withMaxSuccess 10000 $ Ix.prop_WeakBisimilarity  TS.observeTrace
  , testProperty "Weak bisimilarity (grammar builder)" $
      withMaxSuccess 300  $ Ix.prop_WeakBisimilarity' TS.observeTrace
  ]

ixProperties :: TestTree
ixProperties = testGroup "Basic model"
  [ testProperty "New: Positive or non-positive depth" $
      withMaxSuccess 10000 $ Ix.prop_observeNew @Int @Int @Int Ix.conversion
  , testProperty "History length is always smaller than the max depth" $
      withMaxSuccess 10000 $ Ix.prop_sizeLEDepth @Int @Int @Int Ix.conversion
  , testProperty "Rewind: Connection with `ixDepth`" $
      withMaxSuccess 10000 $ Ix.prop_rewindDepth @Int @Int @Int Ix.conversion
  , testProperty "Relationship between Insert/Rewind" $
      withMaxSuccess 10000 $ Ix.prop_insertRewindInverse @Int @Int @Int Ix.conversion
  , testProperty "Insert is folding the structure" $
      withMaxSuccess 10000 $ Ix.prop_observeInsert @Int @Int @Int Ix.conversion
  ]

sProperties :: TestTree
sProperties = testGroup "New index properties."
  [ testProperty "New: Positive or non-positive depth" $
      withMaxSuccess 10000 $ Ix.prop_observeNew @Int @Int S.conversion
  , testProperty "History length is always smaller than the max depth" $
      withMaxSuccess 1000 $ Ix.prop_sizeLEDepth @Int @Int S.conversion
  , testProperty "Rewind: Connection with `ixDepth`" $
      withMaxSuccess 1000 $ Ix.prop_rewindDepth @Int @Int S.conversion
  , testProperty "Relationship between Insert/Rewind" $
      withMaxSuccess 1000 $ Ix.prop_insertRewindInverse @Int @Int S.conversion
  , testProperty "Insert is folding the structure" $
      withMaxSuccess 1000 $ Ix.prop_observeInsert @Int @Int S.conversion
  ]

tProperties :: TestTree
tProperties = testGroup "New traced index properties."
  [ testProperty "New: Positive or non-positive depth" $
      withMaxSuccess 10000 $ Ix.prop_observeNew @Int @Int TS.conversion
  , testProperty "History length is always smaller than the max depth" $
      withMaxSuccess 1000 $ Ix.prop_sizeLEDepth @Int @Int TS.conversion
  , testProperty "Rewind: Connection with `ixDepth`" $
      withMaxSuccess 1000 $ Ix.prop_rewindDepth @Int @Int TS.conversion
  , testProperty "Relationship between Insert/Rewind" $
      withMaxSuccess 1000 $ Ix.prop_insertRewindInverse @Int @Int TS.conversion
  , testProperty "Insert is folding the structure" $
      withMaxSuccess 1000 $ Ix.prop_observeInsert @Int @Int TS.conversion
  ]

main :: IO ()
main = do
  -- quickSpec ixSignature
  defaultMain tests
