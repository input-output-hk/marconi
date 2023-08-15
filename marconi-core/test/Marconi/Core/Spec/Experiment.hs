{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- | A model base test for indexers.
 -
 - The idea is that we have a naive modelof indexer ('Model') that stores information in a list
 - (from the most recent to the oldest).
 -
 - When we want to test an indexer implementation, we write a 'Model.IndexerTestRunner' for this indexer.
 -
 - It allows us to run the chain of events on both the model and the indexer.
 - We can then query both and compare their results (using 'behaveLikeModel').
 -
 - A set of basic properties for indexers is available and ready to run once you have an `Model.IndexerTestRunner`.
 -
 - A limitation is that our model use 'TestPoint' (a wrapper for 'Int')  as a 'Point' type.
 - If you want to test an indexer with your own events, that have their own 'Point' type,
 - you'll probably need to wrap the event in a @newtype@ to use 'TestPoint' as a 'Point'.
-}
module Marconi.Core.Spec.Experiment (
  -- * Tests

  -- ** Main test suite
  indexingTestGroup,
  storageBasedModelProperty,
  lastSyncBasedModelProperty,

  -- ** Cache test suite
  cacheTestGroup,
  cacheHitProperty,
  cacheMissProperty,

  -- ** Catchup test suite
  catchupTestGroup,

  -- ** Delay test suite
  delayTestGroup,

  -- ** Other tests
  indexingPerformanceTest,
  stopCoordinatorTest,
  resumeSQLiteLastSyncTest,
  resumeMixedLastSyncTest,
  memorySizeUpdateTest,
  withTransformTest,
  withAggregateTest,
  withRollbackFailureTest,

  -- * Mock chain
  DefaultChain,
  defaultChain,
  ForwardChain,
  forwardChain,

  -- ** Events
  Item (..),
  TestPoint (..),
  TestEvent (..),

  -- ** Generators
  GenChainConfig,
  chainSize,
  rollbackFrequency,
  rollbackDepth,
  currentTestPoint,
  genInsert,
  genRollback,
  genItem,

  -- * Model
  IndexerModel (..),
  model,
  runModel,

  -- ** Mapping
  Model.IndexerTestRunner,
  Model.indexerRunner,
  Model.indexerGenerator,

  -- ** Testing
  compareToModelWith,
  Model.compareIndexers,
  behaveLikeModel,

  -- * Instances
  Model.listIndexerRunner,
  mkSqliteIndexerRunner,
  mixedNoMemoryIndexerRunner,
  mixedLowMemoryIndexerRunner,
  mixedHighMemoryIndexerRunner,
  withTracerRunner,
  coordinatorIndexerRunner,

  -- ** Instances internal
  sqliteModelIndexer,
) where

import Control.Concurrent (MVar)
import Control.Concurrent qualified as Con
import Control.Lens (
  Getter,
  Lens',
  filtered,
  folded,
  lens,
  makeLenses,
  to,
  use,
  view,
  views,
  (%~),
  (-~),
  (.=),
  (^.),
  (^..),
 )

import Control.Monad (foldM, replicateM, void)
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, gets)
import Control.Tracer qualified as Tracer

import Data.Foldable (Foldable (foldl'), find)
import Data.Function ((&))

import GHC.Generics (Generic)

import Test.QuickCheck (Arbitrary, Gen, Property, chooseInt, (===), (==>))
import Test.QuickCheck qualified as Test
import Test.QuickCheck.Monadic (PropertyM)
import Test.QuickCheck.Monadic qualified as GenM

import Test.Tasty qualified as Tasty
import Test.Tasty.QuickCheck qualified as Tasty

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT)

import Data.Either (fromRight)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)

import Control.Applicative (Const (Const))
import Data.Monoid (Sum (Sum))
import Database.SQLite.Simple (FromRow)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import GHC.Conc (ThreadStatus (ThreadFinished), threadStatus)

import Data.Word (Word64)
import Marconi.Core.Experiment qualified as Core
import Marconi.Core.Experiment.Coordinator qualified as Core (errorBox, threadIds)
import System.IO.Temp qualified as Tmp
import Test.Marconi.Core.Experiment.ModelBased qualified as Model

newtype TestPoint = TestPoint {unwrapTestPoint :: Int}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Show, FromField, ToField)
  deriving anyclass (FromRow)
  deriving anyclass (SQL.ToRow)

instance Core.HasGenesis TestPoint where
  genesis = 0

-- | We simplify the events to either `Insert` or `Rollback`
data Item event
  = Insert !TestPoint !(Maybe event)
  | Rollback !TestPoint
  deriving stock (Show)

{- | 'GenChainConfig' is used in generators of chain events
 to determine the events that should be build
-}
data GenChainConfig = GenChainConfig
  { _chainSize :: Gen Int
  -- ^ Size of the chain to generate
  , _emptyEventFrequency :: Word
  -- ^ How often an event is @Nothing@
  , _rollbackFrequency :: Word
  -- ^ How often a rollback can happen
  , _rollbackDepth :: TestPoint -> Gen TestPoint
  -- ^ Rollback depth distribution
  , _currentTestPoint :: TestPoint
  -- ^ Track the current point to generate the next one
  }

chainSize :: Getter GenChainConfig (Gen Int)
chainSize = to _chainSize

emptyEventFrequency :: Lens' GenChainConfig Word
emptyEventFrequency =
  lens _emptyEventFrequency (\g f -> g{_emptyEventFrequency = f})

rollbackFrequency :: Lens' GenChainConfig Word
rollbackFrequency =
  lens _rollbackFrequency (\g f -> g{_rollbackFrequency = f})

rollbackDepth :: Lens' GenChainConfig (TestPoint -> Gen TestPoint)
rollbackDepth =
  lens _rollbackDepth (\g d -> g{_rollbackDepth = d})

currentTestPoint :: Lens' GenChainConfig TestPoint
currentTestPoint =
  lens _currentTestPoint (\g p -> g{_currentTestPoint = p})

-- | Generate an insert at the given slot
genInsert :: (Arbitrary event) => Word -> TestPoint -> Gen (Item event)
genInsert f no = do
  xs <-
    Test.frequency
      [ (fromIntegral f, pure Nothing)
      , (100 - fromIntegral f, Just <$> Test.arbitrary)
      ]
  pure $ Insert (no + 1) xs

-- | Generate a rollback, 'GenState' set the maximal depth of the rollback
genRollback :: GenChainConfig -> Gen (Item event)
genRollback = do
  p <- view currentTestPoint
  gen <- view rollbackDepth
  pure $ Rollback <$> gen p

-- | Generate an insert or a rollback, rollback depth is uniform on the chain length
genItem
  :: (Arbitrary event)
  => StateT GenChainConfig Gen (Item event)
genItem = do
  no <- use currentTestPoint
  genRollback' <- gets genRollback
  let setStateSlot = \case
        Insert no' _ -> currentTestPoint .= no'
        Rollback n -> currentTestPoint .= n
  f <- use rollbackFrequency
  let f' = if no > 0 then f else 0 -- no rollback on genesis
  g <- use emptyEventFrequency
  item <-
    lift $
      Test.frequency
        [ (fromIntegral f', genRollback')
        , (100 - fromIntegral f', genInsert g no)
        ]
  setStateSlot item
  pure item

process
  :: (Core.IsIndex m event indexer)
  => (Ord (Core.Point event))
  => (Core.Point event ~ TestPoint)
  => Item event
  -> indexer event
  -> m (indexer event)
process = \case
  Insert ix evt -> Core.index (Core.Timed ix evt)
  Rollback n -> Core.rollback n

genChain
  :: (Arbitrary event)
  => GenChainConfig
  -> Gen [Item event]
genChain cfg = flip evalStateT cfg $ do
  size <- lift $ cfg ^. chainSize
  replicateM size genItem

uniformRollBack :: TestPoint -> Gen TestPoint
uniformRollBack = fmap TestPoint . chooseInt . (,) 0 . unwrapTestPoint

genLargeChain
  :: (Arbitrary event)
  => Word
  -- ^ Rollback percentage
  -> Gen [Item event]
genLargeChain p = do
  let n = Test.chooseInt (1000000, 1200000)
  genChain $ GenChainConfig n 5 p uniformRollBack 0

-- | Chain events with 10% of rollback
newtype DefaultChain event = DefaultChain {_defaultChain :: [Item event]}

makeLenses 'DefaultChain

-- | Chain events with 10% of rollback
instance (Arbitrary event) => Arbitrary (DefaultChain event) where
  arbitrary = Test.sized $ \n ->
    DefaultChain <$> genChain (GenChainConfig (pure n) 5 10 uniformRollBack 0)

-- | Chain events without any rollback
newtype ForwardChain event = ForwardChain {_forwardChain :: [Item event]}

makeLenses 'ForwardChain

instance (Arbitrary event) => Arbitrary (ForwardChain event) where
  arbitrary = Test.sized $ \n ->
    ForwardChain <$> genChain (GenChainConfig (pure n) 5 0 uniformRollBack 0)

-- | Chain events without empty event
newtype FullChain event = FullChain {_fullChain :: [Item event]}

makeLenses 'FullChain

instance (Arbitrary event) => Arbitrary (FullChain event) where
  arbitrary = Test.sized $ \n ->
    FullChain <$> genChain (GenChainConfig (pure n) 0 10 uniformRollBack 0)

-- ** Event instances

newtype TestEvent = TestEvent Int
  deriving stock (Generic)
  deriving newtype (Arbitrary, Eq, Ord, Show, Num, Enum, Real, Integral, FromField, ToField)
  deriving anyclass (FromRow)

type instance Core.Point TestEvent = TestPoint

-- * Model

newtype IndexerModel e = IndexerModel {_model :: [(TestPoint, Maybe e)]}
  deriving stock (Show, Functor, Foldable, Traversable)

makeLenses ''IndexerModel

-- | Build a model from a given chain of events
runModel :: [Item event] -> IndexerModel event
runModel =
  let modelStep :: IndexerModel event -> Item event -> IndexerModel event
      modelStep m (Insert w xs) = m & model %~ ((w, xs) :)
      modelStep m (Rollback n) =
        m & model %~ dropWhile ((> n) . fst)
   in foldl' modelStep (IndexerModel [])

-- | Compare an execution on the base model and one on the indexer
compareToModelWith
  :: (Monad m)
  => (Show event)
  => (Core.Point event ~ TestPoint)
  => (Core.IsIndex m event indexer)
  => Gen [Item event]
  -- ^ the generator used to generate the chain
  -> Model.IndexerTestRunner m event indexer
  -- ^ the runner, applying the chain to the indexer we want to test
  -> (IndexerModel event -> a)
  -- ^ generate the reference value from the base model
  -> (indexer event -> m a)
  -- ^ extract the value we want to test from the indexer
  -> (a -> a -> Property)
  -- ^ the property we want to test
  -> Property
compareToModelWith genChain' runner modelComputation indexerComputation prop =
  let r = runner ^. Model.indexerRunner
      genIndexer = runner ^. Model.indexerGenerator
   in Test.forAll genChain' $ \chain -> r $ do
        initialIndexer <- GenM.run genIndexer
        let model' = runModel chain
            mResult = modelComputation model'
        indexer <- GenM.run $ foldM (flip process) initialIndexer chain
        iResult <- GenM.run $ indexerComputation indexer
        GenM.stop $ mResult `prop` iResult

-- | Compare an execution on the base model and one on the indexer
behaveLikeModel
  :: (Eq a)
  => (Show a)
  => (Show event)
  => (Core.Point event ~ TestPoint)
  => (Core.IsIndex m event indexer)
  => Gen [Item event]
  -- ^ the generator used to generate the chain
  -> Model.IndexerTestRunner m event indexer
  -- ^ the runner, applying the chain to the indexer we want to test
  -> (IndexerModel event -> a)
  -- ^ generate the reference value from the base model
  -> (indexer event -> m a)
  -- ^ extract the value we want to test from the indexer
  -> Property
behaveLikeModel genChain' runner modelComputation indexerComputation =
  compareToModelWith genChain' runner modelComputation indexerComputation (===)

-- | A test tree for the core functionalities of an indexer
indexingTestGroup
  :: (Core.IsIndex m TestEvent indexer)
  => (Core.IsSync m TestEvent indexer)
  => ( Core.Queryable
        (ExceptT (Core.QueryError (Core.EventsMatchingQuery TestEvent)) m)
        TestEvent
        (Core.EventsMatchingQuery TestEvent)
        indexer
     )
  => String
  -> Model.IndexerTestRunner m TestEvent indexer
  -> Tasty.TestTree
indexingTestGroup indexerName runner =
  Tasty.testGroup
    (indexerName <> " core properties")
    [ Tasty.testGroup
        "index"
        [ Tasty.testProperty "indexes events without rollback" $
            Test.withMaxSuccess 5000 $
              storageBasedModelProperty (view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "indexes events with rollbacks" $
            Test.withMaxSuccess 10000 $
              storageBasedModelProperty (view defaultChain <$> Test.arbitrary) runner
        ]
    , Tasty.testGroup
        "lastSync"
        [ Tasty.testProperty "in a chain without rollback" $
            Test.withMaxSuccess 5000 $
              lastSyncBasedModelProperty (view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "in a chain with rollbacks" $
            Test.withMaxSuccess 10000 $
              lastSyncBasedModelProperty (view defaultChain <$> Test.arbitrary) runner
        ]
    ]

indexingPerformanceTest
  :: (Core.IsIndex m TestEvent indexer)
  => (Core.IsSync m TestEvent indexer)
  => ( Core.Queryable
        (ExceptT (Core.QueryError (Core.EventsMatchingQuery TestEvent)) m)
        TestEvent
        (Core.EventsMatchingQuery TestEvent)
        indexer
     )
  => String
  -> Model.IndexerTestRunner m TestEvent indexer
  -> Tasty.TestTree
indexingPerformanceTest indexerName runner =
  Tasty.testProperty (indexerName <> " performance check") $
    Test.withMaxSuccess 5 $
      Test.within 10000000 $
        storageBasedModelProperty (genLargeChain 10) runner

storageBasedModelProperty
  :: (Core.IsIndex m event indexer)
  => (Core.IsSync m event indexer)
  => (Core.Point event ~ TestPoint)
  => (Show event)
  => (Eq event)
  => ( Core.Queryable
        (ExceptT (Core.QueryError (Core.EventsMatchingQuery event)) m)
        event
        (Core.EventsMatchingQuery event)
        indexer
     )
  => Gen [Item event]
  -> Model.IndexerTestRunner m event indexer
  -> Property
storageBasedModelProperty gen runner =
  let indexerEvents indexer =
        fmap (view Core.event)
          . fromRight []
          <$> Core.queryLatestEither Core.allEvents indexer
   in behaveLikeModel
        gen
        runner
        (catMaybes . views model (fmap snd))
        indexerEvents

lastSyncBasedModelProperty
  :: ( Core.IsIndex m event indexer
     , Core.IsSync m event indexer
     , Core.Point event ~ TestPoint
     , Show event
     )
  => Gen [Item event]
  -> Model.IndexerTestRunner m event indexer
  -> Property
lastSyncBasedModelProperty gen runner =
  behaveLikeModel
    gen
    runner
    (views model (maybe Core.genesis fst . listToMaybe))
    Core.lastSyncPoint

sqliteModelIndexerWithFile
  :: FilePath -> ExceptT Core.IndexerError IO (Core.SQLiteIndexer TestEvent)
sqliteModelIndexerWithFile f = do
  let extractor 0 = Nothing
      extractor n = Just n
  Core.mkSingleInsertSqliteIndexer
    f
    (\t -> (t ^. Core.point, t ^. Core.event))
    " CREATE TABLE IF NOT EXISTS index_model \
    \   ( point INT NOT NULL   \
    \   , value INT NOT NULL   \
    \   )"
    "INSERT INTO index_model VALUES (?, ?)"
    (Core.SQLRollbackPlan "index_model" "point" extractor)
    (Core.GetLastSyncQuery "SELECT point FROM index_model ORDER BY point DESC LIMIT 1")

sqliteModelIndexer :: ExceptT Core.IndexerError IO (Core.SQLiteIndexer TestEvent)
sqliteModelIndexer = sqliteModelIndexerWithFile ":memory:"

instance
  ( MonadIO m
  , MonadError (Core.QueryError (Core.EventsMatchingQuery TestEvent)) m
  )
  => Core.Queryable m TestEvent (Core.EventsMatchingQuery TestEvent) Core.SQLiteIndexer
  where
  query =
    let
      rowToResult (Core.EventsMatchingQuery predicate) =
        mapMaybe (traverse predicate . uncurry Core.Timed)
     in
      Core.querySQLiteIndexerWith
        (\p _ -> [":point" SQL.:= p])
        ( const
            " SELECT point, value   \
            \ FROM index_model      \
            \ WHERE point <= :point \
            \ ORDER BY point DESC"
        )
        rowToResult

monadicExceptTIO :: (Tasty.Testable a) => PropertyM (ExceptT err IO) a -> Property
monadicExceptTIO =
  GenM.monadic $ Tasty.ioProperty . fmap (fromRight $ Tasty.property False) . runExceptT

-- | A runner for a 'SQLiteIndexer'
mkSqliteIndexerRunner
  :: Model.IndexerTestRunner (ExceptT Core.IndexerError IO) TestEvent Core.SQLiteIndexer
mkSqliteIndexerRunner = Model.ioIndexerRunner sqliteModelIndexer

mixedModelNoMemoryIndexerWithFile
  :: FilePath
  -> ExceptT
      Core.IndexerError
      IO
      (Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer TestEvent)
mixedModelNoMemoryIndexerWithFile f = do
  dbIndexer <- sqliteModelIndexerWithFile f
  Core.standardMixedIndexer
    0
    1
    dbIndexer

mixedModelNoMemoryIndexer
  :: ExceptT
      Core.IndexerError
      IO
      (Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer TestEvent)
mixedModelNoMemoryIndexer = do
  dbIndexer <- sqliteModelIndexer
  Core.standardMixedIndexer
    0
    1
    dbIndexer

mixedModelLowMemoryIndexer
  :: ExceptT
      Core.IndexerError
      IO
      (Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer TestEvent)
mixedModelLowMemoryIndexer = do
  dbIndexer <- sqliteModelIndexer
  pure $
    Core.mkMixedIndexer
      2
      8
      dbIndexer
      Core.mkListIndexer

mixedModelHighMemoryIndexer
  :: ExceptT
      Core.IndexerError
      IO
      (Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer TestEvent)
mixedModelHighMemoryIndexer = do
  dbIndexer <- sqliteModelIndexer
  pure $
    Core.mkMixedIndexer
      4096
      4096
      dbIndexer
      Core.mkListIndexer

-- | A runner for a 'MixedIndexer' with a small in-memory storage
mixedNoMemoryIndexerRunner
  :: Model.IndexerTestRunner
      (ExceptT Core.IndexerError IO)
      TestEvent
      (Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer)
mixedNoMemoryIndexerRunner = Model.ioIndexerRunner mixedModelNoMemoryIndexer

-- | A runner for a 'MixedIndexer' with a small in-memory storage
mixedLowMemoryIndexerRunner
  :: Model.IndexerTestRunner
      (ExceptT Core.IndexerError IO)
      TestEvent
      (Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer)
mixedLowMemoryIndexerRunner =
  Model.IndexerTestRunner
    monadicExceptTIO
    mixedModelLowMemoryIndexer

-- | A runner for a 'MixedIndexer' with a large in-memory storage
mixedHighMemoryIndexerRunner
  :: Model.IndexerTestRunner
      (ExceptT Core.IndexerError IO)
      TestEvent
      (Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer)
mixedHighMemoryIndexerRunner =
  Model.IndexerTestRunner
    monadicExceptTIO
    mixedModelHighMemoryIndexer

newtype IndexerMVar indexer event = IndexerMVar {getMVar :: MVar (indexer event)}

-- | Provide a coordinator and a way to inspect the coordinated
newtype UnderCoordinator indexer event = UnderCoordinator
  {_underCoordinator :: Core.IndexTransformer (IndexerMVar indexer) Core.Coordinator event}

makeLenses ''UnderCoordinator

instance
  (MonadIO m, MonadError Core.IndexerError m)
  => Core.IsIndex m event (UnderCoordinator indexer)
  where
  index = Core.indexVia underCoordinator
  indexAllDescending = Core.indexAllDescendingVia underCoordinator
  rollback = Core.rollbackVia $ underCoordinator . Core.wrappedIndexer

instance
  (MonadIO m, MonadError Core.IndexerError m, Ord (Core.Point event))
  => Core.IsSync m event (UnderCoordinator indexer)
  where
  lastSyncPoint = Core.lastSyncPointVia underCoordinator

instance
  (Core.Closeable (ExceptT Core.IndexerError IO) Core.Coordinator)
  => Core.Closeable (ExceptT Core.IndexerError IO) (UnderCoordinator indexer)
  where
  close = Core.closeVia $ underCoordinator . Core.wrappedIndexer

instance
  (MonadIO m, Core.Queryable m event (Core.EventsMatchingQuery event) indexer)
  => Core.Queryable m event (Core.EventsMatchingQuery event) (UnderCoordinator indexer)
  where
  query p q ix = do
    let tmvar = ix ^. underCoordinator . Core.wrapperConfig . to getMVar
    indexer <- liftIO $ Con.takeMVar tmvar
    res <- Core.query p q indexer
    liftIO $ Con.putMVar tmvar indexer
    pure res

coordinatorIndexerRunner
  :: ( Core.WorkerIndexer (ExceptT Core.IndexerError IO) event wrapped
     , Core.HasGenesis (Core.Point event)
     , Ord (Core.Point event)
     )
  => Model.IndexerTestRunner (ExceptT Core.IndexerError IO) event wrapped
  -> Model.IndexerTestRunner (ExceptT Core.IndexerError IO) event (UnderCoordinator wrapped)
coordinatorIndexerRunner wRunner =
  Model.IndexerTestRunner
    monadicExceptTIO
    $ do
      wrapped <- wRunner ^. Model.indexerGenerator
      (t, run) <-
        lift $
          Core.createWorker
            "TestWorker"
            (pure . pure)
            wrapped
      UnderCoordinator . Core.IndexTransformer (IndexerMVar t) <$> lift (Core.mkCoordinator [run])

data ParityQuery = OddTestEvent | EvenTestEvent
  deriving (Eq, Ord, Show)

type instance Core.Result ParityQuery = [TestEvent]

instance
  (Applicative m)
  => Core.Queryable m TestEvent ParityQuery Core.ListIndexer
  where
  query p par indexer = do
    let isBefore p' e = p' >= e ^. Core.point
    let f = case par of
          OddTestEvent -> odd
          EvenTestEvent -> even
    pure $
      indexer
        ^.. Core.events
          . folded
          . filtered (isBefore p)
          . Core.event
          . filtered f

instance
  (MonadIO m)
  => Core.Queryable m TestEvent ParityQuery Core.SQLiteIndexer
  where
  query p q indexer =
    let remainder = \case
          OddTestEvent -> odd
          EvenTestEvent -> even
        rowToResult _ = id
     in do
          fmap (either (pure []) (filter (remainder q))) . runExceptT $
            Core.querySQLiteIndexerWith
              (\p' _q' -> [":point" SQL.:= p'])
              ( const $
                  " SELECT value               \
                  \ FROM index_model           \
                  \ WHERE point <= :point      \
                  \ ORDER BY point DESC"
              )
              rowToResult
              p
              q
              indexer

buildCacheFor
  :: (Core.Queryable (ExceptT (Core.QueryError query) (ExceptT Core.IndexerError m)) event query indexer)
  => (Core.IsSync (ExceptT (Core.QueryError query) (ExceptT Core.IndexerError m)) event indexer)
  => (Monad m)
  => (Ord query)
  => (Ord (Core.Point event))
  => query
  -> (Core.Timed (Core.Point event) (Maybe event) -> Core.Result query -> Core.Result query)
  -> indexer event
  -> m (Core.WithCache query indexer event)
buildCacheFor q onForward indexer = do
  let initialCache = Core.withCache onForward indexer
  fromRight initialCache <$> runExceptT (Core.addCacheFor q initialCache)

-- | A runner for a the 'WithCache' tranformer
withCacheRunner
  :: (Core.Queryable (ExceptT (Core.QueryError query) (ExceptT Core.IndexerError m)) event query wrapped)
  => (Core.IsSync (ExceptT (Core.QueryError query) (ExceptT Core.IndexerError m)) event wrapped)
  => (Monad m)
  => (Ord query)
  => (Ord (Core.Point event))
  => query
  -> (Core.Timed (Core.Point event) (Maybe event) -> Core.Result query -> Core.Result query)
  -> Model.IndexerTestRunner m event wrapped
  -> Model.IndexerTestRunner m event (Core.WithCache query wrapped)
withCacheRunner q onForward wRunner =
  Model.IndexerTestRunner
    (wRunner ^. Model.indexerRunner)
    (buildCacheFor q onForward =<< (wRunner ^. Model.indexerGenerator))

oddCacheRunner
  :: Model.IndexerTestRunner
      (ExceptT Core.IndexerError IO)
      TestEvent
      (Core.WithCache ParityQuery Core.ListIndexer)
oddCacheRunner =
  let aggregate timedEvent xs =
        case timedEvent ^. Core.event of
          Just e -> if odd e then e : xs else xs
          Nothing -> xs
   in withCacheRunner OddTestEvent aggregate Model.listIndexerRunner

sqlLiteCacheRunner
  :: Model.IndexerTestRunner
      (ExceptT Core.IndexerError IO)
      TestEvent
      (Core.WithCache ParityQuery Core.SQLiteIndexer)
sqlLiteCacheRunner =
  let aggregate timedEvent xs =
        case timedEvent ^. Core.event of
          Just e -> if odd e then e : xs else xs
          Nothing -> xs
   in withCacheRunner OddTestEvent aggregate mkSqliteIndexerRunner

cacheTestGroup :: Tasty.TestTree
cacheTestGroup =
  Tasty.testGroup
    "Cache"
    [ Tasty.testGroup
        "With ListIndexer"
        [ Tasty.testProperty "Hit cache" $
            Test.withMaxSuccess 10000 $
              cacheHitProperty (view defaultChain <$> Test.arbitrary) oddCacheRunner
        , Tasty.testProperty "Miss cache" $
            Test.withMaxSuccess 10000 $
              cacheMissProperty (view defaultChain <$> Test.arbitrary) oddCacheRunner
        ]
    , Tasty.testGroup
        "With SQLiteIndexer"
        [ Tasty.testProperty "Hit cache" $
            Test.withMaxSuccess 10000 $
              cacheHitProperty (view defaultChain <$> Test.arbitrary) sqlLiteCacheRunner
        , Tasty.testProperty "Miss cache" $
            Test.withMaxSuccess 10000 $
              cacheMissProperty (view defaultChain <$> Test.arbitrary) sqlLiteCacheRunner
        ]
    , cacheUpdateTest
    ]

-- We ask odd elements, which are cached
cacheHitProperty
  :: (Core.IsIndex m TestEvent indexer)
  => (Core.Queryable (ExceptT (Core.QueryError ParityQuery) m) TestEvent ParityQuery indexer)
  => (Core.IsSync m TestEvent indexer)
  => Gen [Item TestEvent]
  -> Model.IndexerTestRunner m TestEvent indexer
  -> Property
cacheHitProperty gen indexer =
  let indexerEvents indexer' =
        fromRight []
          <$> Core.queryLatestEither OddTestEvent indexer'
   in behaveLikeModel
        gen
        indexer
        (views model $ mapMaybe (find odd . snd))
        indexerEvents

-- We ask even elements, which aren't cached
cacheMissProperty
  :: (Core.IsIndex m TestEvent indexer)
  => ( Core.Queryable
        (ExceptT (Core.QueryError ParityQuery) m)
        TestEvent
        ParityQuery
        indexer
     )
  => (Core.IsSync m TestEvent indexer)
  => Gen [Item TestEvent]
  -> Model.IndexerTestRunner m TestEvent indexer
  -> Property
cacheMissProperty gen indexer =
  let indexerEvents indexer' =
        either (error . show) id
          <$> Core.queryLatestEither EvenTestEvent indexer'
      modelEvents = views model $ mapMaybe (find even . snd)
   in behaveLikeModel
        gen
        indexer
        modelEvents
        indexerEvents

-- | A runner for a the 'WithTracer' tranformer
withDelayRunner
  :: (Monad m)
  => Word
  -> Model.IndexerTestRunner m event wrapped
  -> Model.IndexerTestRunner m event (Core.WithDelay wrapped)
withDelayRunner delay wRunner =
  Model.IndexerTestRunner
    (wRunner ^. Model.indexerRunner)
    (Core.withDelay delay <$> wRunner ^. Model.indexerGenerator)

delayProperty
  :: (Core.IsIndex m TestEvent indexer)
  => (Core.IsSync m TestEvent indexer)
  => ( Core.Queryable
        (ExceptT (Core.QueryError (Core.EventsMatchingQuery TestEvent)) m)
        TestEvent
        (Core.EventsMatchingQuery TestEvent)
        indexer
     )
  => (Core.IsSync m TestEvent indexer)
  => Word
  -> Gen [Item TestEvent]
  -> Model.IndexerTestRunner m TestEvent indexer
  -> Property
delayProperty delay gen runner =
  let indexerEvents indexer' =
        fmap (view Core.event)
          . fromRight []
          <$> Core.queryLatestEither Core.allEvents indexer'

      modelEvents lastSync =
        views model (mapMaybe snd . filter ((lastSync >=) . fst))

      dRunner = withDelayRunner delay runner

      r = dRunner ^. Model.indexerRunner
      genIndexer = dRunner ^. Model.indexerGenerator
   in Test.forAll gen $ \chain -> r $ do
        initialIndexer <- GenM.run genIndexer
        indexer <- GenM.run $ foldM (flip process) initialIndexer chain
        iResult <- GenM.run $ indexerEvents indexer
        lastSyncPoint <- GenM.run $ Core.lastSyncPoint indexer
        let model' = runModel chain
            mResult = modelEvents lastSyncPoint model'
        GenM.stop $ mResult === iResult

-- | A test tree for the core functionalities of a delayed indexer
delayTestGroup
  :: (Core.IsIndex m TestEvent indexer)
  => (Core.IsSync m TestEvent indexer)
  => ( Core.Queryable
        (ExceptT (Core.QueryError (Core.EventsMatchingQuery TestEvent)) m)
        TestEvent
        (Core.EventsMatchingQuery TestEvent)
        indexer
     )
  => String
  -> Model.IndexerTestRunner m TestEvent indexer
  -> Tasty.TestTree
delayTestGroup title runner =
  Tasty.testGroup
    (title <> "WithDelay behaviour with WithDelay")
    [ Tasty.testGroup
        "0 delay"
        [ Tasty.testProperty "indexes events without rollback" $
            Test.withMaxSuccess 5000 $
              delayProperty 0 (view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "indexes events with rollbacks" $
            Test.withMaxSuccess 10000 $
              delayProperty 0 (view defaultChain <$> Test.arbitrary) runner
        ]
    , Tasty.testGroup
        "10 delay"
        [ Tasty.testProperty "in a chain without rollback" $
            Test.withMaxSuccess 5000 $
              delayProperty 10 (view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "in a chain with rollbacks" $
            Test.withMaxSuccess 10000 $
              delayProperty 10 (view defaultChain <$> Test.arbitrary) runner
        ]
    ]

-- | A runner for a the 'WithTracer' tranformer
withCatchupRunner
  :: (Monad m)
  => (Core.Point event ~ TestPoint)
  => Word64
  -> Word64
  -> Model.IndexerTestRunner m event wrapped
  -> Model.IndexerTestRunner m event (Core.WithCatchup wrapped)
withCatchupRunner batchSize bypassDistance wRunner =
  let computeDistance (TestPoint d) _ = fromIntegral $ 100 - d
   in Model.IndexerTestRunner
        (wRunner ^. Model.indexerRunner)
        ( Core.withCatchup
            computeDistance
            (Core.CatchupConfig batchSize bypassDistance)
            <$> wRunner ^. Model.indexerGenerator
        )

catchupProperty
  :: (Core.IsIndex m TestEvent indexer)
  => (Core.IsSync m TestEvent indexer)
  => ( Core.Queryable
        (ExceptT (Core.QueryError (Core.EventsMatchingQuery TestEvent)) m)
        TestEvent
        (Core.EventsMatchingQuery TestEvent)
        indexer
     )
  => (Core.IsSync m TestEvent indexer)
  => Word64
  -> Word64
  -> Gen [Item TestEvent]
  -> Model.IndexerTestRunner m TestEvent indexer
  -> Property
catchupProperty batchSize bypassDistance gen runner =
  let indexerEvents indexer' =
        fmap (view Core.event)
          . fromRight []
          <$> Core.queryLatestEither Core.allEvents indexer'

      modelEvents lastSync =
        views model (mapMaybe snd . filter ((lastSync >=) . fst))

      dRunner = withCatchupRunner batchSize bypassDistance runner

      r = dRunner ^. Model.indexerRunner
      genIndexer = dRunner ^. Model.indexerGenerator
   in Test.forAll gen $ \chain -> r $ do
        initialIndexer <- GenM.run genIndexer
        indexer <- GenM.run $ foldM (flip process) initialIndexer chain
        iResult <- GenM.run $ indexerEvents indexer
        lastSyncPoint <- GenM.run $ Core.lastSyncPoint indexer
        let model' = runModel chain
            mResult = modelEvents lastSyncPoint model'
        GenM.stop $ mResult === iResult

-- | A test tree for the core functionalities of a delayed indexer
catchupTestGroup
  :: (Core.IsIndex m TestEvent indexer)
  => (Core.IsSync m TestEvent indexer)
  => ( Core.Queryable
        (ExceptT (Core.QueryError (Core.EventsMatchingQuery TestEvent)) m)
        TestEvent
        (Core.EventsMatchingQuery TestEvent)
        indexer
     )
  => String
  -> Model.IndexerTestRunner m TestEvent indexer
  -> Tasty.TestTree
catchupTestGroup title runner =
  Tasty.testGroup
    (title <> "WithCatchup behaviour with WithCatchup")
    [ Tasty.testGroup
        "0 bufferSize, stop on tip (100 events)"
        [ Tasty.testProperty "indexes events without rollback" $
            Test.withMaxSuccess 5000 $
              catchupProperty 0 0 (view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "indexes events with rollbacks" $
            Test.withMaxSuccess 10000 $
              catchupProperty 0 0 (view defaultChain <$> Test.arbitrary) runner
        ]
    , Tasty.testGroup
        "100 bufferSize, stop on tip (100 events)"
        [ Tasty.testProperty "indexes events without rollback" $
            Test.withMaxSuccess 5000 $
              catchupProperty 100 0 (view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "indexes events with rollbacks" $
            Test.withMaxSuccess 10000 $
              catchupProperty 100 0 (view defaultChain <$> Test.arbitrary) runner
        ]
    , Tasty.testGroup
        "1000 bufferSize, stop on tip (100 events)"
        [ Tasty.testProperty "indexes events without rollback" $
            Test.withMaxSuccess 5000 $
              catchupProperty 1000 0 (view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "indexes events with rollbacks" $
            Test.withMaxSuccess 10000 $
              catchupProperty 1000 0 (view defaultChain <$> Test.arbitrary) runner
        ]
    , Tasty.testGroup
        "0 bufferSize, stop 10 to tip (100 events)"
        [ Tasty.testProperty "indexes events without rollback" $
            Test.withMaxSuccess 5000 $
              catchupProperty 0 10 (view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "indexes events with rollbacks" $
            Test.withMaxSuccess 10000 $
              catchupProperty 0 10 (view defaultChain <$> Test.arbitrary) runner
        ]
    , Tasty.testGroup
        "100 bufferSize, stop 10 to tip (100 events)"
        [ Tasty.testProperty "indexes events without rollback" $
            Test.withMaxSuccess 5000 $
              catchupProperty 100 10 (view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "indexes events with rollbacks" $
            Test.withMaxSuccess 10000 $
              catchupProperty 100 10 (view defaultChain <$> Test.arbitrary) runner
        ]
    , Tasty.testGroup
        "1000 bufferSize, stop 10 to tip (100 events)"
        [ Tasty.testProperty "indexes events without rollback" $
            Test.withMaxSuccess 5000 $
              catchupProperty 1000 10 (view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "indexes events with rollbacks" $
            Test.withMaxSuccess 10000 $
              catchupProperty 1000 10 (view defaultChain <$> Test.arbitrary) runner
        ]
    ]

{- | Check that when when an indexer raises an error,
the coordinator kills the other indexers
-}
stopCoordinatorProperty
  :: (Core.IsIndex (ExceptT Core.IndexerError IO) TestEvent indexer)
  => (Core.IsSync (ExceptT Core.IndexerError IO) TestEvent indexer)
  => (Core.Closeable (ExceptT Core.IndexerError IO) indexer)
  => ( Core.Queryable
        (ExceptT (Core.QueryError (Core.EventsMatchingQuery TestEvent)) IO)
        TestEvent
        (Core.EventsMatchingQuery TestEvent)
        indexer
     )
  => Gen [Item TestEvent]
  -> Model.IndexerTestRunner (ExceptT Core.IndexerError IO) TestEvent indexer
  -> Property
stopCoordinatorProperty gen runner =
  let cRunner = coordinatorIndexerRunner runner
      r = cRunner ^. Model.indexerRunner
      waitForKill = GenM.run $ liftIO $ Con.threadDelay 1000
      forgedError = Core.OtherIndexError "STOP"

      seedError ix =
        GenM.run $
          lift $
            Con.putMVar
              (ix ^. underCoordinator . Core.wrappedIndexer . Core.errorBox)
              forgedError
   in Test.forAll gen $ \chain ->
        length chain > 5 ==> r $ do
          initialIndexer <- GenM.run $ cRunner ^. Model.indexerGenerator
          let (beforeStop, afterStop) = splitAt 5 chain
              stop = head afterStop
          indexer' <- GenM.run $ foldM (flip process) initialIndexer beforeStop
          void $ seedError indexer'
          Left err <- GenM.run $ lift $ runExceptT $ process stop indexer'
          void $ pure $ forgedError === err
          waitForKill
          let threadIds = indexer' ^. underCoordinator . Core.wrappedIndexer . Core.threadIds
          liftIO $ ([ThreadFinished] ===) <$> traverse threadStatus threadIds

stopCoordinatorTest
  :: (Core.IsIndex (ExceptT Core.IndexerError IO) TestEvent indexer)
  => (Core.IsSync (ExceptT Core.IndexerError IO) TestEvent indexer)
  => (Core.Closeable (ExceptT Core.IndexerError IO) indexer)
  => ( Core.Queryable
        (ExceptT (Core.QueryError (Core.EventsMatchingQuery TestEvent)) IO)
        TestEvent
        (Core.EventsMatchingQuery TestEvent)
        indexer
     )
  => Model.IndexerTestRunner (ExceptT Core.IndexerError IO) TestEvent indexer
  -> Tasty.TestTree
stopCoordinatorTest runner =
  Tasty.testProperty "stops coordinator workers" $
    Test.withMaxSuccess 1000 $
      stopCoordinatorProperty (view defaultChain <$> Test.arbitrary) runner

resumeLastSyncProperty
  :: (Core.IsIndex (ExceptT Core.IndexerError IO) TestEvent indexer)
  => (Core.IsSync (ExceptT Core.IndexerError IO) TestEvent indexer)
  => (Core.Closeable (ExceptT Core.IndexerError IO) indexer)
  => (FilePath -> ExceptT Core.IndexerError IO (indexer TestEvent))
  -> Gen [Item TestEvent]
  -> Property
resumeLastSyncProperty rehydrate gen =
  Test.forAll gen $ \chain -> monadicExceptTIO $ do
    file <- lift $ Tmp.withSystemTempDirectory "testResume" pure
    indexer <- lift $ rehydrate file
    indexer' <- GenM.run $ foldM (flip process) indexer chain
    indexer'' <- GenM.run $ rehydrate file
    origSyncPoint <- GenM.run $ Core.lastSyncPoint indexer'
    resumedSyncPoint <- GenM.run $ Core.lastSyncPoint indexer''
    lift $ Core.close indexer'
    lift $ Core.close indexer''
    pure $ origSyncPoint === resumedSyncPoint

resumeSQLiteLastSyncTest :: Tasty.TestTree
resumeSQLiteLastSyncTest =
  Tasty.testProperty "SQLiteIndexer - stop and restart restore lastSyncPoint" $
    Test.withMaxSuccess 500 $
      resumeLastSyncProperty
        sqliteModelIndexerWithFile
        (view fullChain <$> Test.arbitrary)

resumeMixedLastSyncTest :: Tasty.TestTree
resumeMixedLastSyncTest =
  Tasty.testProperty "MixedIndexer - stop and restart restore lastSyncPoint" $
    Test.withMaxSuccess 500 $
      resumeLastSyncProperty
        mixedModelNoMemoryIndexerWithFile
        (view fullChain <$> Test.arbitrary)

memorySizeUpdateProperty
  :: Gen [Item TestEvent]
  -> Property
memorySizeUpdateProperty gen =
  let genSplit = do
        chain <- gen
        changeAt <- Test.choose (0, length chain)
        pure (chain, changeAt)

      indexerEvents indexer =
        fmap (view Core.event)
          . fromRight []
          <$> Core.queryLatestEither Core.allEvents indexer

      refProcess (chain, _) indexer = GenM.run $ foldM (flip process) indexer chain

      testedProcess (chain, changeAt) indexer =
        do
          let (start, end) = splitAt changeAt chain
          flexibleIndexer' <- GenM.run $ foldM (flip process) indexer start
          let flexibleIndexer'' = flexibleIndexer' & Core.keepInMemory -~ 2
          GenM.run $ foldM (flip process) flexibleIndexer'' end

      compareIndexed refIndexer testedIndexer = do
        refEvents :: [TestEvent] <- GenM.run $ indexerEvents refIndexer
        collectedEvents <- GenM.run $ indexerEvents testedIndexer
        GenM.stop $ refEvents === collectedEvents
   in Model.compareIndexers
        refProcess
        testedProcess
        (const compareIndexed)
        genSplit
        mixedLowMemoryIndexerRunner
        mixedLowMemoryIndexerRunner

memorySizeUpdateTest :: Tasty.TestTree
memorySizeUpdateTest =
  Tasty.testProperty "MixedIndexer can change its size while running" $
    Test.withMaxSuccess 10000 $
      memorySizeUpdateProperty (view defaultChain <$> Test.arbitrary)

instance
  (Applicative m)
  => Core.AppendResult m TestEvent ParityQuery Core.ListIndexer
  where
  appendResult p q indexer res = (++) <$> res <*> Core.query p q indexer

cacheUpdateProperty
  :: Gen [Item TestEvent]
  -> Property
cacheUpdateProperty gen =
  let genSplit = do
        chain <- gen
        changeAt <- Test.choose (0, length chain)
        pure (chain, changeAt)

      indexerEvents indexer =
        fromRight []
          <$> Core.queryLatestEither OddTestEvent indexer

      refProcess (chain, _) indexer = GenM.run $ foldM (flip process) indexer chain

      testedProcess (chain, changeAt) indexer =
        do
          let (start, end) = splitAt changeAt (chain :: [Item TestEvent])
          flexibleIndexer' <- GenM.run $ foldM (flip process) indexer start
          flexibleIndexer'' <- GenM.run $ Core.addCacheFor OddTestEvent flexibleIndexer'
          GenM.run $ foldM (flip process) flexibleIndexer'' end

      aggregate timedEvent xs = do
        case timedEvent ^. Core.event of
          Just e -> if odd e then e : xs else xs
          Nothing -> xs

      compareIndexed refIndexer testedIndexer = do
        refEvents <- GenM.run $ indexerEvents refIndexer
        collectedEvents <- GenM.run $ indexerEvents testedIndexer
        GenM.stop $ refEvents === collectedEvents

      runner = withCacheRunner OddTestEvent aggregate mixedLowMemoryIndexerRunner
   in Model.compareIndexers
        refProcess
        testedProcess
        (const compareIndexed)
        genSplit
        runner
        runner

cacheUpdateTest :: Tasty.TestTree
cacheUpdateTest =
  Tasty.testProperty "Adding a cache while indexing dont break anything" $
    Test.withMaxSuccess 10000 $
      cacheUpdateProperty (view defaultChain <$> Test.arbitrary)

-- | A runner for a the 'WithTracer' tranformer
withTracerRunner
  :: (Monad m)
  => Model.IndexerTestRunner m event wrapped
  -> Model.IndexerTestRunner m event (Core.WithTracer m wrapped)
withTracerRunner wRunner =
  Model.IndexerTestRunner
    (wRunner ^. Model.indexerRunner)
    (Core.withTracer Tracer.nullTracer <$> wRunner ^. Model.indexerGenerator)

-- | A runner for a the 'WithTransform' tranformer
withTransformRunner
  :: (Monad m, Core.Point input ~ Core.Point output)
  => (input -> output)
  -> Model.IndexerTestRunner m output wrapped
  -> Model.IndexerTestRunner m input (Core.WithTransform wrapped output)
withTransformRunner f wRunner =
  Model.IndexerTestRunner
    (wRunner ^. Model.indexerRunner)
    (Core.withTransform id (pure . f) <$> wRunner ^. Model.indexerGenerator)

withTransformProperty
  :: Gen [Item TestEvent]
  -> Property
withTransformProperty gen =
  let indexerEvents indexer' =
        fmap (view Core.event)
          . fromRight []
          <$> Core.queryLatestEither Core.allEvents indexer'

      modelEvents lastSync =
        views model (mapMaybe (fmap abs . snd) . filter ((lastSync >=) . fst))

      runner = withTransformRunner abs Model.listIndexerRunner

      r = runner ^. Model.indexerRunner
      genIndexer = runner ^. Model.indexerGenerator
   in Test.forAll gen $ \chain -> r $ do
        initialIndexer <- GenM.run genIndexer
        indexer <- GenM.run $ foldM (flip process) initialIndexer chain
        iResult <- GenM.run $ indexerEvents indexer
        lastSyncPoint <- GenM.run $ Core.lastSyncPoint indexer
        let model' = runModel chain
            mResult = modelEvents lastSyncPoint model'
        GenM.stop $ mResult === iResult

withTransformTest :: Tasty.TestTree
withTransformTest =
  Tasty.testProperty "WithTransform apply transformation before indexing" $
    Test.withMaxSuccess 10000 $
      withTransformProperty (view defaultChain <$> Test.arbitrary)

-- | A runner for a the 'WithAggregate' tranformer
withAggregateRunner
  :: (Monad m)
  => (input -> output)
  -> Model.IndexerTestRunner m output wrapped
  -> Model.IndexerTestRunner m input (Core.WithAggregate wrapped output)
withAggregateRunner f wRunner =
  Model.IndexerTestRunner
    (wRunner ^. Model.indexerRunner)
    (Core.withAggregate f <$> wRunner ^. Model.indexerGenerator)

deriving via (Sum Int) instance Semigroup TestEvent

withAggregateProperty
  :: Gen [Item TestEvent]
  -> Property
withAggregateProperty gen =
  let indexerEvents indexer' =
        fmap (view Core.event)
          . fromRight []
          <$> Core.queryLatestEither Core.allEvents indexer'

      modelAggregate (Just x) (Just y) = Just $ x + y
      modelAggregate Nothing (Just y) = Just y
      modelAggregate (Just x) Nothing = Just x
      modelAggregate Nothing Nothing = Nothing

      modelEvents lastSync =
        catMaybes . init . scanr modelAggregate Nothing
          <$> views model (fmap snd . filter ((lastSync >=) . fst))

      runner = withAggregateRunner id Model.listIndexerRunner

      r = runner ^. Model.indexerRunner
      genIndexer = runner ^. Model.indexerGenerator
   in Test.forAll gen $ \chain -> r $ do
        initialIndexer <- GenM.run genIndexer
        indexer <- GenM.run $ foldM (flip process) initialIndexer chain
        iResult <- GenM.run $ indexerEvents indexer
        lastSyncPoint <- GenM.run $ Core.lastSyncPoint indexer
        let model' = runModel chain
            mResult = modelEvents lastSyncPoint model'
        GenM.stop $ mResult === iResult

withAggregateTest :: Tasty.TestTree
withAggregateTest =
  Tasty.testProperty "WithAggregate apply transformation before indexing" $
    Test.withMaxSuccess 10000 $
      withAggregateProperty (view defaultChain <$> Test.arbitrary)

-- | Provide an indexer that fails on rollback
newtype WithRollbackFailure indexer event = WithRollbackFailure
  {_withRollbackFailure :: Core.IndexTransformer (Const ()) indexer event}

makeLenses ''WithRollbackFailure

-- | A runner for a the 'WithAggregate' tranformer
withRollbackFailureRunner
  :: (Monad m)
  => Model.IndexerTestRunner m event wrapped
  -> Model.IndexerTestRunner m event (WithRollbackFailure wrapped)
withRollbackFailureRunner wRunner =
  Model.IndexerTestRunner
    (wRunner ^. Model.indexerRunner)
    (WithRollbackFailure . Core.IndexTransformer (Const ()) <$> (wRunner ^. Model.indexerGenerator))

instance Core.IndexerTrans WithRollbackFailure where
  type Config WithRollbackFailure = Const ()
  wrap cfg = WithRollbackFailure . Core.IndexTransformer cfg
  unwrap = withRollbackFailure . Core.wrappedIndexer

deriving via
  (Core.IndexTransformer (Const ()) indexer)
  instance
    (Core.IsSync m event indexer) => Core.IsSync m event (WithRollbackFailure indexer)

instance (Core.IsIndex m event indexer) => Core.IsIndex m event (WithRollbackFailure indexer) where
  index = Core.indexVia Core.unwrap
  rollback _ _ = error "STOP"

deriving via
  (Core.IndexTransformer (Const ()) indexer)
  instance
    (Core.Closeable m indexer) => Core.Closeable m (WithRollbackFailure indexer)

checkOutOfRollback
  :: Gen [Item TestEvent]
  -> Property
checkOutOfRollback gen =
  let runner = coordinatorIndexerRunner $ withRollbackFailureRunner Model.listIndexerRunner

      r = runner ^. Model.indexerRunner
      genIndexer = runner ^. Model.indexerGenerator
   in Test.within 10000000 $ Test.expectFailure $ Test.forAll gen $ \chain -> r $ do
        initialIndexer <- GenM.run genIndexer
        void $ GenM.run $ foldM (flip process) initialIndexer (chain ++ [Rollback 0])
        GenM.stop True

withRollbackFailureTest :: Tasty.TestTree
withRollbackFailureTest =
  Tasty.testProperty "Rollback failure in a worker exit nicely" $
    Test.withMaxSuccess 10000 $
      checkOutOfRollback (view defaultChain <$> Test.arbitrary)
