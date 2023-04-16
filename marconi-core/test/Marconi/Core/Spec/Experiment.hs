{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE UndecidableInstances #-}
{- | A model base test for indexers.
 -
 - The idea is that we have a naive modelof indexer ('Model') that stores information in a list
 - (from the most recent to the oldest).
 -
 - When we want to test an indexer implementation, we write a 'IndexerTestRunner' for this indexer.
 -
 - It allows us to run the chain of events on both the model and the indexer.
 - We can then query both and compare their results (using 'behaveLikeModel').
 -
 - A set of basic properties for indexers is available and ready to run once you have an `IndexerTestRunner`.
 -
 - A limitation is that our model use 'TestPoint' (a wrapper for 'Int')  as a 'Point' type.
 - If you want to test an indexer with your own events, that have their own 'Point' type,
 - you'll probably need to wrap the event in a @newtype@ to use 'TestPoint' as a 'Point'.
-}
module Marconi.Core.Spec.Experiment
    (
    -- * Tests
    -- ** Main test suite
      indexingTestGroup
    , storageBasedModelProperty
    , lastSyncBasedModelProperty
    -- ** Cache test suite
    , cacheTestGroup
    , cacheHitProperty
    , cacheMissProperty
    -- ** Delay test suite
    , delayTestGroup
    -- ** Other tests
    , indexingPerformanceTest
    -- * Mock chain
    , DefaultChain
        , defaultChain
    , ForwardChain
        , forwardChain
    -- ** Events
    , Item (..)
    , TestPoint (..)
    , TestEvent (..)
    -- ** Generators
    , GenChainConfig
        , chainSize
        , rollbackFrequency
        , rollbackDepth
        , currentTestPoint
    , genInsert
    , genRollback
    , genItem
    -- * Model
    , IndexerModel (..)
        , model
    , runModel
    -- ** Mapping
    , IndexerTestRunner
        , indexerRunner
        , indexerGenerator
    -- ** Testing
    , compareToModelWith
    , behaveLikeModel
    -- * Instances
    , listIndexerRunner
    , sqliteIndexerRunner
    , mixedLowMemoryIndexerRunner
    , mixedHighMemoryIndexerRunner
    , withTracerRunner
    , coordinatorIndexerRunner
    -- ** Instances internal
    , initSQLite
    , sqliteModelIndexer
    ) where

import Control.Concurrent (MVar)
import Control.Concurrent qualified as Con
import Control.Lens (Getter, Lens', filtered, folded, lens, makeLenses, to, use, view, views, (%~), (.=), (^.), (^..))

import Control.Monad (foldM, replicateM)
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, gets)
import Control.Tracer qualified as Tracer

import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))

import GHC.Generics (Generic)

import Test.QuickCheck (Arbitrary, Gen, Property, choose, (===))
import Test.QuickCheck qualified as Test
import Test.QuickCheck.Monadic (PropertyM)
import Test.QuickCheck.Monadic qualified as GenM

import Test.Tasty qualified as Tasty
import Test.Tasty.QuickCheck qualified as Tasty

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT)

import Data.Either (fromRight)
import Data.Maybe (listToMaybe)

import Database.SQLite.Simple (FromRow)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import Marconi.Core.Experiment (wrappedIndexer)
import Marconi.Core.Experiment qualified as Core

newtype TestPoint = TestPoint { unwrapTestPoint :: Int }
    deriving stock (Generic)
    deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Show, FromField, ToField)
    deriving anyclass (FromRow)
    deriving anyclass (SQL.ToRow)

instance Core.HasGenesis TestPoint where
    genesis = 0

-- | We simplify the events to either `Insert` or `Rollback`
data Item event
    = Insert !TestPoint !event
    | Rollback !TestPoint
    deriving stock Show

-- | 'GenChainConfig' is used in generators of chain events
-- to determine the events that should be build
data GenChainConfig
    = GenChainConfig
        { _chainSize         :: Gen Int
          -- ^ Size of the chain to generate
        , _rollbackFrequency :: Word
          -- ^ How often a rollback can happen
        , _rollbackDepth     :: TestPoint -> Gen TestPoint
          -- ^ Rollback depth distribution
        , _currentTestPoint  :: TestPoint
          -- ^ Track the current point to generate the next one
        }

chainSize :: Getter GenChainConfig (Gen Int)
chainSize = to _chainSize

rollbackFrequency :: Lens' GenChainConfig Word
rollbackFrequency
    = lens _rollbackFrequency (\g f-> g{_rollbackFrequency = f})

rollbackDepth :: Lens' GenChainConfig (TestPoint -> Gen TestPoint)
rollbackDepth
    = lens _rollbackDepth (\g d-> g{_rollbackDepth = d})

currentTestPoint :: Lens' GenChainConfig TestPoint
currentTestPoint
    = lens _currentTestPoint (\g p-> g{_currentTestPoint = p})

-- | Generate an insert at the given slot
genInsert :: Arbitrary event => TestPoint -> Gen (Item event)
genInsert no = do
    xs <- Test.arbitrary
    pure $ Insert (no + 1) xs

-- | Generate a rollback, 'GenState' set the maximal depth of the rollback
genRollback :: GenChainConfig -> Gen (Item event)
genRollback = do
    p <- view currentTestPoint
    gen <- view rollbackDepth
    pure $ Rollback <$> gen p

-- | Generate an insert or a rollback, rollback depth is uniform on the chain length
genItem
    :: Arbitrary event
    => StateT GenChainConfig Gen (Item event)
genItem = do
    no <- use currentTestPoint
    f <- use rollbackFrequency
    genRollback' <- gets genRollback
    let setStateSlot = \case
            Insert no' _ -> currentTestPoint .= no'
            Rollback n   -> currentTestPoint .= n
    let f' = if no > 0 then f else 0 -- no rollback on genesis
    item <- lift $ Test.frequency
        [ (fromIntegral f',  genRollback')
        , (100 - fromIntegral f', genInsert no)
        ]
    setStateSlot item
    pure item


genChain
    :: Arbitrary event
    => GenChainConfig -> Gen [Item event]
genChain cfg = flip evalStateT cfg $ do
    size <- lift $ cfg ^. chainSize
    replicateM size genItem

uniformRollBack :: TestPoint -> Gen TestPoint
uniformRollBack = fmap TestPoint . choose . (,) 0 . unwrapTestPoint

genLargeChain
    :: Arbitrary event
    => Word -- ^ Rollback percentage
    -> Gen [Item event]
genLargeChain p = do
    let n = Test.choose (1000000,1200000)
    genChain $ GenChainConfig n p uniformRollBack 0

-- | Chain events with 10% of rollback
newtype DefaultChain event = DefaultChain {_defaultChain :: [Item event]}

makeLenses 'DefaultChain

-- | Chain events with 10% of rollback (rollbackDepth is)
instance Arbitrary event => Arbitrary (DefaultChain event) where

    arbitrary = Test.sized $ \n ->
        DefaultChain <$> genChain (GenChainConfig (pure n) 10 uniformRollBack 0)

-- | Chain events without any rollback
newtype ForwardChain event = ForwardChain {_forwardChain :: [Item event]}

makeLenses 'ForwardChain

instance Arbitrary event => Arbitrary (ForwardChain event) where

    arbitrary = Test.sized $ \n ->
        ForwardChain <$> genChain (GenChainConfig (pure n) 0 uniformRollBack 0)

-- ** Event instances

newtype TestEvent = TestEvent Int
    deriving stock Generic
    deriving newtype (Arbitrary, Eq, Ord, Show, Num, Enum, Real, Integral, FromField, ToField)
    deriving anyclass FromRow

type instance Core.Point TestEvent = TestPoint

-- * Model

newtype IndexerModel e = IndexerModel {_model :: [(TestPoint, e)]}
    deriving stock Show

makeLenses ''IndexerModel

-- | Build a model from a given chain of events
runModel :: [Item event] -> IndexerModel event
runModel = let

    modelStep :: IndexerModel event -> Item event -> IndexerModel event
    modelStep m (Insert w xs) = m & model %~ ((w,xs):)
    modelStep m (Rollback n) =
        m & model %~ dropWhile ((> n) . fst)

    in foldl' modelStep (IndexerModel [])

-- | Used to map an indexer to a model
data IndexerTestRunner m event indexer
    = IndexerTestRunner
        { _indexerRunner    :: !(PropertyM m Property -> Property)
        , _indexerGenerator :: !(m (indexer event))
        }

makeLenses ''IndexerTestRunner

-- | Compare an execution on the base model and one on the indexer
compareToModelWith
    :: Monad m
    => Show event
    => Core.Point event ~ TestPoint
    => Core.IsIndex m event indexer
    => Core.Rewindable m event indexer
    => Gen [Item event]
    -- ^ the generator used to generate the chain
    -> IndexerTestRunner m event indexer
    -- ^ the runner, applying the chain to the indexer we want to test
    -> (IndexerModel event -> a)
    -- ^ generate the reference value from the base model
    -> (indexer event -> m a)
    -- ^ extract the value we want to test from the indexer
    -> (a -> a -> Property)
    -- ^ the property we want to test
    -> Property
compareToModelWith genChain' runner modelComputation indexerComputation prop
    = let
        process = \case
            Insert ix evt -> Core.index (Core.TimedEvent ix evt)
            Rollback n    -> Core.rewind n
        r = runner ^. indexerRunner
        genIndexer = runner ^. indexerGenerator
    in Test.forAll genChain' $ \chain -> r $ do
        initialIndexer <- GenM.run genIndexer
        let model' = runModel chain
            mResult = modelComputation model'
        indexer <- GenM.run $ foldM (flip process) initialIndexer chain
        iResult <- GenM.run $ indexerComputation indexer
        GenM.stop $  mResult `prop` iResult

-- | Compare an execution on the base model and one on the indexer
behaveLikeModel
    :: Eq a
    => Show a
    => Show event
    => Core.Point event ~ TestPoint
    => Core.IsIndex m event indexer
    => Core.Rewindable m event indexer
    => Gen [Item event]
    -- ^ the generator used to generate the chain
    -> IndexerTestRunner m event indexer
    -- ^ the runner, applying the chain to the indexer we want to test
    -> (IndexerModel event -> a)
    -- ^ generate the reference value from the base model
    -> (indexer event -> m a)
    -- ^ extract the value we want to test from the indexer
    -> Property
behaveLikeModel genChain' runner modelComputation indexerComputation
    = compareToModelWith genChain' runner modelComputation indexerComputation (===)

-- | A test tree for the core functionalities of an indexer
indexingTestGroup
    :: Core.Rewindable m TestEvent indexer
    => Core.IsIndex m TestEvent indexer
    => Core.IsSync m TestEvent indexer
    => Core.Queryable
        (ExceptT (Core.QueryError (Core.EventsMatchingQuery TestEvent)) m)
        TestEvent
        (Core.EventsMatchingQuery TestEvent) indexer
    => String -> IndexerTestRunner m TestEvent indexer -> Tasty.TestTree
indexingTestGroup indexerName runner
    = Tasty.testGroup (indexerName <> " core properties")
        [ Tasty.testGroup "index"
            [ Tasty.testProperty "indexes events without rollback"
                $ Test.withMaxSuccess 5000
                $ storageBasedModelProperty (view forwardChain <$> Test.arbitrary) runner
            , Tasty.testProperty "indexes events with rollbacks"
                $ Test.withMaxSuccess 10000
                $ storageBasedModelProperty (view defaultChain <$> Test.arbitrary) runner
            ]
        , Tasty.testGroup "lastSync"
            [ Tasty.testProperty "in a chain without rollback"
                $ Test.withMaxSuccess 5000
                $ lastSyncBasedModelProperty (view forwardChain <$> Test.arbitrary) runner
            , Tasty.testProperty "in a chain with rollbacks"
                $ Test.withMaxSuccess 10000
                $ lastSyncBasedModelProperty (view defaultChain <$> Test.arbitrary) runner
            ]
        ]

indexingPerformanceTest
    :: Core.Rewindable m TestEvent indexer
    => Core.IsIndex m TestEvent indexer
    => Core.IsSync m TestEvent indexer
    => Core.Queryable
        (ExceptT (Core.QueryError (Core.EventsMatchingQuery TestEvent)) m)
        TestEvent
        (Core.EventsMatchingQuery TestEvent) indexer
    => String -> IndexerTestRunner m TestEvent indexer -> Tasty.TestTree
indexingPerformanceTest indexerName runner
    = Tasty.testProperty (indexerName <> " performance check")
        $ Test.withMaxSuccess 5
        $ Test.within 10000000 $ storageBasedModelProperty (genLargeChain 10) runner

storageBasedModelProperty
    :: Core.Rewindable m event indexer
    => Core.IsIndex m event indexer
    => Core.IsSync m event indexer
    => Core.Point event ~ TestPoint
    => Show event
    => Eq event
    => Core.Queryable
        (ExceptT (Core.QueryError (Core.EventsMatchingQuery event)) m)
        event
        (Core.EventsMatchingQuery event)
        indexer
    => Gen [Item event]
    -> IndexerTestRunner m event indexer
    -> Property
storageBasedModelProperty gen runner
    = let

        indexerEvents indexer
            = fmap (view Core.event)
            . fromRight []
            <$> Core.queryLatest' Core.allEvents indexer

    in behaveLikeModel
        gen
        runner
        (views model (fmap snd))
        indexerEvents

lastSyncBasedModelProperty
    ::
    ( Core.Rewindable m event indexer
    , Core.IsIndex m event indexer
    , Core.IsSync m event indexer
    , Core.Point event ~ TestPoint
    , Show event
    )
    => Gen [Item event]
    -> IndexerTestRunner m event indexer
    -> Property
lastSyncBasedModelProperty gen runner
    = behaveLikeModel
        gen
        runner
        (views model (maybe Core.genesis fst . listToMaybe))
        Core.lastSyncPoint

-- | A runner for a 'ListIndexer'
listIndexerRunner
    :: Core.HasGenesis (Core.Point e)
    => IndexerTestRunner (ExceptT Core.IndexerError IO) e Core.ListIndexer
listIndexerRunner
    = IndexerTestRunner
        monadicExceptTIO
        (pure Core.listIndexer)

initSQLite :: IO SQL.Connection
initSQLite = do
    con <- SQL.open ":memory:"

    SQL.execute_ con
        " CREATE TABLE index_model \
        \   ( point INT NOT NULL   \
        \   , value INT NOT NULL   \
        \   )                      "

    pure con

type instance Core.InsertRecord TestEvent = [(TestPoint, TestEvent)]

sqliteModelIndexer :: SQL.Connection -> Core.SQLiteIndexer TestEvent
sqliteModelIndexer con
    = Core.singleInsertSQLiteIndexer con
        (\t -> (t ^. Core.point, t ^. Core.event))
        "INSERT INTO index_model VALUES (?, ?)"

instance MonadIO m => Core.Rewindable m TestEvent Core.SQLiteIndexer where

    rewind = Core.rewindSQLiteIndexerWith "DELETE FROM index_model WHERE point > ?"

instance
    ( MonadIO m
    , MonadError (Core.QueryError (Core.EventsMatchingQuery TestEvent)) m
    ) =>
    Core.Queryable m TestEvent (Core.EventsMatchingQuery TestEvent) Core.SQLiteIndexer where

    query = let

        rowToResult (Core.EventsMatchingQuery predicate)
            = fmap (uncurry Core.TimedEvent)
            . filter (predicate . snd)

        in Core.querySQLiteIndexerWith
            (\p _ -> [":point" SQL.:= p])
            " SELECT point, value   \
            \ FROM index_model      \
            \ WHERE point <= :point \
            \ ORDER BY point DESC"
            rowToResult

monadicExceptTIO :: Tasty.Testable a => PropertyM (ExceptT err IO) a -> Property
monadicExceptTIO
    = GenM.monadic $ Tasty.ioProperty . fmap (fromRight $ Tasty.property False) . runExceptT

-- | A runner for a 'SQLiteIndexer'
sqliteIndexerRunner
    :: IndexerTestRunner (ExceptT Core.IndexerError IO) TestEvent Core.SQLiteIndexer
sqliteIndexerRunner
    = IndexerTestRunner
        monadicExceptTIO
        (sqliteModelIndexer <$> lift initSQLite)

mixedModelLowMemoryIndexer
    :: SQL.Connection
    -> Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer TestEvent
mixedModelLowMemoryIndexer con
    = Core.mixedIndexer
        2
        8
        (sqliteModelIndexer con)
        Core.listIndexer

mixedModelHighMemoryIndexer
    :: SQL.Connection
    -> Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer TestEvent
mixedModelHighMemoryIndexer con
    = Core.mixedIndexer
        4096
        4096
        (sqliteModelIndexer con)
        Core.listIndexer

-- | A runner for a 'MixedIndexer' with a small in-memory storage
mixedLowMemoryIndexerRunner
    :: IndexerTestRunner
        (ExceptT Core.IndexerError IO)
        TestEvent
        (Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer)
mixedLowMemoryIndexerRunner
    = IndexerTestRunner
        monadicExceptTIO
        (mixedModelLowMemoryIndexer <$> lift initSQLite)

-- | A runner for a 'MixedIndexer' with a large in-memory storage
mixedHighMemoryIndexerRunner
    :: IndexerTestRunner
        (ExceptT Core.IndexerError IO)
        TestEvent
        (Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer)
mixedHighMemoryIndexerRunner
    = IndexerTestRunner
        monadicExceptTIO
        (mixedModelHighMemoryIndexer <$> lift initSQLite)

-- | A runner for a the 'WithTracer' tranformer
withTracerRunner
    :: Monad m
    => IndexerTestRunner m event wrapped
    -> IndexerTestRunner m event (Core.WithTracer m wrapped)
withTracerRunner wRunner
    = IndexerTestRunner
        (wRunner ^. indexerRunner)
        (Core.withTracer Tracer.nullTracer <$> wRunner ^. indexerGenerator)

newtype IndexerMVar indexer event = IndexerMVar {getMVar :: MVar (indexer event)}

-- | Provide a coordinator and a way to inspect the coordinated
newtype UnderCoordinator indexer event
    = UnderCoordinator
        { _underCoordinator :: Core.IndexWrapper (IndexerMVar indexer) Core.Coordinator event }

makeLenses ''UnderCoordinator

instance (MonadIO m, MonadError Core.IndexerError m)
    => Core.IsIndex m event (UnderCoordinator indexer) where

    index = Core.indexVia underCoordinator
    indexAll = Core.indexAllVia underCoordinator

instance MonadIO m => Core.IsSync m event (UnderCoordinator indexer) where

    lastSyncPoint = Core.lastSyncPointVia underCoordinator

instance Core.HasGenesis (Core.Point event)
    => Core.Rewindable (ExceptT Core.IndexerError IO) event (UnderCoordinator indexer) where
    rewind = Core.rewindVia $ underCoordinator . wrappedIndexer

instance (MonadIO m, Core.Queryable m event (Core.EventsMatchingQuery event) indexer) =>
    Core.Queryable m event (Core.EventsMatchingQuery event) (UnderCoordinator indexer) where

    query p q ix = do
        let tmvar = ix ^. underCoordinator . Core.wrapperConfig . to getMVar
        indexer <- liftIO $ Con.takeMVar tmvar
        res <- Core.query p q indexer
        liftIO $ Con.putMVar tmvar indexer
        pure res

coordinatorIndexerRunner
    ::
    ( Core.WorkerIndexer (ExceptT Core.IndexerError IO) event wrapped
    , Core.HasGenesis (Core.Point event)
    , Ord (Core.Point event)
    ) => IndexerTestRunner (ExceptT Core.IndexerError IO) event wrapped
    -> IndexerTestRunner (ExceptT Core.IndexerError IO) event (UnderCoordinator wrapped)
coordinatorIndexerRunner wRunner
    = IndexerTestRunner
        monadicExceptTIO $ do
            wrapped <- wRunner ^. indexerGenerator
            (t, run) <- lift $ Core.createWorker
                pure
                wrapped
            UnderCoordinator . Core.IndexWrapper (IndexerMVar t) <$> lift (Core.start [run])

data ParityQuery = OddTestEvent | EvenTestEvent
    deriving (Eq, Ord, Show)

type instance Core.Result ParityQuery = [TestEvent]

instance Applicative m
    => Core.Queryable m TestEvent ParityQuery Core.ListIndexer where

    query p par indexer = do
        let isBefore p' e = p' >= e ^. Core.point
        let f = case par of
                OddTestEvent  -> odd
                EvenTestEvent -> even
        pure
            $ indexer ^.. Core.events . folded
            . filtered (isBefore p) . Core.event . filtered f

instance MonadIO m
    => Core.Queryable m TestEvent ParityQuery Core.SQLiteIndexer where

    query p q indexer = let

        remainder = \case
            OddTestEvent  -> odd
            EvenTestEvent -> even

        rowToResult _
            = id

        in do
           fmap (either (pure []) (filter (remainder q))) . runExceptT
               $ Core.querySQLiteIndexerWith
               (\p' _q' -> [":point" SQL.:= p'])
               " SELECT value               \
               \ FROM index_model           \
               \ WHERE point <= :point      \
               \ ORDER BY point DESC"
               rowToResult p q
               indexer

buildCacheFor
    :: Core.Queryable (ExceptT (Core.QueryError query) (ExceptT Core.IndexerError m)) event query indexer
    => Core.IsSync (ExceptT (Core.QueryError query) (ExceptT Core.IndexerError m)) event indexer
    => Monad m
    => Ord query
    => Ord (Core.Point event)
    => query
    -> (Core.TimedEvent event -> Core.Result query -> Core.Result query)
    -> indexer event
    -> m (Core.WithCache query indexer event)
buildCacheFor q onForward indexer = do
    let initialCache = Core.withCache onForward indexer
    fromRight initialCache <$> runExceptT (Core.addCacheFor q initialCache)

-- | A runner for a the 'WithTracer' tranformer
withCacheRunner
    :: Core.Queryable (ExceptT (Core.QueryError query) (ExceptT Core.IndexerError m)) event query wrapped
    => Core.IsSync (ExceptT (Core.QueryError query) (ExceptT Core.IndexerError m)) event wrapped
    => Monad m
    => Ord query
    => Ord (Core.Point event)
    => query
    -> (Core.TimedEvent event -> Core.Result query -> Core.Result query)
    -> IndexerTestRunner m event wrapped
    -> IndexerTestRunner m event (Core.WithCache query wrapped)
withCacheRunner q onForward wRunner
    = IndexerTestRunner
        (wRunner ^. indexerRunner)
        (buildCacheFor q onForward =<< (wRunner ^. indexerGenerator))

pairCacheRunner
    :: IndexerTestRunner
        (ExceptT Core.IndexerError IO)
        TestEvent
        (Core.WithCache ParityQuery Core.ListIndexer)
pairCacheRunner = let
    aggregate timedEvent xs
         = let e = timedEvent ^. Core.event
         in if odd e then e:xs else xs
    in withCacheRunner OddTestEvent aggregate listIndexerRunner

sqlLiteCacheRunner
    :: IndexerTestRunner
        (ExceptT Core.IndexerError IO)
        TestEvent
        (Core.WithCache ParityQuery Core.SQLiteIndexer)
sqlLiteCacheRunner = let
    aggregate timedEvent xs
         = let e = timedEvent ^. Core.event
         in if odd e then e:xs else xs
    in withCacheRunner OddTestEvent aggregate sqliteIndexerRunner

cacheTestGroup :: Tasty.TestTree
cacheTestGroup = Tasty.testGroup "Cache"
    [ Tasty.testGroup "With ListIndexer"
        [ Tasty.testProperty "Hit cache"
            $ Test.withMaxSuccess 10000
            $ cacheHitProperty (view defaultChain <$> Test.arbitrary) pairCacheRunner
        , Tasty.testProperty "Miss cache"
            $ Test.withMaxSuccess 10000
            $ cacheMissProperty (view defaultChain <$> Test.arbitrary) pairCacheRunner
        ]
    , Tasty.testGroup "With SQLiteIndexer"
        [ Tasty.testProperty "Hit cache"
            $ Test.withMaxSuccess 10000
            $ cacheHitProperty (view defaultChain <$> Test.arbitrary) sqlLiteCacheRunner
        , Tasty.testProperty "Miss cache"
            $ Test.withMaxSuccess 10000
            $ cacheMissProperty (view defaultChain <$> Test.arbitrary) sqlLiteCacheRunner
        ]
    ]

-- We ask odd elements, which are cached
cacheHitProperty
    :: Core.IsIndex m TestEvent indexer
    => Core.Rewindable m TestEvent indexer
    => Core.Queryable (ExceptT (Core.QueryError ParityQuery) m) TestEvent ParityQuery indexer
    => Core.IsSync m TestEvent indexer
    => Gen [Item TestEvent]
    -> IndexerTestRunner m TestEvent indexer -> Property
cacheHitProperty gen indexer
    = let

        indexerEvents indexer'
            = fromRight []
            <$> Core.queryLatest' OddTestEvent indexer'

    in behaveLikeModel
        gen
        indexer
        (views model (filter odd . fmap snd))
        indexerEvents

-- We ask even elements, which aren't cached
cacheMissProperty
    :: Core.IsIndex m TestEvent indexer
    => Core.Rewindable m TestEvent indexer
    => Core.Queryable
        (ExceptT (Core.QueryError ParityQuery) m)
        TestEvent
        ParityQuery
        indexer
    => Core.IsSync m TestEvent indexer
    => Gen [Item TestEvent]
    -> IndexerTestRunner m TestEvent indexer -> Property
cacheMissProperty gen indexer
    = let

        indexerEvents indexer'
            = fromRight []
            <$> Core.queryLatest' EvenTestEvent indexer'
        modelEvents = views model (filter even . fmap snd)

    in behaveLikeModel
        gen
        indexer
        modelEvents
        indexerEvents

-- | A runner for a the 'WithTracer' tranformer
withDelayRunner
    :: Monad m
    => Word
    -> IndexerTestRunner m event wrapped
    -> IndexerTestRunner m event (Core.WithDelay wrapped)
withDelayRunner delay wRunner
    = IndexerTestRunner
        (wRunner ^. indexerRunner)
        (Core.withDelay delay <$> wRunner ^. indexerGenerator)

delayProperty
    :: Core.IsIndex m TestEvent indexer
    => Core.IsSync m TestEvent indexer
    => Core.Rewindable m TestEvent indexer
    => Core.Queryable
        (ExceptT (Core.QueryError (Core.EventsMatchingQuery TestEvent)) m)
        TestEvent
        (Core.EventsMatchingQuery TestEvent) indexer
    => Core.IsSync m TestEvent indexer
    => Word
    -> Gen [Item TestEvent]
    -> IndexerTestRunner m TestEvent indexer -> Property
delayProperty delay gen runner
    = let

        indexerEvents indexer'
            = fmap (view Core.event)
            . fromRight []
            <$> Core.queryLatest' Core.allEvents indexer'

        modelEvents lastSync
            = views model (fmap snd . filter ((lastSync >=) . fst))

        process = \case
            Insert ix evt -> Core.index (Core.TimedEvent ix evt)
            Rollback n    -> Core.rewind n

        dRunner = withDelayRunner delay runner

        r = dRunner ^. indexerRunner
        genIndexer = dRunner ^. indexerGenerator

    in Test.forAll gen $ \chain -> r $ do
        initialIndexer <- GenM.run genIndexer
        indexer <- GenM.run $ foldM (flip process) initialIndexer chain
        iResult <- GenM.run $ indexerEvents indexer
        lastSyncPoint <- GenM.run $ Core.lastSyncPoint indexer
        let model' = runModel chain
            mResult = modelEvents lastSyncPoint model'
        GenM.stop $  mResult === iResult

-- | A test tree for the core functionalities of a delayed indexer
delayTestGroup
    :: Core.Rewindable m TestEvent indexer
    => Core.IsIndex m TestEvent indexer
    => Core.IsSync m TestEvent indexer
    => Core.Queryable
        (ExceptT (Core.QueryError (Core.EventsMatchingQuery TestEvent)) m)
        TestEvent
        (Core.EventsMatchingQuery TestEvent) indexer
    => IndexerTestRunner m TestEvent indexer -> Tasty.TestTree
delayTestGroup runner
    = Tasty.testGroup "WithDelay core properties"
        [ Tasty.testGroup "0 delay"
            [ Tasty.testProperty "indexes events without rollback"
                $ Test.withMaxSuccess 5000
                $ delayProperty 0 (view forwardChain <$> Test.arbitrary) runner
            , Tasty.testProperty "indexes events with rollbacks"
                $ Test.withMaxSuccess 10000
                $ delayProperty 0 (view defaultChain <$> Test.arbitrary) runner
            ]
        , Tasty.testGroup "10 delay"
            [ Tasty.testProperty "in a chain without rollback"
                $ Test.withMaxSuccess 5000
                $ delayProperty 10 (view forwardChain <$> Test.arbitrary) runner
            , Tasty.testProperty "in a chain with rollbacks"
                $ Test.withMaxSuccess 10000
                $ delayProperty 10 (view defaultChain <$> Test.arbitrary) runner
            ]
        ]