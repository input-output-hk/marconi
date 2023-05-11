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
    -- * The test suite
      indexingTestGroup
    , indexingPerformanceTest
    -- ** individual tests
    , storageBasedModelProperty
    , lastSyncBasedModelProperty
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
import Control.Lens (makeLenses, to, use, view, views, (%~), (.=), (^.))

import Control.Monad (foldM, replicateM)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get)
import Control.Tracer qualified as Tracer

import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))

import GHC.Generics (Generic)

import Test.QuickCheck (Arbitrary, Gen, Property, (===))
import Test.QuickCheck qualified as Test
import Test.QuickCheck.Monadic (PropertyM)
import Test.QuickCheck.Monadic qualified as GenM

import Test.Tasty qualified as Tasty
import Test.Tasty.QuickCheck qualified as Tasty

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT)

import Data.Either (fromRight)
import Data.Maybe (listToMaybe)

import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)

import Marconi.Core.Experiment (wrappedIndexer)
import Marconi.Core.Experiment qualified as Core

newtype TestPoint = TestPoint { unwrapTestPoint :: Int }
    deriving stock (Generic)
    deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Show, FromField, ToField)
    deriving anyclass (SQL.FromRow)
    deriving anyclass (SQL.ToRow)

instance Core.HasGenesis TestPoint where
    genesis = 0

-- | We simplify the events to either `Insert` or `Rollback`
data Item event
    = Insert !TestPoint !event
    | Rollback !TestPoint
    deriving stock Show

-- | 'GenState' is used in generators of chain events to keep track of the latest slot
newtype GenState = GenState { _slotNo :: TestPoint }
    deriving stock Show

makeLenses 'GenState

-- | Generate an insert at the given slot
genInsert :: Arbitrary event => GenState -> Gen (Item event)
genInsert s = do
    xs <- Test.arbitrary
    pure $ Insert (s ^. slotNo + 1) xs

-- | Generate a rollback, 'GenState' set the maximal depth of the rollback
genRollback :: GenState -> Gen (Item event)
genRollback s = do
    n <- TestPoint <$> Test.choose (0, unwrapTestPoint $ s ^. slotNo)
    pure $ Rollback n

-- | Generate an insert or a rollback, rollback depth is uniform on the chain length
genItem
    :: Arbitrary event
    => Word -- ^ rollback frequency (insert weight is 100 - rollback frequency)
    -> StateT GenState Gen (Item event)
genItem f = do
    s <- get
    no <- use slotNo
    let f' = if no > 0 then f else 0 -- no rollback on genesis
    item <- lift $ Test.frequency
        [ (fromIntegral f',  genRollback s)
        , (100 - fromIntegral f', genInsert s)
        ]
    case item of
        Insert no' _ -> slotNo .= no'
        Rollback n   -> slotNo .= n
    pure item

genChain
    :: Arbitrary event
    => Word -- ^ Rollback percentage
    -> Int -- ^ Size
    -> Gen [Item event]
genChain percent size
    = evalStateT (replicateM size (genItem percent)) (GenState Core.genesis)

genLargeChain
    :: Arbitrary event
    => Word -- ^ Rollback percentage
    -> Gen [Item event]
genLargeChain p = do
    n <- Test.choose (50000,200000)
    genChain p n

-- | Chain events with 10% of rollback
newtype DefaultChain event = DefaultChain {_defaultChain :: [Item event]}

makeLenses 'DefaultChain

instance Arbitrary event => Arbitrary (DefaultChain event) where

    arbitrary = Test.sized $ fmap DefaultChain . genChain 10

-- | Chain events without any rollback
newtype ForwardChain event = ForwardChain {_forwardChain :: [Item event]}

makeLenses 'ForwardChain

instance Arbitrary event => Arbitrary (ForwardChain event) where

    arbitrary = Test.sized $ fmap ForwardChain . genChain 0

-- ** Event instances

newtype TestEvent = TestEvent Int
    deriving newtype (Arbitrary, Eq, Ord, Show, Num, FromField, ToField)

type instance Core.Point TestEvent = TestPoint

-- * Model

newtype IndexerModel e = IndexerModel {_model :: [(TestPoint, e)]}
    deriving stock Show

makeLenses ''IndexerModel

-- Build a model for the given chain of events
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
    -> IndexerTestRunner m event indexer
    -> (IndexerModel event -> a)
    -> (indexer event -> m a)
    -> (a -> a -> Property)
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
        indexer <- GenM.run $ foldM (flip process) initialIndexer chain
        iResult <- GenM.run $ indexerComputation indexer
        let model' = runModel chain
            mResult = modelComputation model'
        GenM.stop $ iResult `prop` mResult

-- | Compare an execution on the base model and one on the indexer
behaveLikeModel
    :: Eq a
    => Show a
    => Show event
    => Core.Point event ~ TestPoint
    => Core.IsIndex m event indexer
    => Core.Rewindable m event indexer
    => Gen [Item event]
    -> IndexerTestRunner m event indexer
    -> (IndexerModel event -> a)
    -> (indexer event -> m a)
    -> Property
behaveLikeModel genChain' runner modelComputation indexerComputation
    = compareToModelWith genChain' runner modelComputation indexerComputation (===)

-- | A test tree for the core functionalities of an indexer
indexingTestGroup
    :: ( Core.Rewindable m TestEvent indexer
    , Core.IsIndex m TestEvent indexer
    , Core.IsSync m TestEvent indexer
    , Core.Queryable
        (ExceptT (Core.QueryError (Core.EventsMatchingQuery TestEvent)) m)
        TestEvent
        (Core.EventsMatchingQuery TestEvent) indexer
    ) => String -> IndexerTestRunner m TestEvent indexer -> Tasty.TestTree
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

-- | A test tree for the core functionalities of an indexer
indexingPerformanceTest
    :: ( Core.Rewindable m TestEvent indexer
    , Core.IsIndex m TestEvent indexer
    , Core.IsSync m TestEvent indexer
    , Core.Queryable
        (ExceptT (Core.QueryError (Core.EventsMatchingQuery TestEvent)) m)
        TestEvent
        (Core.EventsMatchingQuery TestEvent) indexer
    ) => String -> IndexerTestRunner m TestEvent indexer -> Tasty.TestTree
indexingPerformanceTest indexerName runner
    = Tasty.testProperty (indexerName <> " performance check")
        $ Test.withMaxSuccess 50
        $ Test.within 1000000 $ storageBasedModelProperty (genLargeChain 10) runner

storageBasedModelProperty
    ::
    ( Core.Rewindable m event indexer
    , Core.IsIndex m event indexer
    , Core.IsSync m event indexer
    , Core.Point event ~ TestPoint
    , Show event
    , Eq event
    , Core.Queryable (ExceptT (Core.QueryError (Core.EventsMatchingQuery event)) m) event (Core.EventsMatchingQuery event) indexer
    )
    => Gen [Item event]
    -> IndexerTestRunner m event indexer
    -> Property
storageBasedModelProperty gen runner
    = let

        indexerEvents indexer = do
            p <- Core.lastSyncPoint indexer
            fmap (view Core.event)
                . fromRight []
                <$> Core.query' p Core.allEvents indexer

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
    => IndexerTestRunner IO e Core.ListIndexer
listIndexerRunner
    = IndexerTestRunner
        GenM.monadicIO
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
            " SELECT point, value \
            \ FROM index_model \
            \ WHERE point <= :point \
            \ ORDER BY point DESC"
            rowToResult

-- | A runner for a 'SQLiteIndexer'
sqliteIndexerRunner
    :: IndexerTestRunner IO TestEvent Core.SQLiteIndexer
sqliteIndexerRunner
    = IndexerTestRunner
        GenM.monadicIO
        (sqliteModelIndexer <$> initSQLite)

mixedModelLowMemoryIndexer
    :: SQL.Connection
    -> Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer TestEvent
mixedModelLowMemoryIndexer con
    = Core.mixedIndexer
        10
        2
        (sqliteModelIndexer con)
        Core.listIndexer

mixedModelHighMemoryIndexer
    :: SQL.Connection
    -> Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer TestEvent
mixedModelHighMemoryIndexer con
    = Core.mixedIndexer
        8192
        4096
        (sqliteModelIndexer con)
        Core.listIndexer

-- | A runner for a 'MixedIndexer' with a small in-memory storage
mixedLowMemoryIndexerRunner
    :: IndexerTestRunner IO TestEvent (Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer)
mixedLowMemoryIndexerRunner
    = IndexerTestRunner
        GenM.monadicIO
        (mixedModelLowMemoryIndexer <$> initSQLite)

-- | A runner for a 'MixedIndexer' with a large in-memory storage
mixedHighMemoryIndexerRunner
    :: IndexerTestRunner IO TestEvent (Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer)
mixedHighMemoryIndexerRunner
    = IndexerTestRunner
        GenM.monadicIO
        (mixedModelHighMemoryIndexer <$> initSQLite)

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

deriving via (Core.IndexWrapper (IndexerMVar indexer) Core.Coordinator)
    instance Core.IsIndex IO event (UnderCoordinator indexer)

deriving via (Core.IndexWrapper (IndexerMVar indexer) Core.Coordinator)
    instance Core.IsSync IO event (UnderCoordinator indexer)

instance Core.HasGenesis (Core.Point event)
    => Core.Rewindable IO event (UnderCoordinator indexer) where
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
    ( Core.IsIndex (ExceptT Core.IndexError IO) event wrapped
    , Core.IsSync (ExceptT Core.IndexError IO) event wrapped
    , Core.Resumable (ExceptT Core.IndexError IO) event wrapped
    , Core.Rewindable (ExceptT Core.IndexError IO) event wrapped
    , Core.HasGenesis (Core.Point event)
    , Ord (Core.Point event)
    ) => IndexerTestRunner IO event wrapped
    -> IndexerTestRunner IO event (UnderCoordinator wrapped)
coordinatorIndexerRunner wRunner
    = IndexerTestRunner
        (wRunner ^. indexerRunner)
        $ do
            wrapped <- wRunner ^. indexerGenerator
            (t, run) <- Core.createWorker pure id wrapped
            UnderCoordinator . Core.IndexWrapper (IndexerMVar t) <$> Core.start [run]
