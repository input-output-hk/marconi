{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
module Marconi.CoreSpec (
  -- * Tests

  -- ** Main test suite
  indexingTestGroup,
  storageBasedModelProperty,
  lastSyncPointBasedModelProperty,

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
  withResumeTest,
  withRollbackFailureTest,

  -- * Mock chain
  DefaultChain,
  defaultChain,
  ForwardChain,
  forwardChain,

  -- ** Events
  Item (..),
  testPoint,
  TestPoint (..),
  TestEvent (..),
  testPointSlot,
  testPointHash,

  -- ** Generators
  GenChainConfig,
  chainSize,
  rollbackFrequency,
  rollbackDepth,
  lastTestPoints,
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

  -- ** WithStability
  withStabilityTestGroup,

  -- ** withStream
  withStreamTestGroup,
) where

import Control.Applicative (Const (Const))
import Control.Category qualified as Category
import Control.Comonad (Comonad (extract))
import Control.Concurrent (
  MVar,
  forkIO,
  newQSem,
  signalQSem,
  threadDelay,
  waitQSem,
 )
import Control.Concurrent qualified as Con
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (
  TBQueue,
  newTBQueueIO,
 )
import Control.Exception (bracket)
import Control.Lens (
  Getter,
  Lens',
  makePrisms,
  traversed,
  view,
  (%=),
  (%~),
  (-~),
  (.=),
  (.~),
  (^.),
  (^..),
  _2,
  _Just,
 )
import Control.Lens qualified as Lens
import Control.Monad (foldM, foldM_, replicateM, void)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State (StateT, evalStateT, gets)
import Control.Tracer qualified as Tracer
import Data.ByteString qualified as BS
import Data.Either (fromRight)
import Data.Foldable (Foldable (foldl'), find)
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.List qualified as List
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Monoid (Sum (Sum))
import Data.Ord (comparing)
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Word (Word64)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.FromField qualified as SQL
import Database.SQLite.Simple.FromRow qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField)
import GHC.Conc (ThreadStatus (ThreadFinished), threadStatus)
import GHC.Generics (Generic)
import Marconi.Core (Streamable (streamFrom), unwrap, withStream)
import Marconi.Core qualified as Core
import Marconi.Core.Coordinator qualified as Core (errorBox, threadIds)
import Network.Socket (
  Family (AF_UNIX),
  SockAddr (SockAddrUnix),
  SocketType (Stream),
  accept,
  bind,
  close,
  connect,
  defaultProtocol,
  listen,
  socket,
 )
import Network.Socket.ByteString (sendAll)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S
import System.IO.Temp qualified as Tmp
import Test.Marconi.Core.ModelBased qualified as Model
import Test.QuickCheck (Arbitrary, Gen, Property, (===), (==>))
import Test.QuickCheck qualified as Test
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Monadic (PropertyM)
import Test.QuickCheck.Monadic qualified as GenM
import Test.Tasty qualified as Tasty
import Test.Tasty.QuickCheck qualified as Tasty

data TestPoint = TestPoint
  { _testPointSlot :: Int
  -- ^ Slot number of the point.
  , _testPointHash :: UUID
  -- ^ Represents the block header hash of the point.
  }
  deriving stock (Eq, Generic, Show)

instance Ord TestPoint where
  compare (TestPoint s1 _) (TestPoint s2 _) = s1 `compare` s2

instance SQL.FromRow TestPoint where
  fromRow = do
    let uuidFieldParser f =
          do
            uuidText <- SQL.fromField f
            case UUID.fromText uuidText of
              Nothing -> SQL.returnError SQL.ConversionFailed f "Cannot deserialise UUID"
              Just uuid -> pure uuid
    slotNo <- SQL.field
    uuid <- SQL.fieldWith uuidFieldParser
    pure $ TestPoint slotNo uuid

instance SQL.ToRow TestPoint where
  toRow (TestPoint slotNo uuid) =
    [ SQL.SQLInteger $ fromIntegral slotNo
    , SQL.SQLText $ UUID.toText uuid
    ]

Lens.makeLenses 'TestPoint

instance Core.HasGenesis TestPoint where
  genesis = TestPoint 0 UUID.nil

-- | We simplify the events to either `Insert` or `Rollback`.
data Item event
  = Insert !TestPoint !(Maybe event)
  | Rollback !TestPoint
  | StableAt !TestPoint
  deriving stock (Show, Eq)

makePrisms ''Item

testPoint :: Lens' (Item event) TestPoint
testPoint = Lens.lens getter setter
  where
    getter (Insert point _) = point
    getter (Rollback point) = point
    getter (StableAt point) = point

    setter (Insert _ event) newPoint = Insert newPoint event
    setter (Rollback _) newPoint = Rollback newPoint
    setter (StableAt _) newPoint = StableAt newPoint

instance (Eq event) => Ord (Item event) where
  compare = comparing (Lens.view testPoint)

data LastStable = ShouldSendStableAt TestPoint | LastStable TestPoint

{- | 'GenChainConfig' is used in generators of chain events
 to determine the events that should be build
-}
data GenChainConfig = GenChainConfig
  { _chainSize :: Gen Int
  -- ^ Size of the chain to generate
  , _chainStableDepth :: Word
  -- ^ Min depth of a block for it to be considered stable (can't be rollbacked).
  , _emptyEventFrequency :: Word
  -- ^ How often an event is @Nothing@
  , _rollbackFrequency :: Word
  -- ^ How often a rollback can happen
  , _rollbackDepth :: Seq TestPoint -> Gen TestPoint
  -- ^ Rollback depth distribution
  , _lastStable :: LastStable
  -- ^ the value of the last `StableAt` event we sent
  , _lastTestPoints :: Seq TestPoint
  -- ^ Track the last 'chainStableDepth' unstable points.
  -- Used to generate the next point and to rollback to previous known points.
  }

chainSize :: Getter GenChainConfig (Gen Int)
chainSize = Lens.to _chainSize

chainStableDepth :: Getter GenChainConfig Word
chainStableDepth = Lens.to _chainStableDepth

emptyEventFrequency :: Lens' GenChainConfig Word
emptyEventFrequency =
  Lens.lens _emptyEventFrequency (\g f -> g{_emptyEventFrequency = f})

rollbackFrequency :: Lens' GenChainConfig Word
rollbackFrequency =
  Lens.lens _rollbackFrequency (\g f -> g{_rollbackFrequency = f})

rollbackDepth :: Lens' GenChainConfig (Seq TestPoint -> Gen TestPoint)
rollbackDepth =
  Lens.lens _rollbackDepth (\g d -> g{_rollbackDepth = d})

lastStable :: Lens' GenChainConfig LastStable
lastStable =
  Lens.lens _lastStable (\g points -> g{_lastStable = points})

lastStablePoint :: GenChainConfig -> TestPoint
lastStablePoint cfg = case cfg ^. lastStable of
  ShouldSendStableAt p -> p
  LastStable p -> p

lastTestPoints :: Lens' GenChainConfig (Seq TestPoint)
lastTestPoints =
  Lens.lens _lastTestPoints (\g points -> g{_lastTestPoints = points})

-- | Generate an insert at the given slot
genInsert :: (Arbitrary event) => Word -> TestPoint -> Gen (Item event)
genInsert f no = do
  xs <-
    Test.frequency
      [ (fromIntegral f, pure Nothing)
      , (100 - fromIntegral f, Just <$> Test.arbitrary)
      ]

  -- Each event is separated by a possibly non-continuous slot number.
  -- For example, [Point 1, Point 3, Point 4, Point 10, ..]
  let maxSlotNoDiffBetweenContinuousEvents = 10
  i <- Test.chooseInt (1, maxSlotNoDiffBetweenContinuousEvents)
  pointHash <- Test.arbitrary
  pure $ Insert (no & testPointSlot %~ (+ i) & testPointHash .~ pointHash) xs

-- | Generate a rollback, 'GenState' set the maximal depth of the rollback
genRollback :: GenChainConfig -> Gen (Item event)
genRollback cfg = do
  let gen = Lens.view rollbackDepth cfg
  Rollback <$> gen (cfg ^. lastTestPoints :|> lastStablePoint cfg)

-- | Generate an insert or a rollback, rollback depth is uniform on the chain length
genItem
  :: (Arbitrary event)
  => StateT GenChainConfig Gen (Item event)
genItem = do
  lastStable' <- Lens.use lastStable
  case lastStable' of
    ShouldSendStableAt pt -> do
      lastStable .= LastStable pt
      pure $ StableAt pt
    LastStable p -> do
      points <- Lens.use lastTestPoints
      genRollback' <- gets genRollback
      f <- Lens.use rollbackFrequency
      let f' = if null points then 0 else f -- no rollback on genesis
      g <- Lens.use emptyEventFrequency
      k <- Lens.use chainStableDepth
      let lastPoint = case points of
            Seq.Empty -> p
            (e :<| _) -> e
      item <-
        lift $
          Test.frequency
            [ (fromIntegral f', genRollback')
            , (100 - fromIntegral f', genInsert g lastPoint)
            ]
      case item of
        Insert newPoint _ -> do
          case points of
            other :|> stable | length points >= fromIntegral k -> do
              -- We remove the oldest point if the size the test points list is equal to K.
              lastTestPoints .= newPoint :<| other
              lastStable .= ShouldSendStableAt stable
            points' -> lastTestPoints .= newPoint :<| points'
        Rollback newPoint -> lastTestPoints %= Seq.filter (<= newPoint)
        StableAt _newPoint -> error "shouldn't generate stable here"
      pure item

-- | Apply an item to an indexer
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
  StableAt n -> Core.setLastStablePoint n

-- | Generate a chain using the given configuration
genChain
  :: (Arbitrary event)
  => GenChainConfig
  -> Gen [Item event]
genChain cfg = flip evalStateT cfg $ do
  size <- lift $ cfg ^. chainSize
  replicateM size genItem

uniformRollBack :: Seq TestPoint -> Gen TestPoint
uniformRollBack = Test.elements . Foldable.toList

-- | Generate a chain with @1_000_000@ to @1_200_000@ elements, and 5% of empty events
genLargeChain
  :: (Arbitrary event)
  => Word
  -- ^ Rollback percentage
  -> Gen [Item event]
genLargeChain p = do
  n <- Test.chooseInt (1_000_000, 1_200_000)
  let k = 2_160
  genChain $ GenChainConfig (pure n) k 5 p uniformRollBack (LastStable Core.genesis) Seq.Empty

-- | Chain events with 10% of rollback
newtype DefaultChain event = DefaultChain {_defaultChain :: [Item event]}

Lens.makeLenses 'DefaultChain

-- | Chain events with 10% of rollback
instance (Arbitrary event) => Arbitrary (DefaultChain event) where
  arbitrary = Test.sized $ \n ->
    let k = 50
     in DefaultChain
          <$> genChain (GenChainConfig (pure n) k 5 10 uniformRollBack (LastStable Core.genesis) Seq.Empty)

-- | Chain events without any rollback
newtype ForwardChain event = ForwardChain {_forwardChain :: [Item event]}

Lens.makeLenses 'ForwardChain

instance (Arbitrary event) => Arbitrary (ForwardChain event) where
  arbitrary = Test.sized $ \n ->
    let k = 50
     in ForwardChain
          <$> genChain (GenChainConfig (pure n) k 5 0 uniformRollBack (LastStable Core.genesis) Seq.Empty)

-- | Chain events without empty event
newtype ChainWithoutEmptyEvents event = ChainWithoutEmptyEvents {_chainWithoutEmptyEvents :: [Item event]}

Lens.makeLenses 'ChainWithoutEmptyEvents

instance (Arbitrary event) => Arbitrary (ChainWithoutEmptyEvents event) where
  arbitrary = Test.sized $ \n ->
    let k = 50
     in ChainWithoutEmptyEvents
          <$> genChain (GenChainConfig (pure n) k 0 10 uniformRollBack (LastStable Core.genesis) Seq.Empty)

-- ** Event instances

newtype TestEvent = TestEvent Int
  deriving stock (Generic)
  deriving newtype (Arbitrary, Eq, Ord, Show, Num, Enum, Real, Integral, FromField, ToField)
  deriving anyclass (SQL.FromRow, SQL.ToRow)

makePrisms ''TestEvent

type instance Core.Point TestEvent = TestPoint

instance SQL.ToRow (Core.Timed TestPoint TestEvent) where
  toRow (Core.Timed point event) = SQL.toRow point ++ SQL.toRow event

instance SQL.FromRow (Core.Timed TestPoint TestEvent) where
  fromRow = do
    let uuidFieldParser f =
          do
            uuidText <- SQL.fromField f
            case UUID.fromText uuidText of
              Nothing -> SQL.returnError SQL.ConversionFailed f "Cannot deserialise UUID"
              Just uuid -> pure uuid
    slotNo <- SQL.field
    uuid <- SQL.fieldWith uuidFieldParser
    event <- SQL.field
    pure $ Core.Timed (TestPoint slotNo uuid) event

-- * Model

-- | A simple model of a chain follower store the valid event in a list, the most recent first
newtype IndexerModel e = IndexerModel {_model :: [(TestPoint, Maybe e)]}
  deriving stock (Show, Functor, Foldable, Traversable)

Lens.makeLenses ''IndexerModel

-- | Build a model from a given chain of events
runModel :: [Item event] -> IndexerModel event
runModel =
  let modelStep :: IndexerModel event -> Item event -> IndexerModel event
      modelStep m (Insert w xs) = m & model %~ ((w, xs) :)
      modelStep m (Rollback n) = m & model %~ dropWhile ((> n) . fst)
      modelStep m (StableAt _n) = m
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
            Test.withMaxSuccess 5_000 $
              storageBasedModelProperty (Lens.view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "indexes events with rollbacks" $
            Test.withMaxSuccess 10_000 $
              storageBasedModelProperty (Lens.view defaultChain <$> Test.arbitrary) runner
        ]
    , Tasty.testGroup
        "IsSync"
        [ Tasty.testGroup
            "lastSyncPoint"
            [ Tasty.testProperty "in a chain without rollback" $
                Test.withMaxSuccess 5_000 $
                  lastSyncPointBasedModelProperty (Lens.view forwardChain <$> Test.arbitrary) runner
            , Tasty.testProperty "in a chain with rollbacks" $
                Test.withMaxSuccess 10_000 $
                  lastSyncPointBasedModelProperty (Lens.view defaultChain <$> Test.arbitrary) runner
            ]
        ]
    ]

-- | Check performances of an indexer, it should be able to ingest a large chain in 1 second
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
      Test.within 20_000_000 $
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
        fmap (Lens.view Core.event)
          . fromRight []
          <$> Core.queryLatestEither Core.allEvents indexer
   in behaveLikeModel
        gen
        runner
        (catMaybes . Lens.views model (fmap snd))
        indexerEvents

-- | The lastSyncPoint of an indexer is correctly set
lastSyncPointBasedModelProperty
  :: ( Core.IsIndex m event indexer
     , Core.IsSync m event indexer
     , Core.Point event ~ TestPoint
     , Show event
     )
  => Gen [Item event]
  -> Model.IndexerTestRunner m event indexer
  -> Property
lastSyncPointBasedModelProperty gen runner =
  behaveLikeModel
    gen
    runner
    (Lens.views model (maybe Core.genesis fst . listToMaybe))
    Core.lastSyncPoint

sqliteModelIndexerWithFile
  :: FilePath -> ExceptT Core.IndexerError IO (Core.SQLiteIndexer TestEvent)
sqliteModelIndexerWithFile filepath = do
  let extractor :: TestPoint -> Maybe Int
      extractor (TestPoint 0 _) = Nothing
      extractor (TestPoint p _) = Just p
  Core.mkSqliteIndexer
    filepath
    [ [sql|
      CREATE TABLE IF NOT EXISTS index_model
        ( pointSlotNo INT NOT NULL
        , pointHash TEXT NOT NULL
        , value INT NOT NULL
        )
      |]
    , [sql|
      CREATE TABLE IF NOT EXISTS sync
        ( lock TEXT PRIMARY KEY DEFAULT 'only one key'
        , pointSlotNo INT NOT NULL
        , pointHash TEXT NOT NULL
        , CONSTRAINT sync_lock CHECK (lock='only one key')
        )
      |]
    ]
    [
      [ Core.SQLInsertPlan
          ( \t ->
              [
                ( t ^. Core.point . testPointSlot
                , UUID.toText $ t ^. Core.point . testPointHash
                , t ^. Core.event
                )
              ]
          )
          "INSERT INTO index_model VALUES (?, ?, ?)"
      ]
    ]
    [Core.SQLRollbackPlan "index_model" "pointSlotNo" extractor]
    (Core.SetLastStablePointQuery "INSERT OR REPLACE INTO sync (pointSlotNo, pointHash) VALUES (?,?)")
    (Core.GetLastStablePointQuery "SELECT pointSlotNo, pointHash FROM sync")

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
      rowToResult
        :: Core.EventsMatchingQuery TestEvent
        -> [Core.Timed TestPoint TestEvent]
        -> [Core.Timed TestPoint TestEvent]
      rowToResult (Core.EventsMatchingQuery predicate) =
        mapMaybe (traverse predicate)
     in
      Core.querySQLiteIndexerWith
        (\p _ -> [":pointSlotNo" SQL.:= p ^. testPointSlot])
        ( const
            [sql|
            SELECT pointSlotNo, pointHash, value
            FROM index_model
            WHERE pointSlotNo <= :pointSlotNo
            ORDER BY pointSlotNo DESC
            |]
        )
        rowToResult

monadicExceptTIO :: (Test.Testable a) => PropertyM (ExceptT err IO) a -> Property
monadicExceptTIO =
  GenM.monadic $ Test.ioProperty . fmap (fromRight $ Test.property False) . runExceptT

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
  resultE <- runExceptT $ Core.standardMixedIndexer 0 1 dbIndexer
  case resultE of
    Left _err -> throwError $ Core.OtherIndexError "Could not create standardMixedIndexer"
    Right v -> pure v

mixedModelNoMemoryIndexer
  :: ExceptT
      Core.IndexerError
      IO
      (Core.MixedIndexer Core.SQLiteIndexer Core.ListIndexer TestEvent)
mixedModelNoMemoryIndexer = do
  dbIndexer <- sqliteModelIndexer
  resultE <- runExceptT $ Core.standardMixedIndexer 0 1 dbIndexer
  case resultE of
    Left _err -> throwError $ Core.OtherIndexError "Could not create standardMixedIndexer"
    Right v -> pure v

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
      4_096
      4_096
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

Lens.makeLenses ''UnderCoordinator

instance
  (MonadIO m, MonadError Core.IndexerError m)
  => Core.IsIndex m event (UnderCoordinator indexer)
  where
  index = Core.indexVia underCoordinator
  indexAllDescending = Core.indexAllDescendingVia underCoordinator
  rollback = Core.rollbackVia $ underCoordinator . Core.wrappedIndexer
  setLastStablePoint = Core.setLastStablePointVia $ underCoordinator . Core.wrappedIndexer

instance
  ( MonadIO m
  , MonadError Core.IndexerError m
  , Ord (Core.Point event)
  , Core.HasGenesis (Core.Point event)
  )
  => Core.IsSync m event (UnderCoordinator indexer)
  where
  lastSyncPoint = Core.lastSyncPointVia underCoordinator
  lastStablePoint = Core.lastStablePointVia underCoordinator

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
    let tmvar = ix ^. underCoordinator . Core.wrapperConfig . Lens.to getMVar
    indexer <- liftIO $ Con.takeMVar tmvar
    res <- Core.query p q indexer
    liftIO $ Con.putMVar tmvar indexer
    pure res

coordinatorIndexerRunner
  :: ( Core.WorkerIndexerType (ExceptT Core.IndexerError IO) event wrapped
     , Core.HasGenesis (Core.Point event)
     , Ord (Core.Point event)
     )
  => Model.IndexerTestRunner (ExceptT Core.IndexerError IO) event wrapped
  -> Model.IndexerTestRunner (ExceptT Core.IndexerError IO) event (UnderCoordinator wrapped)
coordinatorIndexerRunner = coordinatorIndexerRunnerWithPreprocessor Category.id

coordinatorIndexerRunnerWithPreprocessor
  :: ( Core.WorkerIndexerType (ExceptT Core.IndexerError IO) event wrapped
     , Core.HasGenesis (Core.Point event)
     , Ord (Core.Point event)
     )
  => Core.Preprocessor (ExceptT Core.IndexerError IO) (Core.Point event) event event
  -> Model.IndexerTestRunner (ExceptT Core.IndexerError IO) event wrapped
  -> Model.IndexerTestRunner (ExceptT Core.IndexerError IO) event (UnderCoordinator wrapped)
coordinatorIndexerRunnerWithPreprocessor pre wRunner = Model.IndexerTestRunner monadicExceptTIO $ do
  wrapped <- wRunner ^. Model.indexerGenerator
  workerIndexer <- lift $ Core.createWorkerWithPreprocessing "TestWorker" pre wrapped
  UnderCoordinator . Core.IndexTransformer (IndexerMVar $ Core.workerIndexerVar workerIndexer)
    <$> lift (Core.mkCoordinator [Core.worker workerIndexer])

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
          . Lens.folded
          . Lens.filtered (isBefore p)
          . Core.event
          . Lens.filtered f

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
              (\p' _q' -> [":pointSlotNo" SQL.:= p' ^. testPointSlot])
              ( const
                  " SELECT value               \
                  \ FROM index_model           \
                  \ WHERE pointSlotNo <= :pointSlotNo      \
                  \ ORDER BY pointSlotNo DESC"
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
            Test.withMaxSuccess 10_000 $
              cacheHitProperty (Lens.view defaultChain <$> Test.arbitrary) oddCacheRunner
        , Tasty.testProperty "Miss cache" $
            Test.withMaxSuccess 10_000 $
              cacheMissProperty (Lens.view defaultChain <$> Test.arbitrary) oddCacheRunner
        ]
    , Tasty.testGroup
        "With SQLiteIndexer"
        [ Tasty.testProperty "Hit cache" $
            Test.withMaxSuccess 10_000 $
              cacheHitProperty (Lens.view defaultChain <$> Test.arbitrary) sqlLiteCacheRunner
        , Tasty.testProperty "Miss cache" $
            Test.withMaxSuccess 10_000 $
              cacheMissProperty (Lens.view defaultChain <$> Test.arbitrary) sqlLiteCacheRunner
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
        (Lens.views model $ mapMaybe (find odd . snd))
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
      modelEvents = Lens.views model $ mapMaybe (find even . snd)
   in behaveLikeModel
        gen
        indexer
        modelEvents
        indexerEvents

-- | A runner for a the 'WithDelay' tranformer
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
        fmap (Lens.view Core.event)
          . fromRight []
          <$> Core.queryLatestEither Core.allEvents indexer'

      modelEvents lastSync =
        Lens.views model (mapMaybe snd . filter ((lastSync >=) . fst))

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
            Test.withMaxSuccess 5_000 $
              delayProperty 0 (Lens.view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "indexes events with rollbacks" $
            Test.withMaxSuccess 10_000 $
              delayProperty 0 (Lens.view defaultChain <$> Test.arbitrary) runner
        ]
    , Tasty.testGroup
        "10 delay"
        [ Tasty.testProperty "in a chain without rollback" $
            Test.withMaxSuccess 5_000 $
              delayProperty 10 (Lens.view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "in a chain with rollbacks" $
            Test.withMaxSuccess 10_000 $
              delayProperty 10 (Lens.view defaultChain <$> Test.arbitrary) runner
        ]
    ]

-- | A runner for a the 'WithCatchup tranformer.
withCatchupRunner
  :: (Monad m)
  => (Core.Point event ~ TestPoint)
  => Word64
  -> Word64
  -> Model.IndexerTestRunner m event wrapped
  -> Model.IndexerTestRunner m event (Core.WithCatchup wrapped)
withCatchupRunner batchSize bypassDistance wRunner =
  let computeDistance (TestPoint d _) _ = fromIntegral $ 100 - d
   in Model.IndexerTestRunner
        (wRunner ^. Model.indexerRunner)
        ( Core.withCatchup
            computeDistance
            (Core.mkCatchupConfig batchSize bypassDistance)
            <$> wRunner
              ^. Model.indexerGenerator
        )

catchupProperty
  :: (MonadIO m)
  => (Core.IsIndex m TestEvent indexer)
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
        fmap (Lens.view Core.event)
          . fromRight []
          <$> Core.queryLatestEither Core.allEvents indexer'

      modelEvents lastSync =
        Lens.views model (mapMaybe snd . filter ((lastSync >=) . fst))

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
  :: (MonadIO m)
  => (Core.IsIndex m TestEvent indexer)
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
            Test.withMaxSuccess 5_000 $
              catchupProperty 0 0 (Lens.view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "indexes events with rollbacks" $
            Test.withMaxSuccess 10_000 $
              catchupProperty 0 0 (Lens.view defaultChain <$> Test.arbitrary) runner
        ]
    , Tasty.testGroup
        "100 bufferSize, stop on tip (100 events)"
        [ Tasty.testProperty "indexes events without rollback" $
            Test.withMaxSuccess 5_000 $
              catchupProperty 100 0 (Lens.view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "indexes events with rollbacks" $
            Test.withMaxSuccess 10_000 $
              catchupProperty 100 0 (Lens.view defaultChain <$> Test.arbitrary) runner
        ]
    , Tasty.testGroup
        "1000 bufferSize, stop on tip (100 events)"
        [ Tasty.testProperty "indexes events without rollback" $
            Test.withMaxSuccess 5_000 $
              catchupProperty 1_000 0 (Lens.view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "indexes events with rollbacks" $
            Test.withMaxSuccess 10_000 $
              catchupProperty 1_000 0 (Lens.view defaultChain <$> Test.arbitrary) runner
        ]
    , Tasty.testGroup
        "0 bufferSize, stop 10 to tip (100 events)"
        [ Tasty.testProperty "indexes events without rollback" $
            Test.withMaxSuccess 5_000 $
              catchupProperty 0 10 (Lens.view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "indexes events with rollbacks" $
            Test.withMaxSuccess 10_000 $
              catchupProperty 0 10 (Lens.view defaultChain <$> Test.arbitrary) runner
        ]
    , Tasty.testGroup
        "100 bufferSize, stop 10 to tip (100 events)"
        [ Tasty.testProperty "indexes events without rollback" $
            Test.withMaxSuccess 5_000 $
              catchupProperty 100 10 (Lens.view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "indexes events with rollbacks" $
            Test.withMaxSuccess 10_000 $
              catchupProperty 100 10 (Lens.view defaultChain <$> Test.arbitrary) runner
        ]
    , Tasty.testGroup
        "1000 bufferSize, stop 10 to tip (100 events)"
        [ Tasty.testProperty "indexes events without rollback" $
            Test.withMaxSuccess 5_000 $
              catchupProperty 1_000 10 (Lens.view forwardChain <$> Test.arbitrary) runner
        , Tasty.testProperty "indexes events with rollbacks" $
            Test.withMaxSuccess 10_000 $
              catchupProperty 1_000 10 (Lens.view defaultChain <$> Test.arbitrary) runner
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
      waitForKill = GenM.run $ liftIO $ Con.threadDelay 1_000
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
    Test.withMaxSuccess 1_000 $
      stopCoordinatorProperty (Lens.view defaultChain <$> Test.arbitrary) runner

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
    origSyncPoint <- GenM.run $ Core.lastStablePoint indexer'
    resumedSyncPoint <- GenM.run $ Core.lastStablePoint indexer''
    lift $ Core.close indexer'
    lift $ Core.close indexer''
    pure $ origSyncPoint === resumedSyncPoint

resumeSQLiteLastSyncTest :: Tasty.TestTree
resumeSQLiteLastSyncTest =
  Tasty.testProperty "SQLiteIndexer - stop and restart restore lastStablePoint" $
    Test.withMaxSuccess 100 $
      resumeLastSyncProperty
        sqliteModelIndexerWithFile
        (Lens.view chainWithoutEmptyEvents <$> Test.arbitrary)

resumeMixedLastSyncTest :: Tasty.TestTree
resumeMixedLastSyncTest =
  Tasty.testProperty "MixedIndexer - stop and restart restore lastStablePoint" $
    Test.withMaxSuccess 100 $
      resumeLastSyncProperty
        mixedModelNoMemoryIndexerWithFile
        (Lens.view chainWithoutEmptyEvents <$> Test.arbitrary)

memorySizeUpdateProperty
  :: Gen [Item TestEvent]
  -> Property
memorySizeUpdateProperty gen =
  let genSplit = do
        chain <- gen
        changeAt <- Test.choose (0, length chain)
        pure (chain, changeAt)

      indexerEvents indexer =
        fmap (Lens.view Core.event)
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
    Test.withMaxSuccess 10_000 $
      memorySizeUpdateProperty (Lens.view defaultChain <$> Test.arbitrary)

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
    Test.withMaxSuccess 10_000 $
      cacheUpdateProperty (Lens.view defaultChain <$> Test.arbitrary)

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
        fmap (Lens.view Core.event)
          . fromRight []
          <$> Core.queryLatestEither Core.allEvents indexer'

      modelEvents lastSync =
        Lens.views model (mapMaybe (fmap abs . snd) . filter ((lastSync >=) . fst))

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
    Test.withMaxSuccess 10_000 $
      withTransformProperty (Lens.view defaultChain <$> Test.arbitrary)

{- | Outline of the property:

 General idea of property:

 * Generate a chain with 'Insert' and 'Rollback's.
 * Select a random subset of that chain.
 * Create a `ListIndexer` and index the chain subset.
 * Create a `WithResume ListIndexer` and initialize using the previous `ListIndexer`.
 * Create a new chain from the generated one where the Rollbacks in the immutable portion (given
 k) are "applied", thus we remove all rollback events from the immutable part while keeping
 rollbacks from the mutable part.
 * Index this new chain using the existing 'WithResume ListIndexer'.
 * Compare indexed events with a 'ListIndexer' that indexed the new chain.
-}
propWithResumeShouldDrainAlreadySyncedEvents :: Property
propWithResumeShouldDrainAlreadySyncedEvents =
  let getIndexerEvents
        :: ( Monad m
           , Core.Queryable
              (ExceptT (Core.QueryError (Core.EventsMatchingQuery TestEvent)) m)
              TestEvent
              (Core.EventsMatchingQuery TestEvent)
              indexer
           , Core.IsSync m TestEvent indexer
           )
        => indexer TestEvent
        -> m [Core.Timed TestPoint TestEvent]
      getIndexerEvents indexer' = fromRight [] <$> Core.queryLatestEither Core.allEvents indexer'

      applyRollbacksOnChain :: (Eq event) => [Item event] -> [Item event]
      applyRollbacksOnChain chain =
        let (IndexerModel events) = runModel chain
         in List.sort $ fmap (uncurry Insert) events

      gen :: Gen ([Item TestEvent], Int, Word)
      gen = Test.sized $ \n -> do
        k <- Test.chooseInt (1, 10)
        (chain :: [Item TestEvent]) <-
          genChain $
            GenChainConfig (pure n) (fromIntegral k) 5 50 uniformRollBack (LastStable Core.genesis) Seq.Empty
        chainSubsetSize <- Test.chooseInt (0, length chain)
        pure (chain, chainSubsetSize, fromIntegral k)
   in monadicExceptTIO @() $ GenM.forAllM gen $ \(chain, chainSubsetSize, k) -> do
        -- Select random subset of the chain and index it with a ListIndexer
        -- This correspons to an indexer which starts indexing events from genesis up until a
        -- certain point before being stopped.
        let chainSubset = take chainSubsetSize chain
        indexer <- GenM.run $ foldM (flip process) Core.mkListIndexer chainSubset
        -- Initialize a 'WithResume' indexer using the previously created ListIndexer.
        -- Then, re-index the full chain from scratch.
        -- Resume indexing by restarting from genesis and using last immutable sync point.
        lastStable' <- GenM.run $ Core.lastStablePoint indexer
        let runner =
              coordinatorIndexerRunnerWithPreprocessor (Core.withResume lastStable')
              -- resume assumes that the indexer has rollbacked to its last stable point
              $
                Model.ioIndexerRunner (Core.rollback lastStable' indexer)
        let (immutableChainPart, mutableChainPart) = splitAt (length chain - fromIntegral k) chain
            immutableChainPartWithoutRollbacks = applyRollbacksOnChain immutableChainPart
            fullChain = Rollback Core.genesis : immutableChainPartWithoutRollbacks ++ mutableChainPart

            withResumeIndexedEvents = fmap (fmap (Lens.view Core.event)) . getIndexerEvents
        GenM.stop $
          behaveLikeModel
            (pure fullChain)
            runner
            (catMaybes . Lens.views model (fmap snd))
            withResumeIndexedEvents

withResumeTest :: Tasty.TestTree
withResumeTest =
  Tasty.testProperty "WithResume should drain already synced events" $
    Test.withMaxSuccess 10_000 propWithResumeShouldDrainAlreadySyncedEvents

-- | A runner for a the 'WithFold' tranformer (using withFoldMap)
withFoldMapRunner
  :: ( MonadError Core.IndexerError m
     , Monoid output
     , Core.IsSync m output wrapped
     , Core.HasGenesis (Core.Point output)
     , Core.Queryable
        ( ExceptT
            (Core.QueryError (Core.EventAtQuery output))
            m
        )
        output
        (Core.EventAtQuery output)
        wrapped
     , Ord (Core.Point output)
     )
  => (input -> output)
  -> Model.IndexerTestRunner m output wrapped
  -> Model.IndexerTestRunner m input (Core.WithFold m wrapped output)
withFoldMapRunner f wRunner =
  Model.IndexerTestRunner
    (wRunner ^. Model.indexerRunner)
    (Core.withFoldMap Core.getLastEventAtQueryValue f <$> wRunner ^. Model.indexerGenerator)

deriving via (Sum Int) instance Semigroup TestEvent
deriving via (Sum Int) instance Monoid TestEvent

withAggregateProperty
  :: Gen [Item TestEvent]
  -> Property
withAggregateProperty gen =
  let indexerEvents indexer' =
        fmap (Lens.view Core.event)
          . fromRight []
          <$> Core.queryLatestEither Core.allEvents indexer'

      modelAggregate (Just x) (Just y) = Just $ x + y
      modelAggregate Nothing (Just y) = Just y
      modelAggregate (Just x) Nothing = Just x
      modelAggregate Nothing Nothing = Nothing

      modelEvents lastSync =
        catMaybes . init . scanr modelAggregate Nothing
          <$> Lens.views model (fmap snd . filter ((lastSync >=) . fst))

      runner = withFoldMapRunner id Model.listIndexerRunner

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
    Test.withMaxSuccess 10_000 $
      withAggregateProperty (Lens.view defaultChain <$> Test.arbitrary)

-- | Provide an indexer that fails on rollback
newtype WithRollbackFailure indexer event = WithRollbackFailure
  {_withRollbackFailure :: Core.IndexTransformer (Const ()) indexer event}

Lens.makeLenses ''WithRollbackFailure

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
  unwrap = withRollbackFailure . Core.wrappedIndexer

deriving via
  (Core.IndexTransformer (Const ()) indexer)
  instance
    (Core.IsSync m event indexer) => Core.IsSync m event (WithRollbackFailure indexer)

instance (Core.IsIndex m event indexer) => Core.IsIndex m event (WithRollbackFailure indexer) where
  index = Core.indexVia Core.unwrap
  rollback _ _ = error "STOP"
  setLastStablePoint = Core.setLastStablePointVia Core.unwrap

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
   in Test.within 10_000_000 $ Test.expectFailure $ Test.forAll gen $ \chain -> r $ do
        initialIndexer <- GenM.run genIndexer
        void $ GenM.run $ foldM (flip process) initialIndexer (chain ++ [Rollback Core.genesis])
        GenM.stop True

withRollbackFailureTest :: Tasty.TestTree
withRollbackFailureTest =
  Tasty.testProperty "Rollback failure in a worker exit nicely" $
    Test.withMaxSuccess 10_000 $
      checkOutOfRollback (Lens.view defaultChain <$> Test.arbitrary)

-- * withStream

withStreamTestGroup :: Tasty.TestTree
withStreamTestGroup =
  Tasty.testGroup
    "StreamTests"
    [ Tasty.testGroup
        "Stream"
        [ Tasty.testGroup
            "withStreamTBQueue"
            [ Tasty.testProperty "TBQueue streaming works" propWithStreamTBQueue
            ]
        , Tasty.testGroup
            "withStreamSocket"
            [ Tasty.testProperty "Socket streaming works" propWithStreamSocket
            ]
        ]
    ]

{- | Test that a stream from a @TBQueue@ is isomorphic to the original list that was used to create
     the stream.
-}
propWithStreamTBQueue :: Property
propWithStreamTBQueue = monadicExceptTIO @() $ GenM.forAllM genChainWithInstability $ \args -> do
  let chainSubset = take (chainSizeSubset args) (eventGenerator args)
  q :: TBQueue Int <- liftIO $ newTBQueueIO 10
  _ <- liftIO $
    forkIO $
      void $
        runExceptT $ do
          let toInt = view (Core.event . _TestEvent)
          let indexer = withStream toInt q Core.mkListIndexer
          foldM_ (flip process) indexer chainSubset

  let testEvents :: [TestEvent] = chainSubset ^.. traversed . _Insert . _2 . _Just
      expected = fmap (view _TestEvent) testEvents
      stream = S.take (length testEvents) $ streamFrom q

  actual <- liftIO $ S.toList_ stream

  GenM.stop (actual == expected)

{- | Test that a concurrent server and client pair can send and receive over a socket, respectively,
     such that the client can consume from the socket as a stream.
-}
propWithStreamSocket :: Property
propWithStreamSocket = monadicExceptTIO @() $ GenM.forAllM genChainWithInstability $ \args -> do
  (_, (actual, expected)) <- liftIO
    $ Tmp.withSystemTempFile
      "stream-socket"
    $ \path _ -> do
      let chainSubset = take (chainSizeSubset args) (eventGenerator args)
      serverStarted <- newQSem 1
      concurrently
        (server path chainSubset serverStarted)
        (client path chainSubset serverStarted)

  GenM.stop (actual == expected)
  where
    server socketPath chainSubset serverStarted = bracket (runUnixSocketServer socketPath) close $ \s -> do
      signalQSem serverStarted
      (conn, _) <- accept s
      _ <- runExceptT $ do
        let toInt = view (Core.event . _TestEvent)
            indexer = withStream toInt conn Core.mkListIndexer
        foldM_ (flip process) indexer chainSubset
      sendAll conn BS.empty
      close conn

    client socketPath chainSubset serverStarted = do
      waitQSem serverStarted

      -- We need to delay because otherwise we might try to connect before @accept s@ runs
      threadDelay 100
      bracket (runUnixSocketClient socketPath) close $ \s -> do
        let testEvents :: [TestEvent] = chainSubset ^.. traversed . _Insert . _2 . _Just
            stream :: Stream (Of Int) IO () = streamFrom s
        str <- S.toList_ stream
        pure (str, fmap (view _TestEvent) testEvents)

    runUnixSocketServer socketPath = do
      sock <- socket AF_UNIX Stream defaultProtocol
      bind sock (SockAddrUnix socketPath)
      listen sock 1
      pure sock
    runUnixSocketClient socketPath = do
      sock <- socket AF_UNIX Stream defaultProtocol
      connect sock (SockAddrUnix socketPath)
      pure sock

-- * Query modification - Stability
withStabilityTestGroup :: Tasty.TestTree
withStabilityTestGroup =
  Tasty.testGroup
    "StabilityTests"
    [ Tasty.testGroup
        "Stability"
        [ Tasty.testGroup
            "withStability"
            [ Tasty.testProperty "all results are stable" allStable
            , Tasty.testProperty "not all results are stable" notAllStable
            , Tasty.testProperty "withStability will not alter the events" withStabilityUnchanged
            ]
        , Tasty.testGroup
            "withStabilityAt"
            [ Tasty.testProperty "all results are stable at a specific point" stableAt
            , Tasty.testProperty "no results are stable at a specific point" notStableAt
            , Tasty.testProperty "withStabilityAt will not alter the events" withStabilityAtUnchanged
            ]
        ]
    ]

-- * withStability
notAllStable :: Property
notAllStable = monadicExceptTIO @() $ GenM.forAllM genChainWithInstability $ \args -> do
  (indexer, events) <- processCommonParts args
  let n = sizedN args
  eventsWithStab <-
    Core.withStability
      indexer
      (Core.Timed (TestPoint (n + 1) UUID.nil) (TestEvent (n + 1)) : events)
  GenM.stop $ not (all isStable eventsWithStab)

allStable :: Property
allStable = monadicExceptTIO @() $ GenM.forAllM genStableChain $ \args -> do
  (indexer, events) <- processCommonParts args
  eventsWithStab <- Core.withStability indexer events
  GenM.stop $ all isStable eventsWithStab

withStabilityUnchanged :: Property
withStabilityUnchanged = monadicExceptTIO @() $ GenM.forAllM genStableChain $ \args -> do
  (indexer, events) <- processCommonParts args
  eventsWithStab <-
    Core.withStability indexer events
  GenM.stop $ events === fmap extract eventsWithStab

-- * withStabilityAt
stableAt :: Property
stableAt = monadicExceptTIO @() $ GenM.forAllM genChainWithInstability $ \args -> do
  (indexer, events) <- processCommonParts args
  eventsWithStab <- Core.withStabilityAt indexer (TestPoint (sizedN args) UUID.nil) events
  GenM.stop $ all isStable eventsWithStab

notStableAt :: Property
notStableAt = monadicExceptTIO @() $ GenM.forAllM genChainWithInstability $ \args -> do
  (indexer, events) <- processCommonParts args
  eventsWithStab <- Core.withStabilityAt indexer (TestPoint (sizedN args + 1) UUID.nil) events
  GenM.stop $ not (any isStable eventsWithStab)

withStabilityAtUnchanged :: Property
withStabilityAtUnchanged = monadicExceptTIO @() $ GenM.forAllM genStableChain $ \args -> do
  (indexer, events) <- processCommonParts args
  eventsWithStab <- Core.withStabilityAt indexer (TestPoint (sizedN args + 1) UUID.nil) events
  GenM.stop $ events === fmap extract eventsWithStab

-- * Stability helpers

data CommonStabilityParts = CommonStabilityParts
  { eventGenerator :: [Item TestEvent]
  , chainSizeSubset :: Int
  , sizedN :: Int
  }
  deriving (Show)

-- | Helper for common chain generation logic
genCommonChain :: (Int -> TestPoint) -> Gen CommonStabilityParts
genCommonChain testPoint' = Test.sized $ \n -> do
  k <- Test.chooseInt (1, 10)
  (chain :: [Item TestEvent]) <-
    genChain $
      GenChainConfig
        (pure n)
        (fromIntegral k)
        5
        50
        uniformRollBack
        (LastStable (testPoint' n))
        Seq.Empty
  chainSubsetSize <- Test.chooseInt (1, length chain)
  pure $ CommonStabilityParts chain chainSubsetSize n

genChainWithInstability :: Gen CommonStabilityParts
genChainWithInstability = genCommonChain (flip TestPoint UUID.nil)

genStableChain :: Gen CommonStabilityParts
genStableChain = genCommonChain (const Core.genesis)

-- | Helper to generate the indexer and query events
processCommonParts
  :: CommonStabilityParts
  -> PropertyM
      (ExceptT (Core.QueryError (Core.EventsMatchingQuery TestEvent)) IO)
      (Core.ListIndexer TestEvent, [Core.Timed (Core.Point TestEvent) TestEvent])
processCommonParts args = do
  let chainSubset = take (chainSizeSubset args) (eventGenerator args)
  indexer <- GenM.run $ foldM (flip process) Core.mkListIndexer chainSubset
  indexerWithSetPoint <- Core.setLastStablePoint (TestPoint (sizedN args) UUID.nil) indexer
  events <-
    GenM.run $
      Core.query
        (TestPoint 0 UUID.nil)
        (Core.EventsMatchingQuery Just)
        indexerWithSetPoint
  return (indexerWithSetPoint, events)

isStable :: Core.Stability a -> Bool
isStable (Core.Stable _) = True
isStable _ = False
