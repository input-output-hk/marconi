{-# LANGUAGE TemplateHaskell #-}

-- | Provide facilities to run model-base tests on indexers
module Test.Marconi.Core.Experiment.ModelBased (
  -- * Indexer comparison
  compareIndexers,
  compareEventAtToListIndexer,

  -- ** Runners

  --
  -- Some runners for common indexers
  IndexerTestRunner (IndexerTestRunner),
  indexerRunner,
  indexerGenerator,
  ioIndexerRunner,
  listIndexerRunner,
) where

import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad.Except (ExceptT, lift, runExceptT)
import Data.Either (fromRight)
import Marconi.Core.Experiment qualified as Core
import Test.QuickCheck (Gen, Property, (===))
import Test.QuickCheck qualified as Test
import Test.QuickCheck.Gen qualified as Gen
import Test.QuickCheck.Monadic (PropertyM)
import Test.QuickCheck.Monadic qualified as GenM

-- | Used to map an indexer to a model
data IndexerTestRunner m event indexer = IndexerTestRunner
  { _indexerRunner :: !(PropertyM m Property -> Property)
  , _indexerGenerator :: !(m (indexer event))
  }

Lens.makeLenses ''IndexerTestRunner

{- | Compare the behaviour of two indexers.

use the runners to build the model indexer and the tested indexer,
apply the source to both of them and compare the resulting indexers using the provided function

It can be used either to compare an indxer implementation to a model one
or to ensure that an indexer transformer doesn't alter the behaviour of a model.
-}
compareIndexers
  :: (Monad m, Show src)
  => (src -> refIndexer event -> PropertyM m (refIndexer event))
  -- ^ the processing function for the model
  -> (src -> testIndexer event -> PropertyM m (testIndexer event))
  -- ^ the processing function for the tested indexer
  -> (src -> refIndexer event -> testIndexer event -> PropertyM m Property)
  -- ^ the property test
  -> Gen src
  -- ^ the generator for the source events
  -> IndexerTestRunner m event refIndexer
  -- ^ the model
  -> IndexerTestRunner m event testIndexer
  -- ^ the tested indexer
  -> Property
compareIndexers processRef processTested test gen refRunner testRunner =
  let r = refRunner ^. indexerRunner
   in Test.forAll gen $ \chain -> r $ do
        refIndexer <- GenM.run $ refRunner ^. indexerGenerator
        refResultIndexer <- processRef chain refIndexer
        testedIndexer <- GenM.run $ testRunner ^. indexerGenerator
        testedResultIndexer <- processTested chain testedIndexer
        test chain refResultIndexer testedResultIndexer

compareEventAtToListIndexer
  :: forall indexer event
   . ( Core.IsIndex (ExceptT Core.IndexerError IO) event indexer
     , Core.Queryable
        (ExceptT (Core.QueryError (Core.EventAtQuery event)) (ExceptT Core.IndexerError IO))
        event
        (Core.EventAtQuery event)
        indexer
     , Show event
     , Eq event
     , Show (Core.Point event)
     , Ord (Core.Point event)
     , Core.HasGenesis (Core.Point event)
     )
  => Gen [Core.Timed (Core.Point event) (Maybe event)]
  -> indexer event
  -> Property
compareEventAtToListIndexer gen testedIndexer =
  let getEventsAt point =
        fmap (fromRight (error "Query failed"))
          . Core.queryEither point Core.EventAtQuery
      equalityAtSlot evts x y =
        GenM.forAllM (Lens.view Core.point <$> Gen.elements evts) $ \point -> do
          xEvt :: Maybe event <- GenM.run $ getEventsAt point x
          yEvt <- GenM.run $ getEventsAt point y
          GenM.stop $ xEvt === yEvt
   in compareIndexers
        (\xs y -> lift $ Core.indexAll xs y)
        (\xs y -> lift $ Core.indexAll xs y)
        (\src x y -> equalityAtSlot src x y)
        gen
        listIndexerRunner
        (ioIndexerRunner $ pure testedIndexer)

-- | A runner for an indexer that provides can live in an @ExceptT erreIO@ monad
ioIndexerRunner
  :: ExceptT Core.IndexerError IO (indexer event)
  -> IndexerTestRunner (ExceptT Core.IndexerError IO) event indexer
ioIndexerRunner = IndexerTestRunner monadicExceptTIO

-- | A runner for a 'ListIndexer'
listIndexerRunner
  :: (Core.HasGenesis (Core.Point e))
  => IndexerTestRunner (ExceptT Core.IndexerError IO) e Core.ListIndexer
listIndexerRunner = ioIndexerRunner (pure Core.mkListIndexer)

monadicExceptTIO :: (Test.Testable a) => PropertyM (ExceptT err IO) a -> Property
monadicExceptTIO =
  GenM.monadic $ Test.ioProperty . fmap (fromRight $ Test.property False) . runExceptT
