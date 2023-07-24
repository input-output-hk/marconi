{-# LANGUAGE FlexibleContexts #-}

module Spec.Marconi.Tutorial.AddressCount where

import Cardano.Api qualified as C
import Control.Lens ((^.))
import Control.Monad (foldM, forM, forM_)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Gen.Marconi.ChainIndex.Types (genAddressInEra, genBlockNo, genChainPoint', genSlotNo)
import Hedgehog (Gen, Property, PropertyT, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen qualified as H
import Hedgehog.Range qualified as Range
import Marconi.Core.Experiment (
  HasGenesis,
  IndexerError,
  ListIndexer,
  Point,
  Queryable,
  Result,
  SQLiteIndexer,
  Timed (Timed),
  event,
  index,
  indexAll,
  mkListIndexer,
  point,
  query,
 )
import Marconi.Tutorial.Indexers.AddressCount (
  AddressCountEvent (AddressCountEvent, unAddressCountEvent),
  AddressCountQuery (AddressCountQuery),
  mkAddressCountSqliteIndexer,
 )

hprop_generator_preconditions :: Property
hprop_generator_preconditions = H.property $ do
  addrs <- fmap Set.fromList $ H.forAll $ Gen.list (Range.linear 1 20) genAddressAny
  events <- H.forAll $ Gen.list (Range.linear 1 10) $ genAddressCountEvent addrs

  let mkAddressCountAtEachPoint
        :: Timed a (Maybe AddressCountEvent) -> Maybe [(C.AddressAny, [(a, Int)])]
      mkAddressCountAtEachPoint te =
        fmap
          (\(AddressCountEvent m) -> fmap (\(a, c) -> (a, [(te ^. point, c)])) $ Map.toList m)
          $ te ^. event

      addressCountAtEachPoint = Map.fromListWith (<>) $ concat $ mapMaybe mkAddressCountAtEachPoint events

  H.cover 30 "At least one address appearing in only one block" $
    any (\cs -> not $ null cs) $
      Map.elems addressCountAtEachPoint
  H.cover 10 "At least one address appearing in at least 2 different blocks" $
    any (\cs -> length cs > 1) $
      Map.elems addressCountAtEachPoint

hprop_listIndexerAndSqliteIndexerReturnSameQueryResult :: Property
hprop_listIndexerAndSqliteIndexerReturnSameQueryResult = do
  let mkQuery te =
        case te ^. event of
          Nothing -> []
          Just e -> AddressCountQuery <$> Map.keys (unAddressCountEvent e)

  H.property $ do
    addrs <- fmap Set.fromList $ H.forAll $ Gen.list (Range.linear 1 20) genAddressAny
    mkPropertyListIndexerAndSqliteIndexerReturnSameQueryResult
      (Gen.list (Range.linear 1 5) $ genAddressCountEvent addrs)
      (mkAddressCountSqliteIndexer ":memory:")
      mkQuery

mkPropertyListIndexerAndSqliteIndexerReturnSameQueryResult
  :: ( MonadIO m
     , Show e
     , Show (Point e)
     , Ord (Point e)
     , HasGenesis (Point e)
     , Show (Result q)
     , Eq (Result q)
     , Queryable IO e q ListIndexer
     , Queryable IO e q SQLiteIndexer
     )
  => Gen [Timed (Point e) (Maybe e)]
  -> ExceptT IndexerError IO (SQLiteIndexer e)
  -> (Timed (Point e) (Maybe e) -> [q])
  -> PropertyT m ()
mkPropertyListIndexerAndSqliteIndexerReturnSameQueryResult genEvents initSqliteIndexer mkQueries = do
  events <- H.forAll genEvents
  lsIndex <- indexAll events mkListIndexer
  sqliteIndexerE <-
    liftIO $ runExceptT $ do
      sqliteIndexer <- initSqliteIndexer
      foldM (\indexer e -> index e indexer) sqliteIndexer events
  sqliteIndexer <- H.evalEither sqliteIndexerE

  let queries = concatMap mkQueries events
  randomPoint <- H.forAll $ H.element $ fmap (\te -> te ^. point) events
  forM_ queries $ \q -> do
    lsIndexResult <- liftIO $ query randomPoint q lsIndex
    sqliteIndexerResult <- liftIO $ query randomPoint q sqliteIndexer
    lsIndexResult === sqliteIndexerResult

{- | We pre-generate set of addresses. This is done in order to ensure that we generate
 'Timed AddressCountEvent' with the same address across different points. Thus far,
 'genAddressInEra' also never generates the same address, which is why we used this approach.
-}
genAddressCountEvent :: Set C.AddressAny -> Gen (Timed C.ChainPoint (Maybe AddressCountEvent))
genAddressCountEvent addrs = do
  -- This produces an error because of a discenpancy between the list indexer and the
  -- sqliteindexer.
  -- cp <- genChainPoint
  cp <- genChainPoint' genBlockNo genSlotNo
  addressCountEvent <-
    fmap Map.fromList $ do
      -- Could be empty
      addrsSubset <- Gen.subset addrs
      forM (Set.toList addrsSubset) $ \addr -> do
        count <- Gen.int (Range.linear 0 100)
        pure (addr, count)
  let ev = if Map.null addressCountEvent then Nothing else Just $ AddressCountEvent addressCountEvent
  pure $ Timed cp ev

genAddressAny :: Gen C.AddressAny
genAddressAny = do
  (C.AddressInEra _ addr) <- genAddressInEra C.BabbageEra
  pure $ C.toAddressAny addr
