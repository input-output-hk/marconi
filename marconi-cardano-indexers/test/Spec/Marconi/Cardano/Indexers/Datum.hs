{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Marconi.Cardano.Indexers.Datum (
  tests,
) where

import Control.Lens ((^.), (^..))
import Data.Maybe (mapMaybe)

import Control.Lens qualified as Lens
import Data.Aeson qualified as Aeson
import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Gen qualified
import Marconi.Cardano.Indexers.Datum qualified as Datum
import Marconi.Core qualified as Core
import Test.Gen.Marconi.Cardano.Indexers.Datum qualified as Test.Datum
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.Cardano.Indexers.Datum"
    [ testGroup
        "Indexer"
        [ testPropertyNamed
            "DatumIndexer can retrieve all the data it stores"
            "propResolveDatum"
            propResolveDatum
        ]
    , testPropertyNamed
        "JSON event tripping test"
        "propTrippingDatumJSON"
        propTrippingDatumJSON
    ]

-- | We can retrieve datum by hash if we query at its insertion slot
propResolveDatum :: Hedgehog.Property
propResolveDatum = Hedgehog.property $ do
  events <- Hedgehog.forAll Test.Datum.genDatumInfoEvents
  event <-
    Hedgehog.forAll $
      Hedgehog.Gen.element
        =<< Hedgehog.Gen.filter (not . null) (pure $ mapMaybe sequenceA events)
  datumInfo <- Hedgehog.forAll $ Hedgehog.Gen.element $ event ^.. Core.event . Lens.folded
  emptyIndexer <- Hedgehog.evalExceptT $ Datum.mkDatumIndexer ":memory:"
  indexer <- Hedgehog.evalExceptT $ Core.indexAll events emptyIndexer
  retrievedEvents <-
    Hedgehog.evalExceptT $
      Core.query
        (event ^. Core.point)
        (Datum.ResolveDatumQuery $ datumInfo ^. Datum.datumHash)
        indexer
  Just (datumInfo ^. Datum.datum) === retrievedEvents

-- | Standard tripping property for JSON
propTrippingDatumJSON :: Hedgehog.Property
propTrippingDatumJSON = Hedgehog.property $ do
  event <- Hedgehog.forAll Test.Datum.genDatumInfo
  Hedgehog.tripping event Aeson.encode Aeson.eitherDecode
