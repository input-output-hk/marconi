{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Marconi.Cardano.Indexers.BlockInfo (
  propTests,
  unitTests,
) where

import Cardano.Api qualified as C
import Control.Concurrent (readMVar, threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Exception (throwIO)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson qualified as Aeson
import Data.Maybe (mapMaybe)
import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Extras.Test.Base qualified as Hedgehog
import Hedgehog.Gen qualified
import Marconi.Cardano.Core.Indexer.Worker (StandardWorker (StandardWorker))
import Marconi.Cardano.Core.Logger (defaultStdOutLogger, mkMarconiTrace)
import Marconi.Cardano.Core.Runner qualified as Runner
import Marconi.Cardano.Indexers (blockInfoBuilder)
import Marconi.Cardano.Indexers.BlockInfo qualified as BlockInfo
import Marconi.Core qualified as Core
import Test.Gen.Marconi.Cardano.Indexers.BlockInfo qualified as Test.BlockInfo
import Test.Helpers qualified as Helpers
import Test.Integration qualified as Integration
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

{- | Genuine property tests, in which more than one test can be run and
 - options on the number of tests might be configured by the caller.
-}
propTests :: TestTree
propTests =
  testGroup
    "Spec.Marconi.Cardano.Indexers.BlockInfo"
    [ testGroup
        "Indexer"
        [ testPropertyNamed
            "EventAt query works for blockInfo"
            "propRoundTripAtSlotBlockInfo"
            propRoundTripAtSlotBlockInfo
        , testPropertyNamed
            "BlockInfoIndexer can retrieve all the data it stores"
            "propRoundTripBlockInfo"
            propRoundTripBlockInfo
        , testPropertyNamed
            "EventAt query works as a list indexer"
            "propActLikeListIndexerOnEventAt"
            propActLikeListIndexerOnEventAt
        ]
    , testPropertyNamed
        "JSON event tripping test"
        "propTrippingBlockInfoJSON"
        propTrippingBlockInfoJSON
    ]

{- | Unit tests, defined with the Hedgehog API.
 - Tests defined @Hedgehog.'propertyOnce'@ or otherwise with
   a fixed number of test runs that should not be changed.
-}
unitTests :: TestTree
unitTests =
  testGroup
    "Spec.Marconi.Cardano.Indexers.BlockInfo"
    [ testGroup
        "End-to-end indexer tests with cardano-node-emulator"
        [ testPropertyNamed
            "Indexing a testnet and then submitting a transaction has the indexer receive at least one block"
            "endToEndBlockInfo"
            endToEndBlockInfo
        ]
    ]

-- | We can retrieve the event at a given slot
propRoundTripAtSlotBlockInfo :: Hedgehog.Property
propRoundTripAtSlotBlockInfo = Hedgehog.property $ do
  events <- Hedgehog.forAll Test.BlockInfo.genBlockInfoEvents
  event <- Hedgehog.forAll $ Hedgehog.Gen.element events
  indexer <- Hedgehog.evalExceptT (BlockInfo.mkBlockInfoIndexer ":memory:" >>= Core.indexAll events)
  retrievedEvents <-
    Hedgehog.evalExceptT $ Core.query (event ^. Core.point) Core.EventAtQuery indexer
  event ^. Core.event === retrievedEvents

-- | We can retrieve all the events
propRoundTripBlockInfo :: Hedgehog.Property
propRoundTripBlockInfo = Hedgehog.property $ do
  events <- Hedgehog.forAll Test.BlockInfo.genBlockInfoEvents
  let nonEmptyEvents = mapMaybe sequenceA events
  indexer <- Hedgehog.evalExceptT (BlockInfo.mkBlockInfoIndexer ":memory:" >>= Core.indexAll events)
  retrievedEvents <- Hedgehog.evalExceptT $ Core.queryLatest Core.allEvents indexer
  nonEmptyEvents === retrievedEvents

-- | On EventAt, the 'BlockInfoIndexer' behaves like a 'ListIndexer'
propActLikeListIndexerOnEventAt :: Hedgehog.Property
propActLikeListIndexerOnEventAt = Hedgehog.property $ do
  events <- Hedgehog.forAll Test.BlockInfo.genBlockInfoEvents
  indexer <- Hedgehog.evalExceptT (BlockInfo.mkBlockInfoIndexer ":memory:" >>= Core.indexAll events)
  referenceIndexer <- Core.indexAll events Core.mkListIndexer
  event <- Hedgehog.forAll $ Hedgehog.Gen.element events
  (testedResult :: Maybe BlockInfo.BlockInfo) <-
    Hedgehog.evalExceptT $ Core.query (event ^. Core.point) Core.EventAtQuery indexer
  refResult <-
    Hedgehog.evalExceptT $ Core.query (event ^. Core.point) Core.EventAtQuery referenceIndexer
  refResult === testedResult

-- | Standard tripping property for JSON
propTrippingBlockInfoJSON :: Hedgehog.Property
propTrippingBlockInfoJSON = Hedgehog.property $ do
  event <- Hedgehog.forAll Test.BlockInfo.genBlockInfo
  Hedgehog.tripping event Aeson.encode Aeson.eitherDecode

-- | Test for block info using cardano-node-emulator
endToEndBlockInfo :: Hedgehog.Property
endToEndBlockInfo = Helpers.unitTestWithTmpDir "." $ \tempPath -> do
  -- Setup
  (trace, _) <- liftIO $ defaultStdOutLogger "endToEndBlockInfo"
  let marconiTrace = mkMarconiTrace trace

  -- Local node config and connect info, with slots of length 100ms
  (nscConfig, _) <- Hedgehog.evalIO $ Integration.mkLocalNodeInfo tempPath 100

  -- Indexer preprocessor and configuration
  let
    catchupConfig = Integration.mkEndToEndCatchupConfig
    config = Integration.mkEndToEndRunIndexerConfig marconiTrace nscConfig Runner.withDistancePreprocessor

  StandardWorker mindexer worker <-
    Hedgehog.evalIO $
      either throwIO pure
        =<< runExceptT
          (blockInfoBuilder (config ^. Runner.runIndexerConfigSecurityParam) catchupConfig trace tempPath)
  coordinator <- Hedgehog.evalIO $ Core.mkCoordinator [worker]

  -- Start the testnet and indexer, and run the query.

  {- NOTE: PLT-8098
   startTestnet does not shutdown when the test is done.
   As a temporary measure to avoid polluting the test output, Integration.startTestnet squashes all
   SlotAdd log messages.
   -}
  Hedgehog.evalIO $ Integration.startTestnet nscConfig

  res <- Hedgehog.evalIO $
    Async.race (Runner.runIndexer config coordinator undefined) $
      do
        threadDelay 5_000_000

        indexer <- readMVar mindexer

        (queryEvents :: [Core.Timed C.ChainPoint BlockInfo.BlockInfo]) <-
          either throwIO pure
            =<< runExceptT (Core.queryLatest Core.allEvents indexer)

        pure $ not (null queryEvents)

  assertion <- Hedgehog.leftFail res
  Hedgehog.assert assertion
