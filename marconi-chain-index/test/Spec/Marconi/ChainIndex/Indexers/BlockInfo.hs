{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Marconi.ChainIndex.Indexers.BlockInfo (
  propTests,
  unitTests,
  getBlockInfoEvents,
  genBlockInfo,
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
import Data.Time qualified as Time
import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Extras.Test.Base qualified as Hedgehog
import Hedgehog.Gen qualified
import Hedgehog.Range qualified
import Marconi.ChainIndex.Indexers (blockInfoBuilder)
import Marconi.ChainIndex.Indexers.BlockInfo (BlockInfo (BlockInfo))
import Marconi.ChainIndex.Indexers.BlockInfo qualified as BlockInfo
import Marconi.ChainIndex.Indexers.Worker (StandardWorker (StandardWorker))
import Marconi.ChainIndex.Logger (defaultStdOutLogger, mkMarconiTrace)
import Marconi.ChainIndex.Runner qualified as Runner
import Marconi.ChainIndex.Types (RetryConfig (RetryConfig))
import Marconi.Core qualified as Core
import Test.Gen.Marconi.ChainIndex.Mockchain qualified as Gen
import Test.Gen.Marconi.ChainIndex.Types qualified as CGen
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
    "Spec.Marconi.ChainIndex.Indexers.BlockInfo"
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
    "Spec.Marconi.ChainIndex.Indexers.BlockInfo"
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
  events <- Hedgehog.forAll $ getBlockInfoEvents <$> Gen.genMockchainWithInfo
  event <- Hedgehog.forAll $ Hedgehog.Gen.element events
  indexer <- Hedgehog.evalExceptT (BlockInfo.mkBlockInfoIndexer ":memory:" >>= Core.indexAll events)
  retrievedEvents <-
    Hedgehog.evalExceptT $ Core.query (event ^. Core.point) Core.EventAtQuery indexer
  event ^. Core.event === retrievedEvents

-- | We can retrieve all the events
propRoundTripBlockInfo :: Hedgehog.Property
propRoundTripBlockInfo = Hedgehog.property $ do
  events <- Hedgehog.forAll $ getBlockInfoEvents <$> Gen.genMockchainWithInfo
  let nonEmptyEvents = mapMaybe sequenceA events
  indexer <- Hedgehog.evalExceptT (BlockInfo.mkBlockInfoIndexer ":memory:" >>= Core.indexAll events)
  retrievedEvents <- Hedgehog.evalExceptT $ Core.queryLatest Core.allEvents indexer
  nonEmptyEvents === retrievedEvents

-- | On EventAt, the 'BlockInfoIndexer' behaves like a 'ListIndexer'
propActLikeListIndexerOnEventAt :: Hedgehog.Property
propActLikeListIndexerOnEventAt = Hedgehog.property $ do
  events <- Hedgehog.forAll $ getBlockInfoEvents <$> Gen.genMockchainWithInfo
  indexer <- Hedgehog.evalExceptT (BlockInfo.mkBlockInfoIndexer ":memory:" >>= Core.indexAll events)
  referenceIndexer <- Core.indexAll events Core.mkListIndexer
  event <- Hedgehog.forAll $ Hedgehog.Gen.element events
  (testedResult :: Maybe BlockInfo) <-
    Hedgehog.evalExceptT $ Core.query (event ^. Core.point) Core.EventAtQuery indexer
  refResult <-
    Hedgehog.evalExceptT $ Core.query (event ^. Core.point) Core.EventAtQuery referenceIndexer
  refResult === testedResult

-- | Standard tripping property for JSON
propTrippingBlockInfoJSON :: Hedgehog.Property
propTrippingBlockInfoJSON = Hedgehog.property $ do
  event <- Hedgehog.forAll genBlockInfo
  Hedgehog.tripping event Aeson.encode Aeson.eitherDecode

-- | Integration test for block info using cardano-node-emulator
endToEndBlockInfo :: Hedgehog.Property
endToEndBlockInfo = Hedgehog.withShrinks 0 $
  Hedgehog.propertyOnce $
    (liftIO Helpers.setDarwinTmpdir >>) $
      Hedgehog.runFinallies $
        Hedgehog.workspace "." $ \tempPath -> do
          -- Setup
          (trace, _) <- liftIO $ defaultStdOutLogger "endToEndBlockInfo"
          let marconiTrace = mkMarconiTrace trace

          -- Local node config and connect info, with slots of length 100ms
          (nscConfig, _) <- Hedgehog.evalIO $ Integration.mkLocalNodeInfo tempPath 100

          -- Indexer preprocessor and configuration
          let
            -- No rollbacks so this is arbitrary
            securityParam = 1
            startingPoint = C.ChainPointAtGenesis
            retryConfig = RetryConfig 30 (Just 120)
            -- Same as for Marconi.ChainIndex.Run
            catchupConfig = Core.mkCatchupConfig 5_000 100

            config =
              Runner.RunIndexerConfig
                marconiTrace
                Runner.withDistancePreprocessor
                retryConfig
                securityParam
                (Integration.nscNetworkId nscConfig)
                startingPoint
                (Integration.nscSocketPath nscConfig)

          StandardWorker mindexer worker <-
            Hedgehog.evalIO $
              either throwIO pure =<< runExceptT (blockInfoBuilder securityParam catchupConfig trace tempPath)
          coordinator <- Hedgehog.evalIO $ Core.mkCoordinator [worker]

          -- Start the testnet and indexer, and run the query.

          {- NOTE: PLT-8098
           startTestnet returns immediately but runIndexer runs indefinitely, hence the use of
           race and leftFail below. startTestnet does not shutdown when the test is done.
           See Cardano.Node.Socket.Emulator.Server.runServerNode.
           As a temporary measure to avoid polluting the test output, Integration.startTestnet squashes all
           SlotAdd log messages.
           -}
          res <- Hedgehog.evalIO $
            Async.race (Integration.startTestnet nscConfig >> Runner.runIndexer config coordinator) $
              do
                threadDelay 5_000_000

                indexer <- readMVar mindexer

                (queryEvents :: [Core.Timed C.ChainPoint BlockInfo]) <-
                  either throwIO pure
                    =<< runExceptT (Core.queryLatest Core.allEvents indexer)

                pure $ not (null queryEvents)

          assertion <- Hedgehog.leftFail res
          Hedgehog.assert assertion

-- | Generate a list of events from a mock chain
getBlockInfoEvents
  :: Gen.MockchainWithInfo era
  -> [Core.Timed C.ChainPoint (Maybe BlockInfo.BlockInfo)]
getBlockInfoEvents =
  let getChainPoint :: Gen.BlockHeader -> C.ChainPoint
      getChainPoint (Gen.BlockHeader slotNo blockHeaderHash _blockNo) =
        C.ChainPoint slotNo blockHeaderHash

      getBlockInfo :: C.BlockNo -> Gen.MockBlockWithInfo era -> BlockInfo.BlockInfo
      getBlockInfo bno (Gen.MockBlockWithInfo _bh epochNo timestamp _tip _txs) =
        let timestampAsWord = fst $ properFraction $ Time.nominalDiffTimeToSeconds timestamp
         in BlockInfo.BlockInfo bno timestampAsWord epochNo

      getBlockInfoEvent
        :: C.BlockNo
        -> Gen.MockBlockWithInfo era
        -> Core.Timed C.ChainPoint (Maybe BlockInfo.BlockInfo)
      getBlockInfoEvent bno block =
        Core.Timed (getChainPoint $ Gen.mockBlockWithInfoChainPoint block)
          . pure
          $ getBlockInfo bno block
   in zipWith getBlockInfoEvent [0 ..]

genBlockInfo :: Hedgehog.Gen BlockInfo
genBlockInfo = do
  BlockInfo
    <$> CGen.genBlockNo
    <*> (fromIntegral <$> Hedgehog.Gen.word (Hedgehog.Range.constant 10_000 10_000_000))
    <*> CGen.genEpochNo
