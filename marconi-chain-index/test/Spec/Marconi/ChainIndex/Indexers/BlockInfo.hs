{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Marconi.ChainIndex.Indexers.BlockInfo (
  tests,
  getBlockInfoEvents,
  genBlockInfo,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent (BlockEvent))
import Control.Concurrent (readMVar, threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Maybe (mapMaybe)
import Data.Time qualified as Time
import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Extras.Test.Base qualified as Hedgehog
import Hedgehog.Gen qualified
import Hedgehog.Range qualified
import Marconi.ChainIndex.Extract.WithDistance (WithDistance (WithDistance))
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

tests :: TestTree
tests =
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
    , testGroup
        "End-to-end indexer tests with cardano-node-emulator"
        [ testPropertyNamed
            -- TODO: PLT-8098
            "Indexing a testnet and then submitting a transaction has the indexer receive a block"
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
          -- Local blockchain
          -- Start the node emulator
          (localNodeConnectInfo, networkId, socketPath) <- Integration.startTestnet tempPath
          ledgerPP <- Helpers.getLedgerProtocolParams @C.BabbageEra localNodeConnectInfo

          -- Transaction-builder inputs
          txMintValue <- Hedgehog.forAll Integration.genTxMintValue
          address <- Hedgehog.nothingFail Integration.knownShelleyAddress
          witnessSigningKey <- Hedgehog.nothingFail Integration.knownWitnessSigningKey
          (txIns, lovelace) <- Helpers.getAddressTxInsValue @C.BabbageEra localNodeConnectInfo address
          let validityRange = Integration.unboundedValidityRange

          -- TODO: PLT-8098 make a note about the fee calcs and TxOut stuff here being simplified
          -- Create "unbalanced" transaction
          let txbody =
                Integration.mkUnbalancedTxBodyContentFromTxMintValue
                  validityRange
                  ledgerPP
                  address
                  txIns
                  txMintValue

          Integration.validateAndSubmitTx
            localNodeConnectInfo
            ledgerPP
            networkId
            address
            witnessSigningKey
            txbody
            lovelace

          -- Indexer

          -- Setup
          -- TODO: PLT-8098 make a endToEndIndexerRunner or something in the test-lib
          (trace, _) <- liftIO $ defaultStdOutLogger "endToEndBlockInfo"
          let marconiTrace = mkMarconiTrace trace

          let
            toBlockInfo :: BlockEvent -> BlockInfo
            toBlockInfo (BlockEvent (C.BlockInMode (C.Block (C.BlockHeader _ _ bn) _) _) _ t) =
              -- TODO: PLT-8098 check whether it is inteded to convert posix to seconds here
              -- TODO: PLT-8098 get epochno from slotno
              BlockInfo bn (fst . properFraction $ Time.nominalDiffTimeToSeconds t) 0

            blockInfoPreprocessor :: Runner.RunIndexerEventPreprocessing BlockInfo
            blockInfoPreprocessor =
              let eventToProcessedInput = Runner.withNoPreprocessor ^. Runner.runIndexerPreprocessEvent
               in Runner.RunIndexerEventPreprocessing
                    (map (fmap toBlockInfo) . eventToProcessedInput)
                    (Just . (^. BlockInfo.blockNo))
                    (const Nothing)

            -- No rollbacks so this is arbitrary
            securityParam = 1
            startingPoint = C.ChainPointAtGenesis
            retryConfig = RetryConfig 30 (Just 120)
            -- Same as for Marconi.ChainIndex.Run
            catchupConfig = Core.mkCatchupConfig 5_000 100

          let config =
                Runner.RunIndexerConfig
                  marconiTrace
                  Runner.withDistancePreprocessor
                  -- TODO: PLT-8098
                  -- blockInfoPreprocessor
                  retryConfig
                  securityParam
                  networkId
                  startingPoint
                  socketPath

          -- TODO: PLT-8098 delete this experiment if unused

          -- indexer <- Hedgehog.evalExceptT $ BlockInfo.mkBlockInfoIndexer ":memory:"
          ---- TODO: PLT-8098 this should not be needed and should be set to genesis
          -- startingPoint <- Core.lastStablePoint indexer

          StandardWorker mvar worker <-
            Hedgehog.evalExceptT $ blockInfoBuilder securityParam catchupConfig trace tempPath

          coordinator <- liftIO $ Core.mkCoordinator [worker]

          liftIO $ threadDelay 10_000_000
          _ <- liftIO $ Async.async (Runner.runIndexer config coordinator)

          indexer <- liftIO $ readMVar mvar

          liftIO $ threadDelay 30_000_000
          (queryEvents :: [Core.Timed C.ChainPoint BlockInfo]) <-
            Hedgehog.evalExceptT $ Core.queryLatest Core.allEvents indexer

          -- (queryEvents :: [Core.Timed C.ChainPoint BlockInfo]) <-
          --  Hedgehog.evalExceptT $ Core.queryLatest Core.allEvents indexer

          -- Query
          -- (queryEvent :: Core.Timed C.ChainPoint BlockInfo) <-
          --  Integration.queryFirstResultWithRetry
          --    5
          --    10_000_000
          --    (Hedgehog.evalExceptT $ Core.queryLatest Core.allEvents indexer)

          -- TODO: PLT-8098 revise queryFirstResultWithRetry to return maybe, where
          -- nothing says you didn't get anything
          Hedgehog.assert $ not (null queryEvents)

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
    <*> (fromIntegral <$> Hedgehog.Gen.word (Hedgehog.Range.constant 10000 10000000))
    <*> CGen.genEpochNo
