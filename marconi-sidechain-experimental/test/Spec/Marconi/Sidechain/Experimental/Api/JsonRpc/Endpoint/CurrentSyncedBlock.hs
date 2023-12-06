{-# LANGUAGE NumericUnderscores #-}

{- | Tests of queries and handlers in queryCurrentSyncedBlock. For tests of output JSON shapes,
see Spec.Marconi.Sidechain.Experimental.Routes.
-}
module Spec.Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.CurrentSyncedBlock where

import Cardano.Api qualified as C
import Control.Concurrent (readMVar, threadDelay, withMVar)
import Control.Exception (finally, throwIO)
import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.List.NonEmpty qualified as NEList
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import Hedgehog ((===))
import Hedgehog qualified
import Hedgehog.Gen qualified
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance (WithDistance))
import Marconi.Cardano.Indexers qualified as Indexers
import Marconi.Cardano.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.CurrentSyncedBlock qualified as ChainIndex
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.Types qualified as ChainIndex
import Marconi.ChainIndex.Api.Types qualified as ChainIndex
import Marconi.Core qualified as Core
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.CurrentSyncedBlock qualified as Sidechain
import Marconi.Sidechain.Experimental.Api.Types qualified as Sidechain
import Marconi.Sidechain.Experimental.CLI qualified as CLI
import Marconi.Sidechain.Experimental.Env qualified as Sidechain
import Network.JsonRpc.Types (UnusedRequestParams (UnusedRequestParams))
import Spec.Marconi.Sidechain.Experimental.Utils qualified as Utils
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Test.Mockchain
import Test.Gen.Marconi.Cardano.Indexers qualified as Test.Indexers
import Test.Gen.Marconi.Cardano.Indexers.Utxo qualified as Test.Utxo
import Test.Gen.Marconi.Cardano.Indexers.UtxoQuery qualified as Test.UtxoQuery
import Test.Helpers qualified
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Hedgehog (
  HedgehogShrinkLimit (HedgehogShrinkLimit),
  testPropertyNamed,
 )

tests :: TestTree
tests =
  -- TODO: PLT-8634 remove shrink limit?
  localOption (HedgehogShrinkLimit $ Just 5) $
    testGroup
      "Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.queryCurrentSyncedBlock"
      [ testPropertyNamed
          -- TODO: PLT-8634 write description
          "TODO"
          "propCurrentSyncedBlock"
          propCurrentSyncedBlock
      ]

propCurrentSyncedBlock :: Hedgehog.Property
propCurrentSyncedBlock = Hedgehog.property $ Test.Helpers.workspace "." $ \tmp -> do
  -- TODO: PLT-8634
  Hedgehog.evalIO $ putStrLn "Tmp dir: " >> print tmp

  events <- Hedgehog.forAll Test.Mockchain.genMockchainWithInfoAndDistance
  let
    args =
      Utils.initTestingCliArgs
        { CLI.dbDir = tmp
        }
  --
  -- Make the http and build indexers configs (with indexers exposed)
  -- just as you would with the sidechain app.
  (httpConfig, indexersConfig) <- Utils.mkTestSidechainConfigsFromCliArgs args

  -- Index the events directly
  Hedgehog.evalIO $ Test.Indexers.indexAllWithMockchain indexersConfig events

  Hedgehog.evalIO $ threadDelay 1_000_000

  res <-
    Hedgehog.evalIO $
      flip runReaderT httpConfig . runExceptT $
        Sidechain.getCurrentSyncedBlockHandler UnusedRequestParams

  actual <- Hedgehog.evalIO $ either throwIO pure res >>= either (fail . show) pure

  let
    -- TODO: PLT-8634
    expected = ChainIndex.GetCurrentSyncedBlockResult Nothing Nothing Nothing Nothing Nothing Nothing

  actual === expected
