{- | Tests of queries and handlers in PastAddressUtxo. For tests of output JSON shapes,
see Spec.Marconi.Sidechain.Experimental.Routes.
-}
module Spec.Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo where

import Cardano.Api qualified as C
import Control.Concurrent (withMVar)
import Control.Exception (throwIO)
import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import Hedgehog qualified
import Hedgehog.Gen qualified
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance (WithDistance))
import Marconi.Cardano.Indexers qualified as Indexers
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.Types qualified as ChainIndex
import Marconi.ChainIndex.Api.Types qualified as ChainIndex
import Marconi.Core qualified as Core
import Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo qualified as Sidechain
import Marconi.Sidechain.Experimental.Api.Types qualified as Sidechain
import Marconi.Sidechain.Experimental.CLI qualified as CLI
import Marconi.Sidechain.Experimental.Env qualified as Sidechain
import Spec.Marconi.Sidechain.Experimental.Utils qualified as Utils
import Test.Gen.Marconi.Cardano.Indexers qualified as Test.Indexers
import Test.Gen.Marconi.Cardano.Indexers.Utxo qualified as Test.Utxo
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Hedgehog (
  HedgehogShrinkLimit (HedgehogShrinkLimit),
  HedgehogTestLimit (HedgehogTestLimit),
  testPropertyNamed,
 )

-- TODO: PLT8634 remove the option setting

tests :: TestTree
tests =
  -- TODO: PLT-8634 investigating issue with open files
  localOption (HedgehogTestLimit $ Just 1) $
    localOption (HedgehogShrinkLimit $ Just 1) $
      testGroup
        "Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo"
        [ testPropertyNamed
            "Handler returns Utxos from queried address"
            "propQueryTargetAddresses"
            propQueryTargetAddresses
        ]

-- TODO: PLT-8634 change test name to be more descriptive
propQueryTargetAddresses :: Hedgehog.Property
propQueryTargetAddresses = Hedgehog.property $ do
  events <- Hedgehog.forAll Test.Utxo.genTimedUtxosEventsWithDistance

  -- Select a single address from the sampled events
  addr <-
    Hedgehog.forAll $ do
      addrs <- Utils.addressesFromTimedUtxoEvent <$> Hedgehog.Gen.element events
      Hedgehog.Gen.element addrs

  let
    args = Utils.initTestingCliArgs{CLI.targetAddresses = Utils.addressAnysToTargetAddresses [addr]}
    -- Serialise to Bech32-format string as required by handler
    addrString = Text.unpack $ C.serialiseAddress addr

  -- Make the http and build indexers configs (with indexers exposed)
  -- just as you would with the sidechain app.
  (httpConfig, indexersConfig) <- Utils.mkTestSidechainConfigsFromCliArgs args

  -- Index the utxo events directly
  _ <-
    Hedgehog.evalIO $
      withMVar
        (indexersConfig ^. Test.Indexers.testBuildIndexersResultUtxo)
        -- TODO: PLT-8634 seems weird to have Maybe (WithDistance (Maybe event)).
        -- The first is required by indexAllDescending, the second by StandardIndexer.
        (\idx -> runExceptT $ Core.indexAllDescending (map (fmap Just) events) idx)
        >>= either throwIO pure

  -- TODO: PLT-8634 add coverage
  let
    params = ChainIndex.GetUtxosFromAddressParams addrString Nothing Nothing

  res <-
    Hedgehog.evalIO $
      flip runReaderT httpConfig . runExceptT $
        Sidechain.getPastAddressUtxoHandler params

  actual <- Hedgehog.evalIO $ either throwIO pure res >>= either (fail . show) pure

  let
    expected = mapMaybe ((\(WithDistance _ x) -> x) . (^. Core.event)) events

  Hedgehog.assert $ Utils.compareGetUtxosFromAddressResult addr expected actual
