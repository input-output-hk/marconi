{- | Tests of queries and handlers in PastAddressUtxo. For tests of output JSON shapes,
see Spec.Marconi.Sidechain.Experimental.Routes.
-}
module Spec.Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo where

import Control.Lens ((^.))
import Hedgehog qualified
import Hedgehog.Gen qualified
import Marconi.Cardano.Indexers qualified as Indexers
import Marconi.ChainIndex.Api.Types qualified as ChainIndex
import Marconi.Sidechain.Experimental.Api.Types qualified as Sidechain
import Marconi.Sidechain.Experimental.CLI qualified as CLI
import Marconi.Sidechain.Experimental.Env qualified as Sidechain
import Spec.Marconi.Sidechain.Experimental.Utils qualified as Utils
import Test.Gen.Marconi.Cardano.Indexers.Utxo qualified as Test.Utxo
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Marconi.Sidechain.Experimental.Api.JsonRpc.Endpoint.PastAddressUtxo"
    []

-- TODO: PLT-8634 change test name to be more descriptive
propQueryTargetAddresses :: Hedgehog.Property
propQueryTargetAddresses = Hedgehog.property $ do
  events <- Hedgehog.forAll Test.Utxo.genTimedUtxosEvents

  -- Subset of the unique addresses generated to query
  targetAddresses <-
    Hedgehog.forAll $
      Utils.addressAnysToTargetAddresses . Utils.addressesFromTimedUtxoEvents
        <$> Hedgehog.Gen.subsequence events

  let args = Utils.initTestingCliArgs{CLI.targetAddresses = targetAddresses}

  -- Make the SidechainEnv (with indexers) just as you would with the sidechain app.
  env <- Utils.mkTestSidechainEnvFromCliArgs args

  -- Index the utxo events directly

  let indexer =
        env
          ^. Sidechain.sidechainHttpServerConfig
            . Sidechain.chainIndexHttpServerConfig
            . ChainIndex.configQueryables
            . Indexers.queryableUtxo

  -- TODO: PLT-8634 sample addresses, then populate cli args, then create indexer
  -- and index events, then
  -- TODO: PLT-8634 boilerplate from Utxo tests in marconi-cardano-indexers. Revise as needed.
  -- let utxoEvents = getTimedUtxosEvents events
  --    timedEvents = fmap (\evt -> Core.Timed (extractChainPoint evt) evt) events
  --    chainAddresses = utxoEvents ^.. traverse . Core.event . traverse . traverse . Utxo.address
  --    attachDistance dist = Just . WithDistance dist
  --    eventsWithDistance
  --      :: [Core.Timed C.ChainPoint (Maybe (WithDistance (Gen.MockBlock C.BabbageEra)))]
  --    eventsWithDistance = zipWith (fmap . attachDistance) [0 ..] $ reverse timedEvents
  --    resultAtAddress addr = NonEmpty.nonEmpty . NonEmpty.filter ((== addr) . Lens.view Utxo.address)
  --    eventAtAddress
  --      :: C.AddressAny
  --      -> [Core.Timed C.ChainPoint (Maybe (NonEmpty Utxo.Utxo))]
  --      -> [Core.Timed C.ChainPoint (NonEmpty Utxo.Utxo)]
  --    eventAtAddress addr =
  --      mapMaybe (traverse $ resultAtAddress addr)
  --        . mapMaybe sequence

  undefined
