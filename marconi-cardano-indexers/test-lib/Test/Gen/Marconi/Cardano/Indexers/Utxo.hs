-- | Generators and utilities for testing the 'UtxoIndexer'.
module Test.Gen.Marconi.Cardano.Indexers.Utxo where

import Cardano.Api qualified as C
import Control.Lens (folded, (^.), (^..))
import Control.Monad (join)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Hedgehog (Gen)
import Hedgehog.Gen qualified
import Hedgehog.Range qualified
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance (WithDistance))
import Marconi.Cardano.Core.Types qualified as Cardano.Core
import Marconi.Cardano.Indexers.Utxo qualified as Utxo
import Marconi.Core qualified as Core
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Mockchain

-- | Generate a list of timed events for indexing with the @Utxo.'UtxoIndexer'@.
genTimedUtxosEvents :: Gen [Core.Timed C.ChainPoint (Maybe Utxo.UtxoEvent)]
genTimedUtxosEvents = getTimedUtxosEvents <$> Mockchain.genMockchain

{- | Generate a list of timed events with a distance in blocks to the chain tip. For example,
can be used as a base to generate events for indexing directly with
@Marconi.Cardano.Core.Indexer.Worker.'StandardIndexer'@.
-}
genTimedUtxosEventsWithDistance
  :: Gen [Core.Timed C.ChainPoint (WithDistance (Maybe Utxo.UtxoEvent))]
genTimedUtxosEventsWithDistance =
  getTimedUtxosEventsWithDistance . Mockchain.mockchainWithInfoAsMockchainWithDistance
    <$> Mockchain.genMockchainWithInfoAndDistance

-- | Generate a random 'Utxo'
genUtxo :: Hedgehog.Gen Utxo.Utxo
genUtxo = do
  script <- CGen.genScriptInAnyLang
  let genAddressAny :: Hedgehog.Gen C.AddressAny
      genAddressAny = do
        (C.AddressInEra _ addr) <- CGen.genAddressInEra C.BabbageEra
        pure $ C.toAddressAny addr
      hashScriptInAnyLang :: C.ScriptInAnyLang -> C.ScriptHash
      hashScriptInAnyLang (C.ScriptInAnyLang _ s) = C.hashScript s
      scriptHash :: C.ScriptHash
      scriptHash = hashScriptInAnyLang script
  Utxo.Utxo
    <$> genAddressAny -- address
    <*> (Cardano.Core.TxIndexInBlock <$> Hedgehog.Gen.integral_ (Hedgehog.Range.constant 0 100)) -- txIndex
    <*> CGen.genTxIn -- txIn
    <*> Hedgehog.Gen.maybe CGen.genHashScriptData -- datumHash
    <*> CGen.genValue CGen.genAssetId (CGen.genQuantity $ Hedgehog.Range.constant 0 100) -- value
    <*> Hedgehog.Gen.maybe (pure script) -- inlineScript
    <*> Hedgehog.Gen.maybe (pure scriptHash) -- inlineScriptHash

{- HELPERS -}

-- | Generate a list of @Utxo.UtxoEvent@ from a @Mockchain.'Mockchain'@.
getTimedUtxosEvents
  :: (C.IsCardanoEra era)
  => Mockchain.Mockchain era
  -> [Core.Timed C.ChainPoint (Maybe Utxo.UtxoEvent)]
getTimedUtxosEvents =
  let getBlockTimedUtxosEvent block =
        Core.Timed
          (Mockchain.getChainPointFromBlockHeader . Mockchain.mockBlockHeader $ block)
          $ getBlockUtxosEvent block
   in fmap getBlockTimedUtxosEvent

-- | Generate a list of @Utxo@ events with distance from a mock chain with distance.
getTimedUtxosEventsWithDistance
  :: (C.IsCardanoEra era)
  => Mockchain.MockchainWithDistance era
  -> [Core.Timed C.ChainPoint (WithDistance (Maybe Utxo.UtxoEvent))]
getTimedUtxosEventsWithDistance =
  let getBlockTimedUtxosEvent block = Core.Timed (getChainPoint block) $ fmap getBlockUtxosEvent block
      getChainPoint (WithDistance _ block) =
        Mockchain.getChainPointFromBlockHeader . Mockchain.mockBlockHeader $ block
   in map getBlockTimedUtxosEvent

getBlockUtxosEvent :: (C.IsCardanoEra era) => Mockchain.MockBlock era -> Maybe (NonEmpty Utxo.Utxo)
getBlockUtxosEvent (Mockchain.MockBlock _ txs) =
  nonEmpty $ join $ zipWith Utxo.getUtxosFromTx [0 ..] txs

-- | Get the addresses from a timed 'UtxoEvent' with distance.
addressesFromTimedUtxoEvent
  :: Core.Timed C.ChainPoint (WithDistance (Maybe Utxo.UtxoEvent)) -> [C.AddressAny]
addressesFromTimedUtxoEvent = List.nub . getAddrs
  where
    getAddrs :: Core.Timed C.ChainPoint (WithDistance (Maybe Utxo.UtxoEvent)) -> [C.AddressAny]
    getAddrs e = e ^. Core.event ^.. folded . folded . folded . Utxo.address
