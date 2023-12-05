-- | Helpers and generators for testing @Marconi.Cardano.Indexers.Spent@.
module Test.Gen.Marconi.Cardano.Indexers.Spent where

import Cardano.Api qualified as C
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Hedgehog qualified
import Marconi.Cardano.Indexers.Spent qualified as Spent
import Marconi.Core qualified as Core
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Mockchain

-- | Generate events for indexing with @Spent.'SpentIndexer'@.
genSpentInfoEvents :: Hedgehog.Gen [Core.Timed C.ChainPoint (Maybe (NonEmpty Spent.SpentInfo))]
genSpentInfoEvents = getTimedSpentsEvents <$> Mockchain.genMockchain

-- | Generate a list of events from a mock chain
getTimedSpentsEvents
  :: Mockchain.Mockchain era
  -> [Core.Timed C.ChainPoint (Maybe (NonEmpty Spent.SpentInfo))]
getTimedSpentsEvents =
  let getTxBody :: C.Tx era -> C.TxBody era
      getTxBody (C.Tx txBody _) = txBody

      getBlockSpentsEvent
        :: Mockchain.MockBlock era
        -> Core.Timed C.ChainPoint (Maybe (NonEmpty Spent.SpentInfo))
      getBlockSpentsEvent (Mockchain.MockBlock (C.BlockHeader slotNo blockHeaderHash _) txs) =
        Core.Timed (C.ChainPoint slotNo blockHeaderHash) $
          nonEmpty $
            concatMap (Spent.getInputs . getTxBody) txs
   in fmap getBlockSpentsEvent

genSpent :: Hedgehog.Gen Spent.SpentInfo
genSpent = Spent.SpentInfo <$> CGen.genTxIn <*> CGen.genTxId
