-- | Helpers and generators for testing indexers of @Marconi.Cardano.Indexers.Datum@.
module Test.Gen.Marconi.Cardano.Indexers.Datum where

import Cardano.Api qualified as C
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Hedgehog qualified
import Marconi.Cardano.Indexers.Datum qualified as Datum
import Marconi.Core qualified as Core
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Mockchain

-- | Generate events for indexing with @Datum.'DatumIndexer'@.
genDatumInfoEvents :: Hedgehog.Gen [Core.Timed C.ChainPoint (Maybe (NonEmpty Datum.DatumInfo))]
genDatumInfoEvents = getDatumsEvents <$> Mockchain.genMockchain

-- | Generate a list of events from a mock chain
getDatumsEvents
  :: Mockchain.Mockchain era
  -> [Core.Timed C.ChainPoint (Maybe (NonEmpty Datum.DatumInfo))]
getDatumsEvents =
  let getBlockDatumsEvent
        :: Mockchain.MockBlock era
        -> Core.Timed C.ChainPoint (Maybe (NonEmpty Datum.DatumInfo))
      getBlockDatumsEvent (Mockchain.MockBlock (C.BlockHeader slotNo bhh _) txs) =
        Core.Timed (C.ChainPoint slotNo bhh) $
          nonEmpty $
            concatMap (Datum.getDataFromTxBody . C.getTxBody) txs
   in fmap getBlockDatumsEvent

genDatumInfo :: Hedgehog.Gen Datum.DatumInfo
genDatumInfo =
  Datum.DatumInfo
    <$> CGen.genHashScriptData
    <*> (C.getScriptData <$> CGen.genHashableScriptData)
