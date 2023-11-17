module Marconi.ChainIndex.Extract.AnyTxBody where

-- TODO: PLT-8203 again, we'll have cyclical dependencies here
-- so in reality some restructing is needed

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent)
import Cardano.Api.Extended.Streaming.ChainSyncEvent (ChainSyncEvent (RollBackward, RollForward))
import Data.Maybe (listToMaybe)
import Marconi.ChainIndex.Extract.WithDistance qualified as WithDistance
import Marconi.ChainIndex.Indexers (AnyTxBody (AnyTxBody))
import Marconi.ChainIndex.Runner qualified as Runner
import Marconi.ChainIndex.Types qualified as Types
import Marconi.Core qualified as Core

blockEventToAnyTxBodys
  :: BlockEvent
  -> [Core.Timed C.ChainPoint AnyTxBody]
blockEventToAnyTxBodys = toTxBodys
  where
    getTimedTxBody :: (C.IsCardanoEra era) => C.BlockNo -> Types.TxIndexInBlock -> C.Tx era -> AnyTxBody
    getTimedTxBody blockNo ix tx = AnyTxBody blockNo ix (C.getTxBody tx)
    toTxBodys :: Types.BlockEvent -> [Core.Timed C.ChainPoint AnyTxBody]
    toTxBodys (Types.BlockEvent (C.BlockInMode (C.Block (C.BlockHeader slotNo hsh bn) txs) _) _ _) =
      zipWith (\ix -> Core.Timed (C.ChainPoint slotNo hsh) . getTimedTxBody bn ix) [0 ..] txs

extractAnyTxBodys :: ChainSyncEvent BlockEvent -> [Core.ProcessedInput C.ChainPoint AnyTxBody]
extractAnyTxBodys (RollForward x _tip) = map (Core.Index . fmap Just) $ blockEventToAnyTxBodys x
extractAnyTxBodys (RollBackward x _tip) = [Core.Rollback x]

anyTxBodyPreprocessor
  :: Runner.RunIndexerEventPreprocessing AnyTxBody
anyTxBodyPreprocessor =
  Runner.RunIndexerEventPreprocessing
    extractAnyTxBodys
    (\(AnyTxBody bn _ _) -> Just bn)
    (const Nothing)
