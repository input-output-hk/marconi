module Marconi.ChainIndex.Extract.MintTokenEvent where

import Marconi.ChainIndex.Indexers.MintTokenEvent (
  mintTokenEventBlockNo,
  mintTokenEventLocation,
  mintTokenEvents,
 )
import Marconi.ChainIndex.Indexers.MintTokenEvent qualified as MintTokenEvent

-- TODO: PLT-8203 likely will end up with circular dependency, since you will want to
-- import the extract functions throughout. this is just a spike so do the minimum to avoid that.

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent)
import Cardano.Api.Extended.Streaming.ChainSyncEvent (ChainSyncEvent)
import Control.Lens (firstOf, folded, (^.))
import Data.List.NonEmpty qualified as NonEmpty
import Marconi.ChainIndex.Extract.AnyTxBody qualified as AnyTxBody
import Marconi.ChainIndex.Extract.WithDistance qualified as WithDistance
import Marconi.ChainIndex.Indexers (AnyTxBody (AnyTxBody))
import Marconi.ChainIndex.Runner qualified as Runner
import Marconi.Core qualified as Core

mintTokenEventsFromAnyTxBody :: AnyTxBody -> [MintTokenEvent.MintTokenEvent]
mintTokenEventsFromAnyTxBody (AnyTxBody bn ix txb) = MintTokenEvent.extractEventsFromTx bn ix txb

mintTokenBlockEventsFromAnyTxBody
  :: AnyTxBody -> Maybe MintTokenEvent.MintTokenBlockEvents
mintTokenBlockEventsFromAnyTxBody =
  fmap MintTokenEvent.MintTokenBlockEvents
    . NonEmpty.nonEmpty
    . mintTokenEventsFromAnyTxBody

-- TODO: PLT-8203 should put this utility elsewhere. fmap instance doesn't seem to do this, though
-- try join.
mapEventInProcessedInput
  :: (event -> Maybe event') -> Core.ProcessedInput point event -> Core.ProcessedInput point event'
mapEventInProcessedInput f (Core.Index x) = Core.Index ((>>= f) <$> x)
mapEventInProcessedInput f (Core.IndexAllDescending x) = Core.IndexAllDescending $ fmap ((>>= f) <$>) x
mapEventInProcessedInput _ (Core.Rollback t) = Core.Rollback t
mapEventInProcessedInput _ (Core.StableAt t) = Core.StableAt t
mapEventInProcessedInput _ Core.Stop = Core.Stop

extractMintTokenBlockEvents
  :: ChainSyncEvent BlockEvent
  -> [Core.ProcessedInput C.ChainPoint MintTokenEvent.MintTokenBlockEvents]
extractMintTokenBlockEvents = map (mapEventInProcessedInput mintTokenBlockEventsFromAnyTxBody) . AnyTxBody.extractAnyTxBodys

-- NOTE: PLT-8203 This assumes the block number is the same within a MintTokenBlockEvents, which is
-- guaranteed here by the call to MintTokenEvent.extractEventsFromTx.
blockNoFromMintTokenBlockEvents :: MintTokenEvent.MintTokenBlockEvents -> Maybe C.BlockNo
blockNoFromMintTokenBlockEvents = firstOf (mintTokenEvents . folded . mintTokenEventLocation . mintTokenEventBlockNo)

mintTokenBlockEventsPreprocessor
  :: Runner.RunIndexerEventPreprocessing MintTokenEvent.MintTokenBlockEvents
mintTokenBlockEventsPreprocessor =
  Runner.RunIndexerEventPreprocessing
    extractMintTokenBlockEvents
    blockNoFromMintTokenBlockEvents
    (const Nothing)
