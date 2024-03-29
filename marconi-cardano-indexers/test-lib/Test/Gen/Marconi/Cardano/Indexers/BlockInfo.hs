{-# LANGUAGE NumericUnderscores #-}

-- | Generators and helpers for testing @Marconi.Cardano.Indexers.BlockInfo@.
module Test.Gen.Marconi.Cardano.Indexers.BlockInfo where

import Cardano.Api qualified as C
import Data.Time qualified as Time
import Hedgehog qualified
import Hedgehog.Gen qualified
import Hedgehog.Range qualified
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance (WithDistance))
import Marconi.Cardano.Indexers.BlockInfo qualified as BlockInfo
import Marconi.Core qualified as Core
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Mockchain
import Test.Gen.Marconi.Cardano.Core.Types qualified as Test.Core

-- | Generate events for indexing with @BlockInfo.'BlockInfoIndexer'@.
genBlockInfoEvents :: Hedgehog.Gen [Core.Timed C.ChainPoint (Maybe BlockInfo.BlockInfo)]
genBlockInfoEvents = getTimedBlockInfoEvents <$> Mockchain.genMockchainWithInfo

-- | Generate a list of events from a mock chain
getTimedBlockInfoEvents
  :: Mockchain.MockchainWithInfo era
  -> [Core.Timed C.ChainPoint (Maybe BlockInfo.BlockInfo)]
getTimedBlockInfoEvents = map getBlockInfoEvent

{- | Generate a list of @BlockInfo.'BlockInfo'@ events from a @Mockchain.'Mockchain'@.
 - This version is used in some tests and manually fixes the block numbers by index of
 - the provided chain, rather than using those provided in the chain.
-}
getTimedBlockInfoEventsFixedBlockNos
  :: Mockchain.MockchainWithInfo era
  -> [Core.Timed C.ChainPoint (Maybe BlockInfo.BlockInfo)]
getTimedBlockInfoEventsFixedBlockNos =
  let getChainPoint :: C.BlockHeader -> C.ChainPoint
      getChainPoint (C.BlockHeader slotNo blockHeaderHash _blockNo) =
        C.ChainPoint slotNo blockHeaderHash

      getBlockInfoFixedBlockNo :: C.BlockNo -> Mockchain.MockBlockWithInfo era -> BlockInfo.BlockInfo
      getBlockInfoFixedBlockNo bno (Mockchain.MockBlockWithInfo _bh epochNo timestamp _tip _txs) =
        let timestampAsWord = fst $ properFraction $ Time.nominalDiffTimeToSeconds timestamp
         in BlockInfo.BlockInfo bno timestampAsWord epochNo

      getBlockInfoEventFixedBlockNo
        :: C.BlockNo
        -> Mockchain.MockBlockWithInfo era
        -> Core.Timed C.ChainPoint (Maybe BlockInfo.BlockInfo)
      getBlockInfoEventFixedBlockNo bno block =
        Core.Timed (getChainPoint $ Mockchain.mockBlockWithInfoChainPoint block)
          . pure
          $ getBlockInfoFixedBlockNo bno block
   in zipWith getBlockInfoEventFixedBlockNo [0 ..]

-- | Generate a list of events from a mock chain with info and distance.
getTimedBlockInfoEventsWithInfoAndDistance
  :: Mockchain.MockchainWithInfoAndDistance era
  -> [Core.Timed C.ChainPoint (WithDistance (Maybe BlockInfo.BlockInfo))]
getTimedBlockInfoEventsWithInfoAndDistance = map op
  where
    op (WithDistance d block) = WithDistance d <$> getBlockInfoEvent block

getBlockInfo :: Mockchain.MockBlockWithInfo era -> BlockInfo.BlockInfo
getBlockInfo (Mockchain.MockBlockWithInfo bh epochNo timestamp _tip _txs) =
  let timestampAsWord = fst $ properFraction $ Time.nominalDiffTimeToSeconds timestamp
   in BlockInfo.BlockInfo (Mockchain.getBlockNoFromBlockHeader bh) timestampAsWord epochNo

getBlockInfoEvent
  :: Mockchain.MockBlockWithInfo era
  -> Core.Timed C.ChainPoint (Maybe BlockInfo.BlockInfo)
getBlockInfoEvent block =
  Core.Timed (Mockchain.getChainPointFromBlockHeader $ Mockchain.mockBlockWithInfoChainPoint block)
    . pure
    $ getBlockInfo block

genBlockInfo :: Hedgehog.Gen BlockInfo.BlockInfo
genBlockInfo = do
  BlockInfo.BlockInfo
    <$> Test.Core.genBlockNo
    <*> (fromIntegral <$> Hedgehog.Gen.word (Hedgehog.Range.constant 10_000 10_000_000))
    <*> CGen.genEpochNo
