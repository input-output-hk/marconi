{-# LANGUAGE NumericUnderscores #-}

-- | Generators and helpers for testing @Marconi.Cardano.Indexers.BlockInfo@.
module Test.Gen.Marconi.Cardano.Indexers.BlockInfo where

import Cardano.Api qualified as C
import Data.Time qualified as Time
import Hedgehog qualified
import Hedgehog.Gen qualified
import Hedgehog.Range qualified
import Marconi.Cardano.Indexers.BlockInfo qualified as BlockInfo
import Marconi.Core qualified as Core
import Test.Gen.Cardano.Api.Typed qualified as CGen
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Mockchain
import Test.Gen.Marconi.Cardano.Core.Types qualified as Test.Core

-- | Generate events for indexing with @BlockInfo.'BlockInfoIndexer'@.
genBlockInfoEvents :: Hedgehog.Gen [Core.Timed C.ChainPoint (Maybe BlockInfo.BlockInfo)]
genBlockInfoEvents = getBlockInfoEvents <$> Mockchain.genMockchainWithInfo

-- | Generate a list of events from a mock chain
getBlockInfoEvents
  :: Mockchain.MockchainWithInfo C.BabbageEra
  -> [Core.Timed C.ChainPoint (Maybe BlockInfo.BlockInfo)]
getBlockInfoEvents =
  let getChainPoint :: Mockchain.BlockHeader -> C.ChainPoint
      getChainPoint (Mockchain.BlockHeader slotNo blockHeaderHash _blockNo) =
        C.ChainPoint slotNo blockHeaderHash

      getBlockInfo :: C.BlockNo -> Mockchain.MockBlockWithInfo era -> BlockInfo.BlockInfo
      getBlockInfo bno (Mockchain.MockBlockWithInfo _bh epochNo timestamp _tip _txs) =
        let timestampAsWord = fst $ properFraction $ Time.nominalDiffTimeToSeconds timestamp
         in BlockInfo.BlockInfo bno timestampAsWord epochNo

      getBlockInfoEvent
        :: C.BlockNo
        -> Mockchain.MockBlockWithInfo era
        -> Core.Timed C.ChainPoint (Maybe BlockInfo.BlockInfo)
      getBlockInfoEvent bno block =
        Core.Timed (getChainPoint $ Mockchain.mockBlockWithInfoChainPoint block)
          . pure
          $ getBlockInfo bno block
   in zipWith getBlockInfoEvent [0 ..]

genBlockInfo :: Hedgehog.Gen BlockInfo.BlockInfo
genBlockInfo = do
  BlockInfo.BlockInfo
    <$> Test.Core.genBlockNo
    <*> (fromIntegral <$> Hedgehog.Gen.word (Hedgehog.Range.constant 10_000 10_000_000))
    <*> CGen.genEpochNo
