{-# LANGUAGE TemplateHaskell #-}

module Marconi.Sidechain.Experimental.Indexers where

import Cardano.Api qualified as C
import Cardano.BM.Trace (Trace)
import Control.Lens (makeLenses, over, (^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Marconi.ChainIndex.Indexers (Coordinator, MarconiChainIndexQueryables)
import Marconi.ChainIndex.Indexers qualified as ChainIndex.Indexers
import Marconi.ChainIndex.Indexers.EpochState qualified as EpochState
import Marconi.ChainIndex.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Runner (RunIndexerConfig, TipOrBlock, runIndexerConfigChainPoint)
import Marconi.ChainIndex.Runner qualified as ChainIndex.Runner
import Marconi.ChainIndex.Types (SecurityParam)
import Marconi.Core (CatchupConfig, IndexerError)

{- TYPE -}

{- | Configuration for constructing and running marconi indexers as used
in this package.
-}
data SidechainIndexersConfig = SidechainIndexersConfig
  { _sidechainIndexersTrace :: !(Trace IO Text)
  , _sidechainIndexersSecurityParam :: !SecurityParam
  , _sidechainIndexersRunIndexerConfig :: !(RunIndexerConfig TipOrBlock)
  , _sidechainIndexersCatchupConfig :: !CatchupConfig
  , _sidechainIndexersDbPath :: !FilePath
  , _sidechainIndexersEpochStateConfig :: !EpochState.EpochStateWorkerConfig
  , _sidechainIndexersMintTokenEventConfig :: !MintTokenEvent.MintTokenEventConfig
  , _sidechainIndexersUtxoConfig :: !Utxo.UtxoIndexerConfig
  }

makeLenses ''SidechainIndexersConfig

{- INDEXER BUILDERS AND RUNNERS -}

{- | Build the sidechain indexer workers and coordinator,
similarly to the marconi-chain-index application.
-}
sidechainBuildIndexers
  :: (MonadIO m)
  => SidechainIndexersConfig
  -> m (Either IndexerError (C.ChainPoint, MarconiChainIndexQueryables, Coordinator))
sidechainBuildIndexers config =
  liftIO . runExceptT $
    ChainIndex.Indexers.buildIndexers
      (config ^. sidechainIndexersSecurityParam)
      (config ^. sidechainIndexersCatchupConfig)
      (config ^. sidechainIndexersUtxoConfig)
      (config ^. sidechainIndexersMintTokenEventConfig)
      (config ^. sidechainIndexersEpochStateConfig)
      (config ^. sidechainIndexersTrace)
      (config ^. sidechainIndexersDbPath)

runSidechainIndexers
  :: (MonadIO m)
  => SidechainIndexersConfig
  -> C.ChainPoint
  -- ^ Indexer's last stable point, used to update the preferred starting point
  -- in 'RunIndexerConfig' as needed.
  -> ChainIndex.Indexers.Coordinator
  -> m ()
runSidechainIndexers config indexerLastStablePoint =
  liftIO
    . ChainIndex.Runner.runIndexer (updateRunnerConfig config ^. sidechainIndexersRunIndexerConfig)
  where
    updateRunnerConfig =
      over
        (sidechainIndexersRunIndexerConfig . runIndexerConfigChainPoint)
        (updateStartingPoint indexerLastStablePoint)
    updateStartingPoint :: C.ChainPoint -> C.ChainPoint -> C.ChainPoint
    updateStartingPoint stablePoint C.ChainPointAtGenesis = stablePoint
    updateStartingPoint _ pt = pt
