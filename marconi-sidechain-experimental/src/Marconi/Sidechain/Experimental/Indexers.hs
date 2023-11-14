{-# LANGUAGE TemplateHaskell #-}

module Marconi.Sidechain.Experimental.Indexers where

import Cardano.Api qualified as C
import Cardano.BM.Trace (Trace)
import Control.Lens (makeLenses, over, (^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
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

-- | Configuration for running indexers via a coordinator.
data SidechainRunIndexersConfig = SidechainRunIndexersConfig
  { _sidechainRunIndexersRunIndexerConfig :: !(RunIndexerConfig TipOrBlock)
  , _sidechainRunIndexersCoordinator :: !ChainIndex.Indexers.Coordinator
  }

{- | Configuration for constructing marconi indexers as used
in this package.
-}
data SidechainBuildIndexersConfig = SidechainBuildIndexersConfig
  { _sidechainBuildIndexersTrace :: !(Trace IO Text)
  , _sidechainBuildIndexersSecurityParam :: !SecurityParam
  , _sidechainBuildIndexersCatchupConfig :: !CatchupConfig
  , _sidechainBuildIndexersDbPath :: !FilePath
  , _sidechainBuildIndexersEpochStateConfig :: !EpochState.EpochStateWorkerConfig
  , _sidechainBuildIndexersMintTokenEventConfig :: !MintTokenEvent.MintTokenEventConfig
  , _sidechainBuildIndexersUtxoConfig :: !Utxo.UtxoIndexerConfig
  }

makeLenses ''SidechainBuildIndexersConfig
makeLenses ''SidechainRunIndexersConfig

{- INDEXER BUILDERS AND RUNNERS -}

{- | Build the sidechain indexer workers and coordinator,
similarly to the marconi-chain-index application.
-}
sidechainBuildIndexers
  :: (MonadIO m)
  => SidechainBuildIndexersConfig
  -> m (Either IndexerError (C.ChainPoint, MarconiChainIndexQueryables, Coordinator))
sidechainBuildIndexers config =
  liftIO . runExceptT $
    ChainIndex.Indexers.buildIndexers
      (config ^. sidechainBuildIndexersSecurityParam)
      (config ^. sidechainBuildIndexersCatchupConfig)
      (config ^. sidechainBuildIndexersUtxoConfig)
      (config ^. sidechainBuildIndexersMintTokenEventConfig)
      (config ^. sidechainBuildIndexersEpochStateConfig)
      (config ^. sidechainBuildIndexersTrace)
      (config ^. sidechainBuildIndexersDbPath)

updateRunIndexerConfigWithLastStable
  :: C.ChainPoint -> SidechainRunIndexersConfig -> SidechainRunIndexersConfig
updateRunIndexerConfigWithLastStable lastStable =
  over
    (sidechainRunIndexersRunIndexerConfig . runIndexerConfigChainPoint)
    (updateStartingPoint lastStable)
  where
    updateStartingPoint :: C.ChainPoint -> C.ChainPoint -> C.ChainPoint
    updateStartingPoint stablePoint C.ChainPointAtGenesis = stablePoint
    updateStartingPoint _ pt = pt

-- TODO: Should implement failsIfResync from original sidechain
runIndexers :: ReaderT SidechainRunIndexersConfig IO ()
runIndexers = do
  config <- ask
  liftIO $
    ChainIndex.Runner.runIndexer
      (config ^. sidechainRunIndexersRunIndexerConfig)
      (config ^. sidechainRunIndexersCoordinator)
