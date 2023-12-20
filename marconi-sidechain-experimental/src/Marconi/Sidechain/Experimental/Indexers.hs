{-# LANGUAGE TemplateHaskell #-}

module Marconi.Sidechain.Experimental.Indexers where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming.ChainSyncEvent (ChainSyncEvent)
import Cardano.BM.Trace (Trace)
import Control.Lens (makeLenses, over, (^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Text (Text)
import Marconi.Cardano.ChainIndex.Indexers (MarconiCardanoQueryables, SyncStatsCoordinator)
import Marconi.Cardano.ChainIndex.Indexers qualified as ChainIndex.Indexers
import Marconi.Cardano.Core.Extract.WithDistance (WithDistance)
import Marconi.Cardano.Core.Logger (mkMarconiTrace)
import Marconi.Cardano.Core.Runner (RunIndexerConfig, runIndexerConfigChainPoint)
import Marconi.Cardano.Core.Runner qualified as ChainIndex.Runner
import Marconi.Cardano.Core.Types (BlockEvent, SecurityParam, TipAndBlock)
import Marconi.Cardano.Indexers.ExtLedgerStateCoordinator qualified as EpochState
import Marconi.Cardano.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.Cardano.Indexers.Utxo qualified as Utxo
import Marconi.Core (CatchupConfig, IndexerError)

{- TYPE -}

-- | Configuration for running indexers via a coordinator.
data SidechainRunIndexersConfig = SidechainRunIndexersConfig
  { _sidechainRunIndexersRunIndexerConfig :: !(RunIndexerConfig (ChainSyncEvent BlockEvent) TipAndBlock)
  , _sidechainRunIndexersCoordinator :: !SyncStatsCoordinator
  }

{- | Configuration for constructing marconi indexers as used
in this package.
-}
data SidechainBuildIndexersConfig = SidechainBuildIndexersConfig
  { _sidechainBuildIndexersTrace :: !(Trace IO Text)
  , _sidechainBuildIndexersSecurityParam :: !SecurityParam
  , _sidechainBuildIndexersCatchupConfig :: !CatchupConfig
  , _sidechainBuildIndexersDbPath :: !FilePath
  , _sidechainBuildIndexersEpochStateConfig
      :: !(EpochState.ExtLedgerStateWorkerConfig IO (WithDistance BlockEvent))
  , _sidechainBuildIndexersMintTokenEventConfig :: !MintTokenEvent.MintTokenEventConfig
  , _sidechainBuildIndexersUtxoConfig :: !Utxo.UtxoIndexerConfig
  }

makeLenses ''SidechainBuildIndexersConfig
makeLenses ''SidechainRunIndexersConfig

{- INDEXER BUILDERS AND RUNNERS -}

{- | Build the sidechain indexer workers and coordinator,
similarly to the marconi-cardano-chain-index application.
-}
sidechainBuildIndexers
  :: (MonadIO m)
  => SidechainBuildIndexersConfig
  -> m (Either IndexerError (C.ChainPoint, MarconiCardanoQueryables, SyncStatsCoordinator))
sidechainBuildIndexers config =
  liftIO . runExceptT $
    ChainIndex.Indexers.buildIndexers
      (config ^. sidechainBuildIndexersSecurityParam)
      (config ^. sidechainBuildIndexersCatchupConfig)
      (config ^. sidechainBuildIndexersUtxoConfig)
      (config ^. sidechainBuildIndexersMintTokenEventConfig)
      (config ^. sidechainBuildIndexersEpochStateConfig)
      tracer
      (mkMarconiTrace tracer)
      (config ^. sidechainBuildIndexersDbPath)
  where
    tracer = config ^. sidechainBuildIndexersTrace

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

runIndexers :: ReaderT SidechainRunIndexersConfig IO ()
runIndexers = do
  config <- ask
  liftIO $
    ChainIndex.Runner.runIndexerOnChainSync
      (config ^. sidechainRunIndexersRunIndexerConfig)
      (config ^. sidechainRunIndexersCoordinator)
