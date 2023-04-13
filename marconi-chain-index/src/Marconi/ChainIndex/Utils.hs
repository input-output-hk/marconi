{-# LANGUAGE DerivingStrategies #-}
module Marconi.ChainIndex.Utils
    ( isBlockRollbackable
    , querySecurityParam
    , querySecurityParamEra
    ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Streaming.Helpers qualified as C
import Control.Monad.IO.Class (liftIO)
import Marconi.ChainIndex.Types (SecurityParam)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

isBlockRollbackable :: SecurityParam -> C.BlockNo -> C.ChainTip -> Bool
isBlockRollbackable securityParam (C.BlockNo chainSyncBlockNo) localChainTip =
    let chainTipBlockNo =
            case localChainTip of
              C.ChainTipAtGenesis             -> 0
              (C.ChainTip _ _ (C.BlockNo bn)) -> bn
     -- TODO Need to confirm if it's "<" or "<="
     in chainTipBlockNo - chainSyncBlockNo <= fromIntegral securityParam

-- | Query security param from node
querySecurityParam :: C.NetworkId -> FilePath -> IO SecurityParam
querySecurityParam = querySecurityParamEra C.BabbageEraInCardanoMode

-- | Query security param from node
querySecurityParamEra
  :: forall era
   . C.IsShelleyBasedEra era
  => C.EraInMode era C.CardanoMode
  -> C.NetworkId
  -> FilePath
  -> IO SecurityParam
querySecurityParamEra eraInMode networkId socketPath = do
  result <- liftIO $ C.queryNodeLocalState localNodeConnectInfo Nothing queryInMode
  let genesisParameters = either showError (either showError id) (result :: Either C.AcquiringFailure (Either EraMismatch C.GenesisParameters))
  return $ fromIntegral $ C.protocolParamSecurity genesisParameters

  where
    localNodeConnectInfo :: C.LocalNodeConnectInfo C.CardanoMode
    localNodeConnectInfo = C.mkLocalNodeConnectInfo networkId socketPath

    queryInMode :: C.QueryInMode C.CardanoMode (Either EraMismatch C.GenesisParameters)
    queryInMode = C.QueryInEra eraInMode
      $ C.QueryInShelleyBasedEra (C.shelleyBasedEra @era) C.QueryGenesisParameters

    -- TODO /Really/ handle the error.
    showError :: Show a => a -> b
    showError = error . ("Marconi.ChainIndex.Utils.querySecurityParam: " <>) . show
