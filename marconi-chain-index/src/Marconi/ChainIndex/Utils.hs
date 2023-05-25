{-# LANGUAGE DerivingStrategies #-}
module Marconi.ChainIndex.Utils
    ( isBlockRollbackable
    , getBlockNoFromChainTip
    , querySecurityParam
    , querySecurityParamEra
    , chainPointOrGenesis
    ) where

import Cardano.Api qualified as C
import Cardano.Streaming.Helpers qualified as C
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Text (pack)
import Marconi.ChainIndex.Error (IndexerError (CantStartIndexer))
import Marconi.ChainIndex.Types (SecurityParam)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

isBlockRollbackable :: SecurityParam -> C.BlockNo -> C.BlockNo -> Bool
isBlockRollbackable securityParam (C.BlockNo chainSyncBlockNo) (C.BlockNo chainTipBlockNo) =
     -- TODO Need to confirm if it's "<" or "<="
     chainTipBlockNo - chainSyncBlockNo <= fromIntegral securityParam

getBlockNoFromChainTip :: C.ChainTip -> C.BlockNo
getBlockNoFromChainTip chainTip =
    case chainTip of
      C.ChainTipAtGenesis -> 0
      (C.ChainTip _ _ bn) -> bn

-- | Query security param from node
querySecurityParam :: C.NetworkId -> FilePath -> ExceptT IndexerError IO SecurityParam
querySecurityParam = querySecurityParamEra C.BabbageEraInCardanoMode

-- | Query security param from node
querySecurityParamEra
  :: forall era
   . C.IsShelleyBasedEra era
  => C.EraInMode era C.CardanoMode
  -> C.NetworkId
  -> FilePath
  -> ExceptT IndexerError IO SecurityParam
querySecurityParamEra eraInMode networkId socketPath = do
  result <- lift $ C.queryNodeLocalState localNodeConnectInfo Nothing queryInMode
  genesisParameters <- case result of
      Left err         -> toError err
      Right (Left err) -> toError err
      Right (Right x)  -> pure x
  return $ fromIntegral $ C.protocolParamSecurity genesisParameters

  where
    localNodeConnectInfo :: C.LocalNodeConnectInfo C.CardanoMode
    localNodeConnectInfo = C.mkLocalNodeConnectInfo networkId socketPath

    queryInMode :: C.QueryInMode C.CardanoMode (Either EraMismatch C.GenesisParameters)
    queryInMode = C.QueryInEra eraInMode
      $ C.QueryInShelleyBasedEra (C.shelleyBasedEra @era) C.QueryGenesisParameters

    toError :: Show a => a -> ExceptT IndexerError IO b
    toError = throwError . CantStartIndexer . pack . show

-- | Return the first element of the list of chain points. If the list is empty, return the genesis
-- point.
chainPointOrGenesis :: [C.ChainPoint] -> C.ChainPoint
chainPointOrGenesis result = case result of
  []     -> C.ChainPointAtGenesis
  cp : _ -> cp
