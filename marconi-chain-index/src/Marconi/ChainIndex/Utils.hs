{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

{- |
This module contains a bunch of utility functions for working in `marconi-chain-index`.
-}
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

-- | Query security param from the local node.
-- It queries the current era first, and uses that to query the security parameter.
querySecurityParam
    :: C.NetworkId
    -> FilePath -- ^ Node socket file path
    -> ExceptT IndexerError IO SecurityParam
querySecurityParam networkId socketPath = do
    (C.AnyCardanoEra era) <- queryCurrentEra networkId socketPath
    case shelleyBasedToCardanoEra era of
      Nothing -> throwError $ CantStartIndexer "The security parameter can only be queried in shelley based era."
      Just shelleyBasedEra -> querySecurityParamEra shelleyBasedEra networkId socketPath

-- | Query the current era of the local node's current state.
queryCurrentEra
  :: C.NetworkId
  -> FilePath -- ^ Node socket file path
  -> ExceptT IndexerError IO C.AnyCardanoEra
queryCurrentEra networkId socketPath = do
    result <- lift $ C.queryNodeLocalState localNodeConnectInfo Nothing queryInMode
    case result of
      Left err -> toError err
      Right x  -> pure x
 where
    localNodeConnectInfo :: C.LocalNodeConnectInfo C.CardanoMode
    localNodeConnectInfo = C.mkLocalNodeConnectInfo networkId socketPath

    queryInMode :: C.QueryInMode C.CardanoMode C.AnyCardanoEra
    queryInMode = C.QueryCurrentEra C.CardanoModeIsMultiEra

    toError :: Show a => a -> ExceptT IndexerError IO b
    toError = throwError . CantStartIndexer . pack . show

-- | Query security param from the local node given a Shelley based era.
querySecurityParamEra
  :: forall era
   . C.ShelleyBasedEra era
  -> C.NetworkId
  -> FilePath -- ^ Node socket file path
  -> ExceptT IndexerError IO SecurityParam
querySecurityParamEra shelleyBasedEra networkId socketPath = do
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
    queryInMode = C.QueryInEra (toShelleyEraInCardanoMode shelleyBasedEra)
      $ C.QueryInShelleyBasedEra shelleyBasedEra C.QueryGenesisParameters

    toError :: Show a => a -> ExceptT IndexerError IO b
    toError = throwError . CantStartIndexer . pack . show

-- | Return the first element of the list of chain points. If the list is empty, return the genesis
-- point.
chainPointOrGenesis :: [C.ChainPoint] -> C.ChainPoint
chainPointOrGenesis result = case result of
  []     -> C.ChainPointAtGenesis
  cp : _ -> cp

-- TODO This should be moved to `cardano-api`.
toShelleyEraInCardanoMode :: C.ShelleyBasedEra era -> C.EraInMode era C.CardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraShelley = C.ShelleyEraInCardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraAllegra = C.AllegraEraInCardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraMary    = C.MaryEraInCardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraAlonzo  = C.AlonzoEraInCardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraBabbage = C.BabbageEraInCardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraConway  = C.ConwayEraInCardanoMode

-- | Converts a 'CardanoEra' to the more specific 'ShelleyBasedEra'.
-- TODO This should be moved to `cardano-api`.
shelleyBasedToCardanoEra :: C.CardanoEra era -> Maybe (C.ShelleyBasedEra era)
shelleyBasedToCardanoEra C.ByronEra   = Nothing
shelleyBasedToCardanoEra C.ShelleyEra = Just C.ShelleyBasedEraShelley
shelleyBasedToCardanoEra C.AllegraEra = Just C.ShelleyBasedEraAllegra
shelleyBasedToCardanoEra C.MaryEra    = Just C.ShelleyBasedEraMary
shelleyBasedToCardanoEra C.AlonzoEra  = Just C.ShelleyBasedEraAlonzo
shelleyBasedToCardanoEra C.BabbageEra = Just C.ShelleyBasedEraBabbage
shelleyBasedToCardanoEra C.ConwayEra  = Just C.ShelleyBasedEraConway
