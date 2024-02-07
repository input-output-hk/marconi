{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Query the 'SecurityParam' from a running local node.
-}
module Marconi.Cardano.ChainIndex.SecurityParam (
  querySecurityParam,
  querySecurityParamEra,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.IPC qualified as CE
import Cardano.Api.Extended.Shelley qualified as CE
import Control.Monad.Except (
  ExceptT,
  throwError,
 )
import Control.Monad.Trans (MonadTrans (lift))
import Data.Text (pack)
import Marconi.Cardano.ChainIndex.Error (IndexerError (CantStartIndexer))
import Marconi.Cardano.Core.Types (
  SecurityParam,
 )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

{- | Query security param from the local node.
 It queries the current era first, and uses that to query the security parameter.
-}
querySecurityParam
  :: C.NetworkId
  -> FilePath
  -- ^ Node socket file path
  -> ExceptT (IndexerError err) IO SecurityParam
querySecurityParam networkId socketPath = do
  (C.AnyCardanoEra era) <- queryCurrentEra networkId socketPath
  case CE.cardanoEraToShelleyBasedEra era of
    Nothing -> throwError $ CantStartIndexer "The security parameter can only be queried in shelley based era."
    Just shelleyBasedEra -> querySecurityParamEra shelleyBasedEra networkId socketPath

-- | Query the current era of the local node's current state.
queryCurrentEra
  :: C.NetworkId
  -> FilePath
  -- ^ Node socket file path
  -> ExceptT (IndexerError err) IO C.AnyCardanoEra
queryCurrentEra networkId socketPath = do
  let localNodeConnectInfo :: C.LocalNodeConnectInfo C.CardanoMode
      localNodeConnectInfo = CE.mkLocalNodeConnectInfo networkId socketPath

      queryInMode :: C.QueryInMode C.CardanoMode C.AnyCardanoEra
      queryInMode = C.QueryCurrentEra C.CardanoModeIsMultiEra

  result <- lift $ C.queryNodeLocalState localNodeConnectInfo Nothing queryInMode
  case result of
    Left err -> toError err
    Right x -> pure x

-- | Query security param from the local node given a Shelley based era.
querySecurityParamEra
  :: forall era err
   . C.ShelleyBasedEra era
  -> C.NetworkId
  -> FilePath
  -- ^ Node socket file path
  -> ExceptT (IndexerError err) IO SecurityParam
querySecurityParamEra shelleyBasedEra networkId socketPath = do
  result <- lift $ C.queryNodeLocalState localNodeConnectInfo Nothing queryInMode
  genesisParameters <- case result of
    Left err -> toError err
    Right (Left err) -> toError err
    Right (Right x) -> pure x
  pure $ fromIntegral $ C.protocolParamSecurity genesisParameters
  where
    localNodeConnectInfo :: C.LocalNodeConnectInfo C.CardanoMode
    localNodeConnectInfo = CE.mkLocalNodeConnectInfo networkId socketPath

    queryInMode :: C.QueryInMode C.CardanoMode (Either EraMismatch (C.GenesisParameters C.ShelleyEra))
    queryInMode =
      C.QueryInEra (CE.toShelleyEraInCardanoMode shelleyBasedEra) $
        C.QueryInShelleyBasedEra shelleyBasedEra C.QueryGenesisParameters

toError :: (Show a) => a -> ExceptT (IndexerError err) IO b
toError = throwError . CantStartIndexer . pack . show
