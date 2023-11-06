{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
This module contains a bunch of utility functions for working in `marconi-chain-index`.
-}
module Marconi.ChainIndex.Utils (
  isBlockRollbackable,
  getBlockNoFromChainTip,
  queryCurrentNodeBlockNo,
  querySecurityParam,
  querySecurityParamEra,
  toException,
  chainPointOrGenesis,
  addressesToPredicate,
  chainPointToSlotNo,
  chainPointToHash,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.IPC qualified as C
import Cardano.Slotting.Slot (WithOrigin (At, Origin))
import Control.Exception (Exception, throw)
import Control.Monad.Except (
  ExceptT,
  runExceptT,
  throwError,
 )
import Control.Monad.Trans (MonadTrans (lift))
import Data.Text (pack)
import Marconi.ChainIndex.Error (IndexerError (CantStartIndexer))
import Marconi.ChainIndex.Types (
  SecurityParam,
  TargetAddresses,
 )
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

-- | Query the current tip from the local node.
queryCurrentNodeBlockNo
  :: C.NetworkId
  -> FilePath
  -- ^ Node socket file path
  -> ExceptT (IndexerError err) IO C.BlockNo
queryCurrentNodeBlockNo networkId socketPath = do
  let localNodeConnectInfo :: C.LocalNodeConnectInfo C.CardanoMode
      localNodeConnectInfo = C.mkLocalNodeConnectInfo networkId socketPath

      queryInMode :: C.QueryInMode C.CardanoMode (WithOrigin C.BlockNo)
      queryInMode = C.QueryChainBlockNo

      toError :: (Show a) => a -> ExceptT (IndexerError err) IO b
      toError = throwError . CantStartIndexer . pack . show

  result <- lift $ C.queryNodeLocalState localNodeConnectInfo Nothing queryInMode
  case result of
    Left err -> toError err
    Right Origin -> toError ("The node chain is still at genesis. Did it start syncing?" :: String)
    Right (At blockNo) -> pure blockNo

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
  case shelleyBasedToCardanoEra era of
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
      localNodeConnectInfo = C.mkLocalNodeConnectInfo networkId socketPath

      queryInMode :: C.QueryInMode C.CardanoMode C.AnyCardanoEra
      queryInMode = C.QueryCurrentEra C.CardanoModeIsMultiEra

      toError :: (Show a) => a -> ExceptT (IndexerError err) IO b
      toError = throwError . CantStartIndexer . pack . show

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
    localNodeConnectInfo = C.mkLocalNodeConnectInfo networkId socketPath

    queryInMode :: C.QueryInMode C.CardanoMode (Either EraMismatch (C.GenesisParameters C.ShelleyEra))
    queryInMode =
      C.QueryInEra (toShelleyEraInCardanoMode shelleyBasedEra) $
        C.QueryInShelleyBasedEra shelleyBasedEra C.QueryGenesisParameters

    toError :: (Show a) => a -> ExceptT (IndexerError err) IO b
    toError = throwError . CantStartIndexer . pack . show

toException :: (Exception err) => ExceptT err IO a -> IO a
toException mx = do
  x <- runExceptT mx
  case x of
    Left err -> throw err
    Right res -> pure res

{- | Return the first element of the list of chain points. If the list is empty, return the genesis
 point.
-}
chainPointOrGenesis :: [C.ChainPoint] -> C.ChainPoint
chainPointOrGenesis result = case result of
  [] -> C.ChainPointAtGenesis
  cp : _ -> cp

-- | Convert a list of target addresses to a predicate function
addressesToPredicate :: Maybe TargetAddresses -> Maybe (C.Address C.ShelleyAddr -> Bool)
addressesToPredicate = fmap (\list -> (`elem` list))

-- TODO This should be moved to `cardano-api`.
toShelleyEraInCardanoMode :: C.ShelleyBasedEra era -> C.EraInMode era C.CardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraShelley = C.ShelleyEraInCardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraAllegra = C.AllegraEraInCardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraMary = C.MaryEraInCardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraAlonzo = C.AlonzoEraInCardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraBabbage = C.BabbageEraInCardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraConway = C.ConwayEraInCardanoMode

{- | Converts a 'CardanoEra' to the more specific 'ShelleyBasedEra'.
 TODO This should be moved to `cardano-api`.
-}
shelleyBasedToCardanoEra :: C.CardanoEra era -> Maybe (C.ShelleyBasedEra era)
shelleyBasedToCardanoEra C.ByronEra = Nothing
shelleyBasedToCardanoEra C.ShelleyEra = Just C.ShelleyBasedEraShelley
shelleyBasedToCardanoEra C.AllegraEra = Just C.ShelleyBasedEraAllegra
shelleyBasedToCardanoEra C.MaryEra = Just C.ShelleyBasedEraMary
shelleyBasedToCardanoEra C.AlonzoEra = Just C.ShelleyBasedEraAlonzo
shelleyBasedToCardanoEra C.BabbageEra = Just C.ShelleyBasedEraBabbage
shelleyBasedToCardanoEra C.ConwayEra = Just C.ShelleyBasedEraConway

chainPointToSlotNo :: C.ChainPoint -> Maybe C.SlotNo
chainPointToSlotNo C.ChainPointAtGenesis = Nothing
chainPointToSlotNo (C.ChainPoint s _) = Just s

chainPointToHash :: C.ChainPoint -> Maybe (C.Hash C.BlockHeader)
chainPointToHash C.ChainPointAtGenesis = Nothing
chainPointToHash (C.ChainPoint _ h) = Just h
