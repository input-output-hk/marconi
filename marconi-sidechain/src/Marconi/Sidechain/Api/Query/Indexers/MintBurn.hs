{-# LANGUAGE LambdaCase #-}

module Marconi.Sidechain.Api.Query.Indexers.MintBurn (
  queryByPolicyAndAssetId,
  updateEnvState,
  queryByAssetIdAtSlot,
) where

import Cardano.Api qualified as C
import Control.Concurrent.STM (STM, TMVar, atomically, readTMVar)
import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Marconi.ChainIndex.Error qualified as CI
import Marconi.ChainIndex.Indexers.MintBurn (
  MintBurnHandle,
  StorableResult (MintBurnResult),
  TxMintRow,
 )
import Marconi.ChainIndex.Indexers.MintBurn qualified as MintBurn
import Marconi.ChainIndex.Types (SecurityParam)
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Core.Storable (State)
import Marconi.Core.Storable qualified as Storable
import Marconi.Sidechain.Api.Query.Indexers.Utxo qualified as Utxo
import Marconi.Sidechain.Api.Routes (
  BurnTokenEventResult (BurnTokenEventResult),
 )
import Marconi.Sidechain.Env (
  SidechainIndexersEnv,
  mintBurnIndexerEnvIndexer,
  mintBurnIndexerEnvTargetAssets,
  sidechainAddressUtxoIndexer,
  sidechainMintBurnIndexer,
 )
import Marconi.Sidechain.Error (
  QueryExceptions (IndexerInternalError, QueryError, UntrackedPolicy),
 )
import Marconi.Sidechain.Utils (writeTMVar)

updateEnvState :: TMVar (State MintBurnHandle) -> State MintBurnHandle -> STM ()
updateEnvState = writeTMVar

queryByAssetIdAtSlot
  :: SecurityParam
  -> SidechainIndexersEnv
  -> C.PolicyId
  -> Maybe C.AssetName
  -> Maybe Word64
  -> Maybe C.TxId
  -> IO (Either QueryExceptions [BurnTokenEventResult])
queryByAssetIdAtSlot securityParam env policy asset slotWord =
  queryByPolicyAndAssetId securityParam env policy asset (fromIntegral <$> slotWord)

queryByPolicyAndAssetId
  :: SecurityParam
  -> SidechainIndexersEnv
  -> C.PolicyId
  -> Maybe C.AssetName
  -> Maybe C.SlotNo
  -> Maybe C.TxId
  -> IO (Either QueryExceptions [BurnTokenEventResult])
queryByPolicyAndAssetId securityParam env policyId assetId slotNo txId = do
  let trackedAssets = env ^. sidechainMintBurnIndexer . mintBurnIndexerEnvTargetAssets
  mintBurnIndexer <-
    atomically $ readTMVar $ env ^. sidechainMintBurnIndexer . mintBurnIndexerEnvIndexer
  if isTracked trackedAssets
    then query mintBurnIndexer
    else pure $ Left $ UntrackedPolicy policyId assetId
  where
    query indexer = do
      let q = MintBurn.QueryBurnByAssetId policyId assetId slotNo txId
      res <- runExceptT $ Storable.query indexer q
      case res of
        Right (MintBurnResult mintBurnRows) -> do
          case slotNo of
            Nothing -> do
              -- The user didn't provide an upper bound slot number. Therefore, we query the latest
              -- block number of the local node.
              blockNoE <- Utxo.queryCurrentNodeBlockNo (env ^. sidechainAddressUtxoIndexer)
              pure $ do
                blockNo <- blockNoE
                pure $ toAssetIdTxResult blockNo <$> mintBurnRows
            Just s -> do
              -- The user provided an upper bound slot number. Therefore, we convert the provided
              -- slot number to a block number by using the indexer which indexes that information.
              blockNoE <- Utxo.queryBlockNoAtSlotNo (env ^. sidechainAddressUtxoIndexer) s
              pure $ do
                blockNo <- blockNoE
                pure $ toAssetIdTxResult blockNo <$> mintBurnRows
        Left (CI.QueryError MintBurn.InvalidInterval{}) ->
          pure $
            Left $
              QueryError
                "The 'createdBeforeSlotNo' param value must be larger than the slot number of the 'createdAfterTx' transaction."
        Left (CI.QueryError MintBurn.TxNotIndexed{}) ->
          pure $
            Left $
              QueryError
                "The 'createdAfterTx' param value must be an existing transaction ID in the Cardano network that burned a token ('AssetId')."
        Left (CI.InvalidIndexer err) -> pure $ Left $ IndexerInternalError err
        _other -> pure $ Left $ QueryError "invalid query result"

    validAssetName maybeName = fromMaybe True $ do
      name <- maybeName
      assetId' <- assetId
      pure $ name == assetId'

    isTracked = \case
      Nothing -> True
      Just trackedAssets -> any (\(pid, name) -> pid == policyId && validAssetName name) trackedAssets

    toAssetIdTxResult :: C.BlockNo -> TxMintRow -> BurnTokenEventResult
    toAssetIdTxResult nodeTipBlockNo x =
      BurnTokenEventResult
        (x ^. MintBurn.txMintRowSlotNo)
        (x ^. MintBurn.txMintRowBlockHeaderHash)
        (x ^. MintBurn.txMintRowBlockNo)
        (x ^. MintBurn.txMintRowTxId)
        (MintBurn.mintAssetRedeemerHash <$> x ^. MintBurn.txMintRowRedeemer)
        (MintBurn.mintAssetRedeemerData <$> x ^. MintBurn.txMintRowRedeemer)
        (x ^. MintBurn.txMintRowAssetName)
        (negate $ x ^. MintBurn.txMintRowQuantity)
        (isTxMintRowStable securityParam x nodeTipBlockNo)

isTxMintRowStable :: SecurityParam -> TxMintRow -> C.BlockNo -> Bool
isTxMintRowStable securityParam txMintRow blockNo =
  not $ Utils.isBlockRollbackable securityParam (txMintRow ^. MintBurn.txMintRowBlockNo) blockNo
