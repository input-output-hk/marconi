{-# LANGUAGE LambdaCase #-}

module Marconi.Sidechain.Api.Query.Indexers.MintBurn (
  initializeEnv,
  queryByPolicyAndAssetId,
  updateEnvState,
  findByAssetIdAtSlot,
) where

import Cardano.Api qualified as C
import Control.Concurrent.STM (STM, TMVar, atomically, newEmptyTMVarIO, readTMVar)
import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Marconi.ChainIndex.Error qualified as CI
import Marconi.ChainIndex.Indexers.MintBurn (
  MintBurnHandle,
  StorableResult (MintBurnResult),
  TxMintRow,
 )
import Marconi.ChainIndex.Indexers.MintBurn qualified as MintBurn
import Marconi.Core.Storable (State)
import Marconi.Core.Storable qualified as Storable
import Marconi.Sidechain.Api.Routes (AssetIdTxResult (AssetIdTxResult))
import Marconi.Sidechain.Api.Types (
  MintBurnIndexerEnv (MintBurnIndexerEnv),
  QueryExceptions (QueryError, UntrackedPolicy),
  SidechainEnv,
  mintBurnIndexerEnvIndexer,
  mintBurnIndexerEnvTargetAssets,
  sidechainEnvIndexers,
  sidechainMintBurnIndexer,
 )
import Marconi.Sidechain.Utils (writeTMVar)

{- | Bootstraps the utxo query environment.
 The module is responsible for accessing SQLite for quries.
 The main issue we try to avoid here is mixing inserts and quries in SQLite to avoid locking the database
-}
initializeEnv
  :: Maybe (NonEmpty (C.PolicyId, Maybe C.AssetName))
  -> IO MintBurnIndexerEnv
  -- ^ returns Query runtime environment
initializeEnv targets = do
  ix <- newEmptyTMVarIO
  pure $ MintBurnIndexerEnv targets ix

updateEnvState :: TMVar (State MintBurnHandle) -> State MintBurnHandle -> STM ()
updateEnvState = writeTMVar

findByAssetIdAtSlot
  :: SidechainEnv
  -> C.PolicyId
  -> Maybe C.AssetName
  -> Maybe Word64
  -> Maybe C.TxId
  -> IO (Either QueryExceptions [AssetIdTxResult])
findByAssetIdAtSlot env policy asset slotWord =
  queryByPolicyAndAssetId env policy asset (fromIntegral <$> slotWord)

queryByPolicyAndAssetId
  :: SidechainEnv
  -> C.PolicyId
  -> Maybe C.AssetName
  -> Maybe C.SlotNo
  -> Maybe C.TxId
  -> IO (Either QueryExceptions [AssetIdTxResult])
queryByPolicyAndAssetId env policyId assetId slotNo txId = do
  let mintBurnEnv = env ^. sidechainEnvIndexers . sidechainMintBurnIndexer
  let trackedAssets = mintBurnEnv ^. mintBurnIndexerEnvTargetAssets
  mintBurnIndexer <- atomically $ readTMVar $ mintBurnEnv ^. mintBurnIndexerEnvIndexer
  if isTracked trackedAssets
    then query mintBurnIndexer
    else pure $ Left $ UntrackedPolicy policyId assetId
  where
    query indexer = do
      let q = MintBurn.QueryBurnByAssetId policyId assetId slotNo txId
      res <- runExceptT $ Storable.query indexer q
      case res of
        Right (MintBurnResult mintBurnRows) -> pure $ Right $ toAssetIdTxResult <$> mintBurnRows
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
        _other -> pure $ Left $ QueryError "invalid query result"

    validAssetName maybeName = fromMaybe True $ do
      name <- maybeName
      assetId' <- assetId
      pure $ name == assetId'

    isTracked = \case
      Nothing -> True
      Just trackedAssets -> any (\(pid, name) -> pid == policyId && validAssetName name) trackedAssets

    toAssetIdTxResult :: TxMintRow -> AssetIdTxResult
    toAssetIdTxResult x =
      AssetIdTxResult
        (x ^. MintBurn.txMintRowSlotNo)
        (x ^. MintBurn.txMintRowBlockHeaderHash)
        (x ^. MintBurn.txMintRowBlockNo)
        (x ^. MintBurn.txMintRowTxId)
        (MintBurn.mintAssetRedeemerHash <$> x ^. MintBurn.txMintRowRedeemer)
        (MintBurn.mintAssetRedeemerData <$> x ^. MintBurn.txMintRowRedeemer)
        (x ^. MintBurn.txMintRowAssetName)
        (negate $ x ^. MintBurn.txMintRowQuantity)
