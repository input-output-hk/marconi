module Marconi.Sidechain.Api.Query.Indexers.MintBurn (
  initializeEnv,
  queryByPolicyAndAssetId,
  updateEnvState,
  findByAssetIdAtSlot,
) where

import Cardano.Api qualified as C

import Control.Concurrent.STM (STM, TMVar, atomically, newEmptyTMVarIO, tryReadTMVar)
import Control.Lens ((^.))

import Data.Word (Word64)

import Control.Monad.Except (runExceptT)
import Data.List.NonEmpty (NonEmpty)
import Marconi.ChainIndex.Indexers.MintBurn (MintBurnHandle, StorableResult (MintBurnResult), TxMintRow)
import Marconi.ChainIndex.Indexers.MintBurn qualified as MintBurn
import Marconi.Core.Storable (State)
import Marconi.Core.Storable qualified as Storable
import Marconi.Sidechain.Api.Routes (AssetIdTxResult (AssetIdTxResult))
import Marconi.Sidechain.Api.Types (
  MintBurnIndexerEnv (MintBurnIndexerEnv),
  QueryExceptions (QueryError),
  SidechainEnv,
  mintBurnIndexerEnvIndexer,
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
  -> C.AssetName
  -> Maybe Word64
  -> IO (Either QueryExceptions [AssetIdTxResult])
findByAssetIdAtSlot env policy asset slotWord =
  queryByPolicyAndAssetId env policy asset (fromIntegral <$> slotWord)

queryByPolicyAndAssetId
  :: SidechainEnv
  -> C.PolicyId
  -> C.AssetName
  -> Maybe C.SlotNo
  -> IO (Either QueryExceptions [AssetIdTxResult])
queryByPolicyAndAssetId env policyId assetId slotNo = do
  mintBurnIndexer <-
    atomically $
      tryReadTMVar $
        env ^. sidechainEnvIndexers . sidechainMintBurnIndexer . mintBurnIndexerEnvIndexer
  case mintBurnIndexer of
    Nothing ->
      -- May occur at startup before marconi-sidechain gets to update the indexer
      pure $ Right []
    Just indexer -> query indexer
  where
    query indexer = do
      let q = MintBurn.QueryBurnByAssetId policyId assetId slotNo
      res <- runExceptT $ Storable.query indexer q
      case res of
        Right (MintBurnResult mintBurnRows) -> pure $ Right $ toAssetIdTxResult <$> mintBurnRows
        _other -> pure $ Left $ QueryError "invalid query result"

    toAssetIdTxResult :: TxMintRow -> AssetIdTxResult
    toAssetIdTxResult x =
      AssetIdTxResult
        (x ^. MintBurn.txMintRowSlotNo)
        (x ^. MintBurn.txMintRowBlockHeaderHash)
        (x ^. MintBurn.txMintRowBlockNo)
        (x ^. MintBurn.txMintRowTxId)
        (Just $ x ^. MintBurn.txMintRowRedeemerHash)
        (Just $ x ^. MintBurn.txMintRowRedeemerData)
        (x ^. MintBurn.txMintRowQuantity)
