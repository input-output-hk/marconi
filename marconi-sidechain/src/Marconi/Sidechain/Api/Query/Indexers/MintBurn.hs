module Marconi.Sidechain.Api.Query.Indexers.MintBurn
    ( initializeEnv
    , queryByPolicyAndAssetId
    , updateEnvState
    , findByAssetIdAtSlot
    ) where

import Cardano.Api qualified as C

import Control.Concurrent.STM (STM, TMVar, atomically, newEmptyTMVarIO, tryReadTMVar)
import Control.Lens ((^.))

import Data.Word (Word64)

import Control.Monad.Except (runExceptT)
import Marconi.ChainIndex.Indexers.MintBurn (MintBurnHandle, StorableQuery (QueryByAssetId),
                                             StorableResult (MintBurnResult), TxMintRow, txMintRowBlockHeaderHash,
                                             txMintRowQuantity, txMintRowRedeemerData, txMintRowSlotNo, txMintRowTxId)
import Marconi.Core.Storable (State)
import Marconi.Core.Storable qualified as Storable
import Marconi.Sidechain.Api.Routes (AssetIdTxResult (AssetIdTxResult))
import Marconi.Sidechain.Api.Types (MintBurnIndexerEnv (MintBurnIndexerEnv), QueryExceptions (QueryError), SidechainEnv,
                                    mintBurnIndexerEnvIndexer, sidechainEnvIndexers, sidechainMintBurnIndexer)
import Marconi.Sidechain.Utils (writeTMVar)

-- | Bootstraps the utxo query environment.
-- The module is responsible for accessing SQLite for quries.
-- The main issue we try to avoid here is mixing inserts and quries in SQLite to avoid locking the database
initializeEnv
    :: IO MintBurnIndexerEnv -- ^ returns Query runtime environment
initializeEnv = do
    ix <- newEmptyTMVarIO
    pure $ MintBurnIndexerEnv ix

updateEnvState :: TMVar (State MintBurnHandle) -> State MintBurnHandle -> STM ()
updateEnvState = writeTMVar

findByAssetIdAtSlot
    :: SidechainEnv
    -> C.PolicyId
    -> C.AssetName
    -> Maybe Word64
    -> IO (Either QueryExceptions [AssetIdTxResult])
findByAssetIdAtSlot env policy asset slotWord
    = queryByPolicyAndAssetId env policy asset (fromIntegral <$> slotWord)


queryByPolicyAndAssetId
    :: SidechainEnv
    -> C.PolicyId
    -> C.AssetName
    -> Maybe C.SlotNo
    -> IO (Either QueryExceptions [AssetIdTxResult])
queryByPolicyAndAssetId env policyId assetId slotNo = do
    mintBurnIndexer <-
        atomically
        $ tryReadTMVar
        $ env ^. sidechainEnvIndexers . sidechainMintBurnIndexer . mintBurnIndexerEnvIndexer
    case mintBurnIndexer of
      Nothing      -> pure $ Left $ QueryError "Failed to read MintBurn indexer"
      Just indexer -> query indexer
    where
        query indexer = do
            let q = QueryByAssetId policyId assetId slotNo
            res <- runExceptT $ Storable.query Storable.QEverything indexer q
            case res of
                Right (MintBurnResult mintBurnRows) -> pure $ Right $ toAssetIdTxResult <$> mintBurnRows
                _other                              -> pure $ Left $ QueryError "invalid query result"

        toAssetIdTxResult :: TxMintRow -> AssetIdTxResult
        toAssetIdTxResult x = AssetIdTxResult
            (x ^. txMintRowBlockHeaderHash)
            (x ^. txMintRowSlotNo)
            (x ^. txMintRowTxId)
            Nothing
            (Just $ x ^. txMintRowRedeemerData)
            (x ^. txMintRowQuantity)

