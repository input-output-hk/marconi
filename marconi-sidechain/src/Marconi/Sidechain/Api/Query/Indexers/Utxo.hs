module Marconi.Sidechain.Api.Query.Indexers.Utxo (
  queryCurrentSyncedBlock,
  queryBlockNoAtSlotNo,
  queryCurrentNodeBlockNo,
  findByAddress,
  findByBech32AddressAtSlot,
  Utxo.UtxoIndexer,
  reportBech32Addresses,
  withQueryAction,
  updateEnvState,
) where

import Cardano.Api qualified as C
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (readTMVar)
import Control.Exception (throwIO)
import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.STM (STM)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Text (Text, pack)
import Marconi.ChainIndex.Error (IndexerError (InvalidIndexer))
import Marconi.ChainIndex.Error qualified as CI
import Marconi.ChainIndex.Indexers.AddressDatum (StorableQuery)
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Core.Storable qualified as Storable
import Marconi.Sidechain.Api.Routes (
  AddressUtxoResult (AddressUtxoResult),
  GetCurrentSyncedBlockResult (GetCurrentSyncedBlockResult),
  GetUtxosFromAddressResult (GetUtxosFromAddressResult),
  SidechainTip (SidechainTip),
  SpentInfoResult (SpentInfoResult),
  UtxoTxInput (UtxoTxInput),
 )
import Marconi.Sidechain.Env (
  AddressUtxoIndexerEnv (AddressUtxoIndexerEnv),
  addressUtxoIndexerEnvIndexer,
  addressUtxoIndexerEnvTargetAddresses,
 )
import Marconi.Sidechain.Error (
  QueryExceptions (IndexerInternalError, QueryError, UnexpectedQueryResult),
 )
import Marconi.Sidechain.Utils (writeTMVar)

{- | Query utxos by Address
  Address conversion error from Bech32 may occur
-}
findByAddress
  :: AddressUtxoIndexerEnv
  -- ^ Query run time environment
  -> Utxo.QueryUtxoByAddress
  -> IO (Either QueryExceptions GetUtxosFromAddressResult)
findByAddress env = withQueryAction env . Utxo.QueryUtxoByAddressWrapper

-- | Retrieve the current synced point of the utxo indexer
queryCurrentSyncedBlock
  :: AddressUtxoIndexerEnv
  -- ^ Query run time environment
  -> IO (Either QueryExceptions GetCurrentSyncedBlockResult)
  -- ^ Wrong result type are unlikely but must be handled
queryCurrentSyncedBlock env = do
  indexer <- atomically (readTMVar $ env ^. addressUtxoIndexerEnvIndexer)
  res <- runExceptT $ Storable.query indexer Utxo.LastSyncedBlockInfoQuery
  pure $ case res of
    Right (Utxo.LastSyncedBlockInfoResult blockInfoM tip) ->
      Right $ GetCurrentSyncedBlockResult blockInfoM (SidechainTip tip)
    Left (InvalidIndexer err) -> Left $ IndexerInternalError err
    _other -> Left $ UnexpectedQueryResult Utxo.LastSyncedBlockInfoQuery

queryCurrentNodeBlockNo :: AddressUtxoIndexerEnv -> IO (Either QueryExceptions C.BlockNo)
queryCurrentNodeBlockNo env = do
  currentSyncedBlock <- queryCurrentSyncedBlock env
  pure $
    fmap
      ( \(GetCurrentSyncedBlockResult _ (SidechainTip chainTip)) ->
          Utils.getBlockNoFromChainTip chainTip
      )
      currentSyncedBlock

queryBlockNoAtSlotNo :: AddressUtxoIndexerEnv -> C.SlotNo -> IO C.BlockNo
queryBlockNoAtSlotNo env slotNo = do
  indexer <- atomically $ readTMVar $ env ^. addressUtxoIndexerEnvIndexer
  let q = Utxo.BlockNoFromSlotNoQuery slotNo
  res <- runExceptT $ Storable.query indexer q
  case res of
    Right (Utxo.BlockNoFromSlotNoResult (Just blockNo)) ->
      pure blockNo
    Left (CI.InvalidIndexer err) -> throwIO $ IndexerInternalError err
    _other -> throwIO $ UnexpectedQueryResult q

{- | Retrieve Utxos associated with the given address
 We return an empty list if no address is not found
-}
findByBech32AddressAtSlot
  :: AddressUtxoIndexerEnv
  -- ^ Query run time environment
  -> Text
  -- ^ Bech32 Address
  -> Utxo.Interval C.SlotNo
  -- ^ slotNo query interval
  -> IO (Either QueryExceptions GetUtxosFromAddressResult)
  -- ^ Plutus address conversion error may occur
findByBech32AddressAtSlot env addressText slotInterval =
  let toQueryExceptions e = QueryError (addressText <> " generated error: " <> pack (show e))

      queryAtAddressAndSlot
        :: Utxo.QueryUtxoByAddress -> IO (Either QueryExceptions GetUtxosFromAddressResult)
      queryAtAddressAndSlot = findByAddress env

      query :: Either QueryExceptions Utxo.QueryUtxoByAddress
      query = do
        addr <-
          bimap toQueryExceptions C.toAddressAny $
            C.deserialiseFromBech32 C.AsShelleyAddress addressText
        pure $ Utxo.QueryUtxoByAddress addr slotInterval
   in case query of
        Right q -> queryAtAddressAndSlot q
        Left e -> pure $ Left e

{- | Execute the query function
 We must stop the utxo inserts before doing the query
-}
withQueryAction
  :: AddressUtxoIndexerEnv
  -- ^ Query run time environment
  -> StorableQuery Utxo.UtxoHandle
  -- ^ Address and slot to query
  -> IO (Either QueryExceptions GetUtxosFromAddressResult)
withQueryAction env query =
  (atomically $ readTMVar $ env ^. addressUtxoIndexerEnvIndexer) >>= action
  where
    action indexer = do
      res <- runExceptT $ Storable.query indexer query
      let spentInfoResult row =
            SpentInfoResult
              (row ^. Utxo.srSpentBlockInfo . Utxo.blockInfoSlotNo)
              (row ^. Utxo.srSpentTxId)
      pure $ case res of
        Right (Utxo.UtxoByAddressResult rows) ->
          Right $
            GetUtxosFromAddressResult $
              rows <&> \row ->
                let bi = Utxo.utxoResultBlockInfo row
                 in AddressUtxoResult
                      (Utxo._blockInfoSlotNo bi)
                      (Utxo._blockInfoBlockHeaderHash bi)
                      (Utxo._blockInfoBlockNo $ Utxo.utxoResultBlockInfo row)
                      (Utxo.utxoResultTxIndexInBlock row)
                      (Utxo.utxoResultTxIn row)
                      (Utxo.utxoResultDatumHash row)
                      (Utxo.utxoResultDatum row)
                      (Utxo.utxoResultValue row)
                      (spentInfoResult <$> Utxo.utxoResultSpentInfo row)
                      (UtxoTxInput <$> Utxo.utxoResultTxIns row)
        Left (InvalidIndexer err) -> Left $ IndexerInternalError err
        _other ->
          Left $ UnexpectedQueryResult query

{- | report target addresses
 Used by JSON-RPC
-}
reportBech32Addresses
  :: AddressUtxoIndexerEnv
  -> [Text]
reportBech32Addresses env =
  let addrs = maybe [] toList (env ^. addressUtxoIndexerEnvTargetAddresses)
   in fmap C.serialiseAddress addrs

updateEnvState :: AddressUtxoIndexerEnv -> Utxo.UtxoIndexer -> STM ()
updateEnvState (AddressUtxoIndexerEnv _ t) = writeTMVar t
