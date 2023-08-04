module Marconi.Sidechain.Api.Query.Indexers.Utxo (
  initializeEnv,
  currentSyncedBlock,
  findByAddress,
  findByBech32AddressAtSlot,
  Utxo.UtxoIndexer,
  reportBech32Addresses,
  withQueryAction,
  updateEnvState,
) where

import Cardano.Api qualified as C
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, readTMVar)
import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.STM (STM)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text, pack)
import Marconi.ChainIndex.Error (IndexerError (InvalidIndexer))
import Marconi.ChainIndex.Indexers.AddressDatum (StorableQuery)
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types (TargetAddresses)
import Marconi.Core.Storable qualified as Storable
import Marconi.Sidechain.Api.Routes (
  AddressUtxoResult (AddressUtxoResult),
  GetCurrentSyncedBlockResult (GetCurrentSyncedBlockResult),
  GetUtxosFromAddressResult (GetUtxosFromAddressResult),
  SidechainTip (SidechainTip),
  SpentInfoResult (SpentInfoResult),
  UtxoTxInput (UtxoTxInput),
 )
import Marconi.Sidechain.Api.Types (
  AddressUtxoIndexerEnv (AddressUtxoIndexerEnv),
  QueryExceptions (IndexerInternalError, QueryError, UnexpectedQueryResult),
  addressUtxoIndexerEnvIndexer,
  addressUtxoIndexerEnvTargetAddresses,
 )
import Marconi.Sidechain.Utils (writeTMVar)

{- | Bootstraps the utxo query environment.
 The module is responsible for accessing SQLite for quries.
 The main issue we try to avoid here is mixing inserts and quries in SQLite to avoid locking the database
-}
initializeEnv
  :: Maybe TargetAddresses
  -- ^ user provided target addresses
  -> IO AddressUtxoIndexerEnv
  -- ^ returns Query runtime environment
initializeEnv targetAddresses = do
  ix <- newEmptyTMVarIO
  pure $ AddressUtxoIndexerEnv targetAddresses ix

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
currentSyncedBlock
  :: AddressUtxoIndexerEnv
  -- ^ Query run time environment
  -> IO (Either QueryExceptions GetCurrentSyncedBlockResult)
  -- ^ Wrong result type are unlikely but must be handled
currentSyncedBlock env = do
  indexer <- atomically (readTMVar $ env ^. addressUtxoIndexerEnvIndexer)
  res <- runExceptT $ Storable.query indexer Utxo.LastSyncedBlockInfoQuery
  pure $ case res of
    Right (Utxo.LastSyncedBlockInfoResult blockInfoM tip) ->
      Right $ GetCurrentSyncedBlockResult blockInfoM (SidechainTip tip)
    Left (InvalidIndexer err) -> Left $ IndexerInternalError err
    _other -> Left $ UnexpectedQueryResult Utxo.LastSyncedBlockInfoQuery

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
  let addrs = maybe [] NonEmpty.toList (env ^. addressUtxoIndexerEnvTargetAddresses)
   in fmap C.serialiseAddress addrs

updateEnvState :: AddressUtxoIndexerEnv -> Utxo.UtxoIndexer -> STM ()
updateEnvState (AddressUtxoIndexerEnv _ t) = writeTMVar t
