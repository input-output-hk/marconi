module Marconi.Sidechain.Api.Query.Indexers.Utxo
    ( initializeEnv
    , currentSyncedBlock
    , findByAddress
    , findByBech32AddressAtSlot
    , reportQueryAddresses
    , Utxo.UtxoIndexer
    , reportBech32Addresses
    , withQueryAction
    , updateEnvState
    ) where

import Cardano.Api qualified as C
import Control.Arrow (left)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, tryReadTMVar)
import Control.Lens ((^.), (^?))
import Control.Monad.Except (runExceptT)
import Control.Monad.STM (STM)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text, unpack)
import GHC.Word (Word64)
import Marconi.ChainIndex.Error (IndexerError)
import Marconi.ChainIndex.Indexers.AddressDatum (StorableQuery)
import Marconi.ChainIndex.Indexers.Utxo (address, datum, datumHash, txIn, urCreationBlockHash, urCreationSlotNo,
                                         urSpentSlotNo, urSpentTxId, urUtxo)
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types (TargetAddresses)
import Marconi.Core.Storable qualified as Storable
import Marconi.Sidechain.Api.Routes (AddressUtxoResult (AddressUtxoResult),
                                     GetCurrentSyncedBlockResult (GetCurrentSyncedBlockResult),
                                     GetUtxosFromAddressResult (GetUtxosFromAddressResult),
                                     SpentInfoResult (SpentInfoResult))
import Marconi.Sidechain.Api.Types (AddressUtxoIndexerEnv (AddressUtxoIndexerEnv),
                                    QueryExceptions (QueryError, UnexpectedQueryResult), addressUtxoIndexerEnvIndexer,
                                    addressUtxoIndexerEnvTargetAddresses)
import Marconi.Sidechain.Utils (writeTMVar)

-- | Bootstraps the utxo query environment.
-- The module is responsible for accessing SQLite for quries.
-- The main issue we try to avoid here is mixing inserts and quries in SQLite to avoid locking the database
initializeEnv
    :: Maybe TargetAddresses -- ^ user provided target addresses
    -> IO AddressUtxoIndexerEnv -- ^ returns Query runtime environment
initializeEnv targetAddresses = do
    ix <- newEmptyTMVarIO
    pure $ AddressUtxoIndexerEnv targetAddresses ix

-- | Query utxos by Address
--  Address conversion error from Bech32 may occur
findByAddress
    :: AddressUtxoIndexerEnv -- ^ Query run time environment
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
    indexer <- atomically
        (tryReadTMVar $ env ^. addressUtxoIndexerEnvIndexer)
    case indexer of
         Just i -> do
             res <- runExceptT $ Storable.query i Utxo.LastSyncPoint
             case res of
                  Right (Utxo.LastSyncPointResult cp) -> pure $ Right $ GetCurrentSyncedBlockResult cp
                  _other                              -> pure $ Left $ UnexpectedQueryResult Utxo.LastSyncPoint
         Nothing -> pure . Right $ GetCurrentSyncedBlockResult C.ChainPointAtGenesis

-- | Retrieve Utxos associated with the given address
-- We return an empty list if no address is not found
findByBech32AddressAtSlot
    :: AddressUtxoIndexerEnv -- ^ Query run time environment
    -> Text -- ^ Bech32 Address
    -> Word64 -- ^ slotNo upper bound query
    -> Maybe Word64 -- ^ slotNo lower bound query
    -> IO (Either QueryExceptions GetUtxosFromAddressResult) -- ^ Plutus address conversion error may occur
findByBech32AddressAtSlot env addressText upperBoundSlotNo lowerBoundSlotNo
    = let

        toQueryExceptions e = QueryError (unpack  addressText <> " generated error: " <> show e)

        intervalWrapper :: Maybe C.SlotNo -> C.SlotNo -> Either QueryExceptions (Utxo.Interval C.SlotNo)
        intervalWrapper s s' =

          let
            f :: IndexerError -> QueryExceptions
            f = QueryError . show

          in left f (Utxo.interval s s')

        slotInterval :: Either QueryExceptions (Utxo.Interval C.SlotNo)
        slotInterval = intervalWrapper
             (C.SlotNo <$> lowerBoundSlotNo)
             (C.SlotNo upperBoundSlotNo)

        utxoQuery :: C.AddressAny -> Utxo.Interval C.SlotNo  -> Utxo.QueryUtxoByAddress
        utxoQuery addr = Utxo.QueryUtxoByAddress addr

        queryAtAddressAndSlot :: Utxo.QueryUtxoByAddress -> IO (Either QueryExceptions GetUtxosFromAddressResult)
        queryAtAddressAndSlot = findByAddress env

        query :: Either QueryExceptions Utxo.QueryUtxoByAddress
        query = do
          si <- slotInterval
          addr <-
            bimap toQueryExceptions C.toAddressAny
            $ C.deserialiseFromBech32 C.AsShelleyAddress addressText
          pure $ utxoQuery addr si

      in case query of
           Right q -> queryAtAddressAndSlot q
           Left e  -> pure $ Left e

-- | Execute the query function
-- We must stop the utxo inserts before doing the query
withQueryAction
    :: AddressUtxoIndexerEnv -- ^ Query run time environment
    -> StorableQuery Utxo.UtxoHandle -- ^ Address and slot to query
    -> IO (Either QueryExceptions GetUtxosFromAddressResult)
withQueryAction env query =
  (atomically $ tryReadTMVar $ env ^. addressUtxoIndexerEnvIndexer) >>= action
  where
    action Nothing = pure $ Right $ GetUtxosFromAddressResult [] -- may occures at startup before marconi-chain-index gets to update the indexer
    action (Just indexer) = do
            res <- runExceptT $ Storable.query indexer query
            let spentInfo row =
                    -- either both parameters are Nothing or both are defined
                    SpentInfoResult <$> row ^? urSpentSlotNo <*> row ^? urSpentTxId
            pure $ case res of
                 Right (Utxo.UtxoResult rows) ->
                     Right $ GetUtxosFromAddressResult $ rows <&> \row ->
                         AddressUtxoResult
                            (row ^. urCreationBlockHash)
                            (row ^. urCreationSlotNo)
                            (row ^. urUtxo . txIn)
                            (row ^. urUtxo . address)
                            (row ^. urUtxo . datumHash)
                            (row ^. urUtxo . datum)
                            (spentInfo row)
                 _other               ->
                     Left $ UnexpectedQueryResult query

-- | report target addresses
-- Used by JSON-RPC
reportQueryAddresses
    :: AddressUtxoIndexerEnv
    -> IO [C.Address C.ShelleyAddr]
reportQueryAddresses env = pure $ maybe [] NonEmpty.toList (env ^. addressUtxoIndexerEnvTargetAddresses)

reportBech32Addresses
    :: AddressUtxoIndexerEnv
    -> [Text]
reportBech32Addresses env =
    let addrs = maybe [] NonEmpty.toList (env ^. addressUtxoIndexerEnvTargetAddresses)
     in fmap C.serialiseAddress addrs

updateEnvState :: AddressUtxoIndexerEnv -> Utxo.UtxoIndexer -> STM ()
updateEnvState (AddressUtxoIndexerEnv _ t) =  writeTMVar t
