{-
-- | The purpose of this module is to compare the Utxos between cardano-db-sync and Marconi
Assumptions:

1. Both marconi and cardano-db-sync are synched with cardano node
2. the follwoing environemnt variales are set:
     - MARCONI_DB_DIRECTORY_PATH
     - DBSYNC_PG_URL: Postgres URL, see https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING for detail.   cardano-db-sync's postgres database details are in its repo in the file: config/secrets/postgres_password

  To run this specific test:
@
cabal test marconi-chain-index-test-compare-cardano-db-sync --flags="-ci"  --test-option=--pattern="Utxo"
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utxo where

import Control.Concurrent.Async (Concurrently (Concurrently), runConcurrently)
import Control.Concurrent.STM.TMChan (TMChan, closeTMChan, newTMChanIO, readTMChan, writeTMChan)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Word (Word64)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.FromField qualified as PG
import Database.PostgreSQL.Simple.FromRow qualified as PG
import Database.PostgreSQL.Simple.ToField qualified as PG
import Database.PostgreSQL.Simple.ToRow qualified as PG
import Database.SQLite.Simple qualified as SQLite
import Database.SQLite.Simple.ToField qualified as SQLite
import GHC.Conc (atomically, forkIO)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S
import Text.RawString.QQ (r)

import Cardano.Api qualified as C
import DBUtils qualified as UtxoDb
import Hedgehog (Property, property, withTests)
import Hedgehog qualified
import Marconi.ChainIndex.Orphans ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Utxo"
    [ testPropertyNamed
        "Compare all Utxo addresses between Marconi SQLite database and cardano-db-sync"
        "marconiUtxoAddressesAreSubsetOfDbSyncUtxosAddresses"
        marconiUtxoAddressesAreSubsetOfDbSyncUtxosAddresses
    ]

findDbSyncAddressInMarconi :: Property
findDbSyncAddressInMarconi =
  withTests 1 $
    property $ do
      (UtxoDb.DbConnection sqliteConn _) <- UtxoDb.mkDbConnection
      liftIO $ printAllAddresses sqliteConn
      Hedgehog.success

{-
-- The purpose of this test is to verify addresses in marconi SQLite unspent_transaction are in cardano-db-sync
-- Note: This test does not address marconi in-memeory utxos.
-- Relaxing this requirement has the following  pro/conss:
-- Pros:
  * cardano-node does not have to be up
  * the test is repeatable and simpler to verify as it avoids potential race conditions of running marconi and cardano-db-sync in parallel
-- Cons:
  * the test is not comprehensive
-- Upon failure, we print a list of delta-addresses, addresses that are in dbSync and not in Marconi
--
-- Assumptions:
- marconi and cardano-db-sync are synchronised with cardano node
-}
marconiUtxoAddressesAreSubsetOfDbSyncUtxosAddresses :: Property
marconiUtxoAddressesAreSubsetOfDbSyncUtxosAddresses =
  withTests 1 $
    property $ do
      dbs@(UtxoDb.DbConnection sqliteConn _) <- UtxoDb.mkDbConnection
      liftIO $ initDBs dbs

      [maxSlot] :: [Integer] <- liftIO $ SQLite.query_ sqliteConn "SELECT MAX(slotNo) FROM marconiUtxos"
      liftIO $ mkDeltaAddressesTable sqliteConn maxSlot

      [SQLite.Only deltaMarconiDbSyncCount] :: [SQLite.Only Int] <-
        liftIO $ SQLite.query_ sqliteConn "SELECT COUNT(1) FROM deltaAddresses"
      [SQLite.Only dbSyncUtxoCount] :: [SQLite.Only Int] <-
        liftIO $ SQLite.query_ sqliteConn "SELECT COUNT(1) FROM dbSyncUtxos"
      [SQLite.Only marconiUtxoCount] :: [SQLite.Only Int] <-
        liftIO $ SQLite.query_ sqliteConn "SELECT COUNT(1) FROM marconiUtxos"

      liftIO . putStrLn $
        "Address Utxo Marconi and cardano-db-sync comparason where maximum slotNo = "
          <> show maxSlot
          <> "\nnumber Utxos in marconi database: "
          <> show marconiUtxoCount
          <> "\nnumber Utxos in cardano-db-sync database: "
          <> show dbSyncUtxoCount
          <> "\ndelta addresses between marconi and cardano-db-sync: "
          <> show deltaMarconiDbSyncCount

      Hedgehog.annotate $
        "deltaMarconiDbSyncCount: "
          <> show deltaMarconiDbSyncCount
          <> "\nmarconiUtxoCount: "
          <> show marconiUtxoCount
          <> "\ndbsyncUtxoCount: "
      Hedgehog.assert $ marconiUtxoCount > 1
      Hedgehog.assert $ dbSyncUtxoCount > 1
      if deltaMarconiDbSyncCount /= 0
        then (liftIO $ printAllAddresses sqliteConn) >> Hedgehog.failure -- proof that for the slotNo range, there are equal number of Address-Utxos between db-sync and marconi
        else Hedgehog.success
  where
    mkDeltaAddressesTable :: SQLite.Connection -> Integer -> IO ()
    mkDeltaAddressesTable sqliteConn maxSlot = do
      void $ SQLite.execute_ sqliteConn "DROP TABLE IF EXISTS deltaAddresses"
      void $
        SQLite.execute
          sqliteConn
          [r|CREATE TABLE deltaAddresses AS
            SELECT marconi.address, marconi.slotNo
            FROM marconiUtxos marconi
            LEFT JOIN dbSyncUtxos dbsync
            ON (marconi.address = dbsync.address)
            WHERE dbsync.address IS NULL AND dbsync.slotNo <= ?|]
          (SQLite.Only maxSlot)

{-
initialize a SQL workflow:
  * create a postgres view of utxos that includes slot_no
  * stream the above to marconi SQLight
  * create subsets of the above db-sync originated utxos, so that we may compare marconi utxos with those of db-sync
  * create a delta table that contains utxo addresses in marconi that do not exist in db-sync
  * This test requires both marconi and db-sync have been synchronised with cardano-node
-}
initDBs :: UtxoDb.DbConnection -> IO ()
initDBs (UtxoDb.DbConnection sqliteConn pgConn) =
  let mkSQLiteMarconiUtxos :: IO ()
      mkSQLiteMarconiUtxos =
        liftIO $
          SQLite.execute_ sqliteConn "DROP TABLE IF EXISTS marconiUtxos"
            >> SQLite.execute_
              sqliteConn
              [r|CREATE TABLE IF NOT EXISTS marconiUtxos AS
          SELECT
            u.address
            , u.txId
            , u.txIx
            , u.datum
            , u.datumHash
            , u.value
            , u.inlineScript
            , u.inlineScriptHash
            , u.slotNo
            , u.blockHash
          FROM unspent_transactions u
          LEFT JOIN spent s ON
            u.txId = s.txId AND u.txIx = s.txIx
          WHERE
            s.txId IS NULL AND s.txIx IS NULL |]

      mkPGDbSyncUtxosView :: IO () -- create a dbsync view with slotNo to the utxo_view
      mkPGDbSyncUtxosView =
        void $
          PG.execute_
            pgConn
            [r|CREATE OR REPLACE VIEW utxos_v AS
              SELECT
                tx.hash txId,
                v.index txOutIndex,
                v.address_raw,
                v.value,
                block.slot_no slotNo,
                block.hash blockHash,
                block.block_no blockNo,
                v.data_hash datumHash
              FROM utxo_view as v
              INNER JOIN tx on v.tx_id = tx.id
              INNER JOIN block on block.id = tx.block_id|]

      mkSQLiteDbSyncUtxos :: IO ()
      mkSQLiteDbSyncUtxos =
        liftIO $
          SQLite.execute_ sqliteConn "DROP TABLE IF EXISTS dbSyncUtxos"
            >> SQLite.execute_
              sqliteConn
              [r|CREATE TABLE IF NOT EXISTS dbSyncUtxos
                  ( txId BLOB NOT NULL
                  , txIndex INT
                  , address BLOB NOT NULL
                  , slotNo INT
                  , blockHash BLOB
                  , blockNo INT
                  , datumHash BLOB)|]

      loadUtxosFromDbSync :: IO ()
      loadUtxosFromDbSync = do
        liftIO $
          S.mapM_
            ( SQLite.execute
                sqliteConn
                [r|INSERT INTO dbSyncUtxos
                  ( txId
                  , txIndex
                  , address
                  , slotNo
                  , blockHash
                  , blockNo
                  , datumHash) VALUES (?, ?, ?, ?, ?, ?, ?)|]
            )
            ( sourceQuery_
                pgConn
                [r|SELECT
                      v.txId
                    , v.txOutIndex
                    , v.address_raw
                    , v.slotNo
                    , v.blockHash
                    , v.blockNo
                    , v.datumHash
                   FROM utxos_v v|]
              :: S.Stream (S.Of DbSyncUtxo) IO ()
            )
   in do
        void $
          runConcurrently $
            (,,)
              <$> Concurrently mkPGDbSyncUtxosView
              <*> Concurrently mkSQLiteMarconiUtxos
              <*> Concurrently mkSQLiteDbSyncUtxos
        loadUtxosFromDbSync

-- * SQL mappings
instance PG.ToField C.EpochNo where
  toField = PG.toField . coerce @C.EpochNo @Word64

instance PG.FromField C.Lovelace where
  fromField f meta = fromIntegral @Integer <$> PG.fromField f meta

instance PG.ToRow Integer where
  toRow = PG.toRow

instance PG.FromRow Integer where
  fromRow = PG.field

instance SQLite.ToRow Word64 where
  toRow :: Word64 -> [SQLite.SQLData]
  toRow = SQLite.toRow

newtype Address = Address {unAddress :: C.AddressAny}

instance SQLite.FromRow Address where
  fromRow = Address <$> SQLite.field

instance SQLite.FromRow Word64 where
  fromRow = SQLite.fromRow

instance SQLite.FromRow Text where
  fromRow = SQLite.fromRow

instance PG.ToRow Word64 where
  toRow = PG.toRow

instance PG.FromRow Word64 where
  fromRow = PG.fromRow

instance PG.FromField (C.Hash C.BlockHeader) where
  fromField f meta =
    let cantDeserialise = PG.returnError PG.ConversionFailed f "Cannot deserialise address."
     in ( PG.fromField f meta
            >>= ( \case
                    Right a -> pure a
                    Left _ -> cantDeserialise
                )
              . C.deserialiseFromRawBytes (C.proxyToAsType Proxy)
        )

instance PG.FromField C.AddressAny where
  fromField f meta =
    let cantDeserialise = PG.returnError PG.ConversionFailed f "Cannot deserialise address."
     in ( PG.fromField f meta
            >>= ( \case
                    Right a -> pure a
                    Left _ -> cantDeserialise
                )
              . C.deserialiseFromRawBytes C.AsAddressAny
        )

instance PG.FromField C.TxId where
  fromField f meta =
    PG.fromField f meta
      >>= ( \case
              Right a -> return a
              Left err -> PG.returnError PG.ConversionFailed f $ "Can't parse C.TxId , error: " <> show err
          )
        . C.deserialiseFromRawBytes (C.proxyToAsType Proxy)

instance PG.ToField (C.Hash C.BlockHeader) where
  toField = PG.toField . C.serialiseToRawBytes

instance PG.ToField C.AddressAny where
  toField = PG.toField . C.serialiseToRawBytes

instance PG.FromField (C.Hash C.ScriptData) where
  fromField f meta =
    PG.fromField f meta
      >>= ( \case
              Right a -> return a
              Left err -> PG.returnError PG.ConversionFailed f $ "Can't parse C.AsHash C.AsScriptData , error: " <> show err
          )
        . C.deserialiseFromRawBytes (C.AsHash C.AsScriptData)

instance SQLite.FromRow DbSyncUtxo where
  fromRow =
    DbSyncUtxo
      <$> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field
      <*> SQLite.field

instance PG.FromField Word64 where
  fromField f meta = fromIntegral @Integer <$> PG.fromField f meta

instance PG.FromField C.SlotNo where
  fromField f meta = fromIntegral @Integer <$> PG.fromField f meta

instance PG.ToField C.SlotNo where
  toField = PG.toField . coerce @C.SlotNo @Word64

instance SQLite.ToRow DbSyncUtxo where
  toRow u =
    [ SQLite.toField (dbTxId u)
    , SQLite.toField (dbTxIndex u)
    , SQLite.toField (dbAddress u)
    , SQLite.toField (dbSlotNo u)
    , SQLite.toField (dbBlockHash u)
    , SQLite.toField (dbBlockNo u)
    , SQLite.toField (dbDatumHash u)
    ]

instance PG.FromRow DbSyncUtxo where
  fromRow =
    DbSyncUtxo
      <$> PG.field
      <*> PG.field
      <*> PG.field
      <*> PG.field
      <*> PG.field
      <*> PG.field
      <*> PG.field

deriving newtype instance PG.ToRow C.SlotNo

-- * create a streaming source/destination to stream a supperset of the utxo_view from cardano-db-sync to marconi-sqlite

-- | create a streaming source
mkStreamingSource
  :: (MonadIO m)
  => ((r -> IO ()) -> IO ())
  -> Stream (Of r) m ()
mkStreamingSource action =
  let chanSource
        :: MonadIO m
        => TMChan r
        -> Stream (Of r) m ()
      chanSource chan = loop
        where
          loop = do
            p <- liftIO $ atomically $ readTMChan chan
            case p of
              Just x -> S.yield x >> loop
              Nothing -> pure ()
   in do
        chan <- liftIO newTMChanIO
        _ <- liftIO $
          forkIO $ do
            action $ atomically . writeTMChan chan
            void (liftIO $ atomically $ closeTMChan chan)
        chanSource chan

-- | Stream rows from cardano-db-sync, no param substition
sourceQuery_
  :: (MonadIO m, PG.FromRow r)
  => PG.Connection
  -> PG.Query
  -> Stream (Of r) m ()
sourceQuery_ conn q = mkStreamingSource $ PG.forEach_ conn q

-- | Stream rows from cardano-db-sync, with param substition
sourceQuery
  :: (PG.ToRow params, PG.FromRow r, MonadIO m)
  => PG.Connection
  -> PG.Query
  -> params
  -> Stream (Of r) m ()
sourceQuery conn q params = mkStreamingSource $ PG.forEach conn q params

data DbSyncUtxo = DbSyncUtxo
  { dbTxId :: C.TxId
  , dbTxIndex :: Word64
  , dbAddress :: C.AddressAny
  , dbSlotNo :: C.SlotNo
  , dbBlockHash :: C.Hash C.BlockHeader
  , dbBlockNo :: Word64
  , dbDatumHash :: !(Maybe (C.Hash C.ScriptData))
  }
  deriving (Eq, Show)

-- | print the list of offending addresses. These are addresses in db-sync and not in marconi that have utxos
printAllAddresses :: SQLite.Connection -> IO ()
printAllAddresses c = do
  putStrLn "starting the test"
  addresses <-
    SQLite.query_ c "SELECT address from deltaAddresses" :: IO [Address]
  let addrs :: [Text] = C.serialiseAddress . unAddress <$> addresses
  putStrLn "/nAddresses that are in Marconi Utxo and not in dbSync utxo_view\n"
  print addrs
