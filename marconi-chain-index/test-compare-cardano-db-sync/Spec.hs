{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Run the Marconi and cardano-db-sync comparison by:

1. Sync up cardano-db-sync

2. Run the EpochState indexer up to sync, possibly using the
   cardano-node from the cardano-db-sync docker

3. Run this test by setting the env variables:

     - CARDANO_NODE_SOCKET_PATH
     - CARDANO_NODE_CONFIG_PATH
     - MARCONI_DB_DIRECTORY_PATH
     - DBSYNC_PGPASSWORD: The default password for cardano-db-sync's postgres database is in its repo in the file: config/secrets/postgres_password
     - NETWORK_MAGIC: "mainnet" or number

   And then run the command:

@
   cabal test marconi-chain-index-test-compare-cardano-db-sync --flag '-ci'
@

   The --flag '-ci' is there to unset the "ci" cabal flag which is on
   by default as we don't want to run it on CI.
-}
module Main where

import Control.Exception (throw)
import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Data.List (intercalate, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Proxy (Proxy (Proxy))
import Data.Ratio (denominator, numerator)
import Data.Scientific (floatingOrInteger)
import Data.String (fromString)
import Data.Word (Word64)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.FromField qualified as PG
import Database.PostgreSQL.Simple.FromRow qualified as PG
import Database.PostgreSQL.Simple.ToField qualified as PG
import Database.SQLite.Simple qualified as SQL
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.RawString.QQ (r)
import Text.Read (readMaybe)

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Shelley.API qualified as Ledger
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Config qualified as O
import Ouroboros.Consensus.Node qualified as O

import Marconi.ChainIndex.Error qualified as Marconi
import Marconi.ChainIndex.Indexers.EpochState qualified as EpochState
import Marconi.ChainIndex.Indexers.MintBurn qualified as MintBurn
import Marconi.ChainIndex.Node.Client.GenesisConfig qualified as GenesisConfig
import Marconi.ChainIndex.Types (SecurityParam, TxIndexInBlock, epochStateDbName, mintBurnDbName)
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Core.Storable qualified as Storable

import Hedgehog ((===))
import Hedgehog qualified as H
import Hedgehog.Extras.Test.Base qualified as H
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Crypto.Hash qualified as Crypto

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Marconi to cardano-db-sync comparisons"
    [ testPropertyNamed
        "Compare all epoch nonces between Marconi and cardano-db-sync"
        "propEpochNonce"
        propEpochNonce
    , testPropertyNamed
        "Compare all epoch stakepool sizes between Marconi and cardano-db-sync"
        "propEpochStakepoolSize"
        propEpochStakepoolSize
    , testPropertyNamed
        "Compare all burn events from Marconi's MintBurn indexer to cardano-db-sync"
        "propMintBurn"
        propMintBurn
    ]

-- * Epoch nonce

{- | Connect to cardano-db-sync's postgres instance, get all (EpochNo,
 Nonce) tuples, query and compare all of these to the one found in
 Marconi.

 As the number of epochs is low (406 at the time of writing), then
 all nonces found in postgres are compared.
-}
propEpochNonce :: H.Property
propEpochNonce = H.withTests 1 $ H.property $ do
  indexer <- openEpochStateIndexer
  conn <- getDbSyncPgConnection
  dbSyncEpochNonces <- liftIO $ PG.query_ conn "select epoch_no, nonce from epoch_param order by epoch_no ASC"
  forM_ dbSyncEpochNonces $ \(epochNo, dbSyncNonce) -> do
    res <- liftIO $ queryIndexerEpochNonce epochNo indexer
    case res of
      Just indexerNonce -> do
        H.footnote $ "Comparing epoch " <> show epochNo
        dbSyncNonce === indexerNonce
      Nothing ->
        fail $ "Epoch not found in indexer, is it synchronised? Epoch no: " <> show epochNo

queryIndexerEpochNonce :: C.EpochNo -> Storable.State EpochState.EpochStateHandle -> IO (Maybe Ledger.Nonce)
queryIndexerEpochNonce epochNo indexer = do
  let query = EpochState.NonceByEpochNoQuery epochNo
  res' <- throwIndexerError $ Storable.query indexer query
  case res' of
    EpochState.NonceByEpochNoResult res -> return $ EpochState.epochNonceRowNonce <$> res
    _ -> return Nothing

indexerStakepoolSizes :: C.EpochNo -> Storable.State EpochState.EpochStateHandle -> IO (Map.Map C.PoolId C.Lovelace)
indexerStakepoolSizes epochNo indexer = do
  let query = EpochState.ActiveSDDByEpochNoQuery epochNo
  result <- throwIndexerError $ Storable.query indexer query
  case result of
    EpochState.ActiveSDDByEpochNoResult rows -> return $ Map.fromList $ map toPair rows
    _ -> return undefined
  where
    toPair row = (EpochState.epochSDDRowPoolId row, EpochState.epochSDDRowLovelace row)

-- * SDD

{- | Connect to cardano-db-sync's postgres instance, get minimum and
 maximum epoch no from epoch_stake table, then compare random 10
 epoch stakepool sizes to what we have in the indexer.
-}
propEpochStakepoolSize :: H.Property
propEpochStakepoolSize = H.withTests 1 $ H.property $ do
  conn <- getDbSyncPgConnection
  indexer <- openEpochStateIndexer
  [(minEpochNo :: C.EpochNo, maxEpochNo :: C.EpochNo)] <- liftIO $ PG.query_ conn "SELECT min(epoch_no), max(epoch_no) FROM epoch_stake"
  let compareEpoch epochNo = do
        dbSyncResult <- liftIO $ dbSyncStakepoolSizes conn epochNo
        marconiResult <- liftIO $ indexerStakepoolSizes epochNo indexer
        H.footnote $
          "Comparing epoch "
            <> show epochNo
            <> ", number of stakepools in epoch "
            <> show (Map.size dbSyncResult)
        dbSyncResult === marconiResult
  H.footnote $
    "Min and max epoch in cardano-db-sync postgres: "
      <> show (coerce @_ @Word64 minEpochNo)
      <> " and "
      <> show (coerce @_ @Word64 maxEpochNo)
      <> ")"
  -- We do '+1' because we are interested in the *active* SDD per epoch, whereas db-sync indexes the
  -- 'set' stake snapshot per epoch.
  forM_ [minEpochNo + 1 .. maxEpochNo + 1] compareEpoch

dbSyncStakepoolSizes :: PG.Connection -> C.EpochNo -> IO (Map.Map C.PoolId C.Lovelace)
dbSyncStakepoolSizes conn epochNo = do
  dbSyncRows :: [(C.PoolId, Rational)] <-
    liftIO
      $ PG.query
        conn
        "   SELECT ph.hash_raw AS pool_hash             \
        \        , sum(amount) AS sum_amount            \
        \     FROM epoch_stake es                       \
        \     JOIN pool_hash ph ON es.pool_id = ph.id \
        \    WHERE epoch_no = ?                         \
        \ GROUP BY epoch_no, pool_hash                  \
        \ ORDER BY sum_amount desc                      "
      -- We do that for the same reason as above. The indexer query returns the *active* SDD for epoch
      -- 'n', so we need to compare it with the db-sync SDD of epoch 'n - 1'.
      $ PG.Only (epochNo - 1)
  return $ Map.fromList $ map (\(a, b) -> (a, rationalToLovelace b)) dbSyncRows
  where
    rationalToLovelace :: Rational -> C.Lovelace
    rationalToLovelace n
      | 1 <- denominator n = fromIntegral $ numerator n
      | otherwise = error "getEpochStakepoolSizes: This should never happen, lovelace can't be fractional."

openEpochStateIndexer :: H.PropertyT IO (Storable.State EpochState.EpochStateHandle)
openEpochStateIndexer = do
  nodeConfigPath <- envOrFail "CARDANO_NODE_CONFIG_PATH"
  securityParam <- getSecurityParam
  (dbDir, dbPath) <- getDbPath epochStateDbName
  liftIO $ do
    topLevelConfig <- topLevelConfigFromNodeConfig nodeConfigPath
    let ledgerStateDirPath = dbDir </> "ledgerStates"
    throwIndexerError $ EpochState.open topLevelConfig dbPath ledgerStateDirPath securityParam

-- * MintBurn

propMintBurn :: H.Property
propMintBurn = H.withTests 1 $ H.property $ do
  -- Open the indexer
  (_, dbPath) <- getDbPath mintBurnDbName
  securityParam <- getSecurityParam
  indexer <- liftIO $ throwIndexerError $ MintBurn.open dbPath securityParam
  let sqliteCon = MintBurn.sqlConnection $ indexer ^. Storable.handle

  -- Connect to dbsync's Postgres
  conn <- getDbSyncPgConnection

  -- Get a random slot from what Marconi has indexed
  slotNo <- getMarconiRandomSlot sqliteCon
  -- TODO: The following is an example slot that fails the test
  -- let slotNo = 23175533

  let compareTxMintRows :: H.PropertyT IO ()
      compareTxMintRows = do
        marconi :: [MintBurn.TxMintRow] <- liftIO $ MintBurn.queryDbTxMintRowsAtSlot sqliteCon slotNo
        dbsync :: [MintBurn.TxMintRow] <- liftIO $ PG.query_ conn $ dbsyncQueryTxMintRowsAtSlot slotNo
        let msg =
              show slotNo
                <> "\nNumber of redeemers: "
                <> (show $ length $ filter (isJust . MintBurn._txMintRowRedeemer) marconi)
        liftIO $ putStrLn msg
        H.footnote msg
        sort marconi === sort dbsync -- TODO: this is where the marconi and dbsync results are compared
  let compareCounts :: H.PropertyT IO ()
      compareCounts = do
        [SQL.Only marconi] <-
          liftIO
            $ SQL.query
              sqliteCon
              "SELECT count(*) FROM minting_policy_events WHERE slotNo = ?"
            $ SQL.Only slotNo
        [PG.Only dbsync] <-
          liftIO $
            PG.query_ conn $
              fromString $
                dbsyncQueryTxMintRows ["count(*)"]
                  <> " WHERE block.slot_no = "
                  <> show (coerce @C.SlotNo @Word64 slotNo)

        H.footnote $ show slotNo <> "\nCounts, marconi: " <> show marconi <> ", dbsync " <> show dbsync
        (marconi :: Int) === (dbsync :: Int)

  -- Compare
  compareCounts
  compareTxMintRows
  where
    getMarconiRandomSlot :: (H.MonadTest m, MonadIO m) => SQL.Connection -> m C.SlotNo
    getMarconiRandomSlot con = do
      SQL.Only slotNo <- H.headM =<< liftIO (SQL.query_ con "SELECT slotNo FROM minting_policy_events ORDER BY RANDOM() LIMIT 1")
      return slotNo

    dbsyncQueryTxMintRowsAtSlot :: C.SlotNo -> PG.Query
    dbsyncQueryTxMintRowsAtSlot slotNo =
      fromString $
        dbsyncQueryTxMintRows fields
          <> " WHERE block.slot_no = "
          <> show (coerce @C.SlotNo @Word64 slotNo)
      where
        fields :: [String]
        fields =
          [ "block.slot_no"
          , "block.hash"
          , "block.block_no"
          , "tx.block_index"
          , "tx.hash"
          , "multi_asset.policy"
          , "multi_asset.name"
          , "ma_tx_mint.quantity"
          , "redeemer_join.hash"
          , "redeemer_join.bytes"
          ]

    dbsyncQueryTxMintRows :: [String] -> String
    dbsyncQueryTxMintRows select =
      "SELECT "
        <> intercalate ", " select
        <> [r|
                 FROM block
                 JOIN tx          ON block.id = tx.block_id
                 JOIN ma_tx_mint  ON tx.id = ma_tx_mint.tx_id
                 JOIN multi_asset ON ma_tx_mint.ident = multi_asset.id

            LEFT JOIN
              ( SELECT DISTINCT redeemer.tx_id, redeemer_data.hash, redeemer_data.bytes
                  FROM redeemer
                  JOIN redeemer_data ON redeemer_data.id = redeemer.redeemer_data_id
                 WHERE redeemer.purpose = 'mint'
              ) redeemer_join ON redeemer_join.tx_id = tx.id
          |]

-- * Helpers

getDbPath :: FilePath -> H.PropertyT IO (FilePath, FilePath)
getDbPath dbName = do
  dbDir <- envOrFail "MARCONI_DB_DIRECTORY_PATH"
  return $ (dbDir, dbDir </> dbName)

getSecurityParam :: H.PropertyT IO SecurityParam
getSecurityParam = do
  socketPath <- envOrFail "CARDANO_NODE_SOCKET_PATH"
  networkMagicStr <- envOrFail "NETWORK_MAGIC"
  networkMagic <- case networkMagicStr of
    "mainnet" -> return C.Mainnet
    _ -> case readMaybe networkMagicStr of
      Nothing -> fail $ "Can't parse network magic: " <> networkMagicStr
      Just word32 -> return $ C.Testnet $ C.NetworkMagic word32
  liftIO $ throwIndexerError $ Utils.querySecurityParam networkMagic socketPath

throwIndexerError :: Monad m => ExceptT Marconi.IndexerError m a -> m a
throwIndexerError action = either throw return =<< runExceptT action

{- | Connect to cardano-db-sync postgres with password from
 DBSYNC_PGPASSWORD env variable.
-}
getDbSyncPgConnection :: H.PropertyT IO PG.Connection
getDbSyncPgConnection = do
  pgPassword <- envOrFail "DBSYNC_PGPASSWORD"
  liftIO $
    PG.connect $
      PG.ConnectInfo
        { PG.connectHost = "localhost"
        , PG.connectPort = 5432
        , PG.connectUser = "postgres"
        , PG.connectPassword = pgPassword
        , PG.connectDatabase = "cexplorer"
        }

-- | Get string from the environment or fail test with instruction.
envOrFail :: String -> H.PropertyT IO String
envOrFail str =
  liftIO (lookupEnv str) >>= \case
    Just v -> return v
    Nothing -> fail $ str <> " environment variable not set!"

topLevelConfigFromNodeConfig
  :: FilePath -> IO (O.TopLevelConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto)))
topLevelConfigFromNodeConfig nodeConfigPath = do
  nodeConfigE <- runExceptT $ GenesisConfig.readNetworkConfig (GenesisConfig.NetworkConfigFile nodeConfigPath)
  nodeConfig <- either (error . show) pure nodeConfigE
  genesisConfigE <- runExceptT $ GenesisConfig.readCardanoGenesisConfig nodeConfig
  genesisConfig <- either (error . show . GenesisConfig.renderGenesisConfigError) pure genesisConfigE
  return $ O.pInfoConfig (GenesisConfig.mkProtocolInfoCardano genesisConfig)

-- * Postgres FromField & ToField instances

deriving newtype instance Real C.EpochNo
deriving newtype instance Integral C.EpochNo

instance PG.FromField C.EpochNo where
  fromField f meta = fromIntegral @Integer <$> PG.fromField f meta

instance PG.ToField C.EpochNo where
  toField = PG.toField . coerce @C.EpochNo @Word64

instance PG.FromField C.Lovelace where
  fromField f meta = fromIntegral @Integer <$> PG.fromField f meta

instance PG.FromField Ledger.Nonce where
  fromField f meta =
    bsToMaybeNonce <$> PG.fromField f meta >>= \case
      Just a -> return a
      _ -> PG.returnError PG.ConversionFailed f "Can't parse Nonce"
    where
      bsToMaybeNonce :: BS.ByteString -> Maybe Ledger.Nonce
      bsToMaybeNonce bs = Ledger.Nonce <$> Crypto.hashFromBytes bs

instance PG.FromField C.PoolId where
  fromField f meta =
    C.deserialiseFromRawBytes (C.AsHash C.AsStakePoolKey) <$> PG.fromField f meta >>= \case
      Right a -> return a
      Left err -> PG.returnError PG.ConversionFailed f $ "Can't parse PoolId, error: " <> show err

deriving newtype instance Real C.SlotNo
deriving newtype instance Integral C.SlotNo
instance PG.FromField C.SlotNo where
  fromField f meta = fromIntegral @Integer <$> PG.fromField f meta

deriving newtype instance Real C.BlockNo
deriving newtype instance Integral C.BlockNo
instance PG.FromField C.BlockNo where
  fromField f meta = fromIntegral @Integer <$> PG.fromField f meta

instance PG.FromField TxIndexInBlock where
  fromField f meta = fromIntegral @Integer <$> PG.fromField f meta

instance PG.FromField C.Quantity where
  fromField f meta =
    floatingOrInteger <$> PG.fromField f meta >>= \case
      Left (d :: Double) -> PG.returnError PG.ConversionFailed f $ "Can't parse Quantity, is: " <> show d
      Right w -> pure $ C.Quantity w

instance PG.FromField C.TxId where
  fromField f meta =
    C.deserialiseFromRawBytes C.AsTxId <$> PG.fromField f meta >>= \case
      Right a -> return a
      Left err -> PG.returnError PG.ConversionFailed f $ "Can't parse TxId, error: " <> show err

instance PG.FromField C.PolicyId where
  fromField f meta =
    C.deserialiseFromRawBytes C.AsPolicyId <$> PG.fromField f meta >>= \case
      Right a -> return a
      Left err -> PG.returnError PG.ConversionFailed f $ "Can't parse PolicyId, error: " <> show err

instance PG.FromField (C.Hash C.BlockHeader) where
  fromField f meta =
    C.deserialiseFromRawBytes (C.proxyToAsType Proxy) <$> PG.fromField f meta >>= \case
      Right a -> return a
      Left err -> PG.returnError PG.ConversionFailed f $ "Can't parse Hash BlockHeader, error: " <> show err

instance PG.FromField C.AssetName where
  fromField f meta =
    C.deserialiseFromRawBytes (C.proxyToAsType Proxy) <$> PG.fromField f meta >>= \case
      Right a -> return a
      Left err -> PG.returnError PG.ConversionFailed f $ "Can't parse AssetName, error: " <> show err

instance PG.FromField C.ScriptData where
  fromField f meta =
    C.deserialiseFromCBOR C.AsScriptData <$> PG.fromField f meta >>= \case
      Right a -> return a
      Left err -> PG.returnError PG.ConversionFailed f $ "Can't parse ScriptData, error: " <> show err

instance PG.FromField (C.Hash C.ScriptData) where
  fromField f meta =
    C.deserialiseFromRawBytes (C.AsHash C.AsScriptData)
      <$> PG.fromField f meta
      >>= \case
        Right a -> return a
        Left err -> PG.returnError PG.ConversionFailed f $ "Can't parse Hash ScriptData, error: " <> show err

instance PG.FromRow MintBurn.TxMintRow where
  fromRow =
    MintBurn.TxMintRow
      <$> PG.field @C.SlotNo
      <*> PG.field @(C.Hash C.BlockHeader)
      <*> PG.field @C.BlockNo
      <*> PG.field @TxIndexInBlock
      <*> PG.field @C.TxId
      <*> PG.field @C.PolicyId
      <*> PG.field @C.AssetName
      <*> PG.field @C.Quantity
      -- TODO: This is where we parse redeemers from dbsync postgres
      <*> do
        maybeRedeemerHash :: Maybe (C.Hash C.ScriptData) <- PG.field
        maybeRedeemerData :: Maybe C.ScriptData <- PG.field
        pure $
          if
              | Just redeemerData <- maybeRedeemerData
              , Just redeemerHash <- maybeRedeemerHash ->
                  Just $
                    MintBurn.MintAssetRedeemer
                      redeemerData
                      redeemerHash
              | otherwise ->
                  Nothing
