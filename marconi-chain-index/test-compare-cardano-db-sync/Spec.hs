{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Slotting.Slot (WithOrigin (At, Origin))
import Control.Exception (throw)
import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Ratio (denominator, numerator)
import Data.String (fromString)
import Data.Time (nominalDiffTimeToSeconds)
import Data.Word (Word64)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.FromField qualified as PG
import Database.PostgreSQL.Simple.ToField qualified as PG
import Hedgehog ((===))
import Hedgehog qualified as H
import Marconi.ChainIndex.Error qualified as Marconi
import Marconi.ChainIndex.Indexers.EpochState qualified as EpochState
import Marconi.ChainIndex.Indexers.Utxo (BlockInfo (BlockInfo), blockInfoBlockNo)
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Node.Client.GenesisConfig qualified as GenesisConfig
import Marconi.ChainIndex.Types (epochStateDbName, utxoDbName)
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Core.Storable qualified as Storable
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Config qualified as O
import Ouroboros.Consensus.Node qualified as O
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)
import Text.Read (readMaybe)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Marconi to cardano-db-sync comparisons"
    [ testPropertyNamed
        "Validate info in marconi's last synced block by comparing with cardano-db-sync"
        "propCurrentSyncedBlockInfo"
        propCurrentSyncedBlockInfo
    , testPropertyNamed
        "Compare all epoch nonces between Marconi and cardano-db-sync"
        "propEpochNonce"
        propEpochNonce
    , testPropertyNamed
        "Compare all epoch stakepool sizes between Marconi and cardano-db-sync"
        "propEpochStakepoolSize"
        propEpochStakepoolSize
    ]

{- | Query the BlockInfo of the latest synced block in Marconi. Then, we compare this BlockInfo with
    the corresponding BlockInfo in cardano-db-sync.
-}
propCurrentSyncedBlockInfo :: H.Property
propCurrentSyncedBlockInfo = H.withTests 1 $ H.property $ do
  indexer <- openUtxoIndexer
  res <- liftIO $ queryCurrentSyncedBlockInfo indexer
  latestBlockInfo <- case res of
    Origin -> do
      fail "Last synced block is genesis. Did you synchronize Marconi?"
    At blockInfo ->
      pure blockInfo

  H.footnote $ "Comparing " <> show latestBlockInfo

  conn <- getDbSyncPgConnection
  let (C.BlockNo latestBlockNo) = latestBlockInfo ^. blockInfoBlockNo
  dbSyncBlockInfos <-
    fmap listToMaybe $
      liftIO $
        PG.query_ conn $
          "select slot_no, hash, block_no, extract(epoch from time), epoch_no from block where block_no = "
            <> fromString (show latestBlockNo)
            <> " limit 1"
  case dbSyncBlockInfos of
    Nothing -> fail "Marconi blockNo not found in db-sync. Did you synchronize cardano-db-sync?"
    Just (slotNo :: Int, blockHeaderHash, blockNo :: Int, time :: Double, epochNo :: Int) -> do
      let (blockTimeStampSeconds, _) = properFraction $ nominalDiffTimeToSeconds $ realToFrac time
      let dbSyncBlockInfo =
            BlockInfo
              (C.SlotNo $ fromIntegral slotNo)
              blockHeaderHash
              (C.BlockNo $ fromIntegral blockNo)
              blockTimeStampSeconds
              (C.EpochNo $ fromIntegral epochNo)
      latestBlockInfo === dbSyncBlockInfo
  where
    queryCurrentSyncedBlockInfo :: Storable.State Utxo.UtxoHandle -> IO (WithOrigin Utxo.BlockInfo)
    queryCurrentSyncedBlockInfo indexer = do
      res <- throwIndexerError $ Storable.query indexer Utxo.LastSyncedBlockInfoQuery
      case res of
        Utxo.LastSyncedBlockInfoResult r -> return r
        _ -> fail "Expected LastSyncedBlockInfoResult, but got another result type"

openUtxoIndexer :: H.PropertyT IO (Storable.State Utxo.UtxoHandle)
openUtxoIndexer = do
  socketPath <- envOrFail "CARDANO_NODE_SOCKET_PATH"
  dbDir <- envOrFail "MARCONI_DB_DIRECTORY_PATH"
  networkMagicStr <- envOrFail "NETWORK_MAGIC"
  networkMagic <- case networkMagicStr of
    "mainnet" -> return C.Mainnet
    _ -> case readMaybe networkMagicStr of
      Nothing -> fail $ "Can't parse network magic: " <> networkMagicStr
      Just word32 -> return $ C.Testnet $ C.NetworkMagic word32
  let dbPath = dbDir </> utxoDbName
  liftIO $ do
    securityParam <- throwIndexerError $ Utils.querySecurityParam networkMagic socketPath
    throwIndexerError $ Utxo.open dbPath (fromIntegral securityParam) False

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
  dbSyncEpochNonces <-
    liftIO $ PG.query_ conn "select epoch_no, nonce from epoch_param order by epoch_no ASC"
  forM_ dbSyncEpochNonces $ \(epochNo, dbSyncNonce) -> do
    res <- liftIO $ queryIndexerEpochNonce epochNo indexer
    case res of
      Just indexerNonce -> do
        H.footnote $ "Comparing epoch " <> show epochNo
        dbSyncNonce === indexerNonce
      Nothing ->
        fail $ "EpochNo not found in indexer. Did you synchronize Marconi? Epoch no: " <> show epochNo

queryIndexerEpochNonce
  :: C.EpochNo -> Storable.State EpochState.EpochStateHandle -> IO (Maybe Ledger.Nonce)
queryIndexerEpochNonce epochNo indexer = do
  let query = EpochState.NonceByEpochNoQuery epochNo
  res' <- throwIndexerError $ Storable.query indexer query
  case res' of
    EpochState.NonceByEpochNoResult res -> return $ EpochState.epochNonceRowNonce <$> res
    _ -> return Nothing

{- | Connect to cardano-db-sync's postgres instance, get minimum and
 maximum epoch no from epoch_stake table, then compare random 10
 epoch stakepool sizes to what we have in the indexer.
-}
propEpochStakepoolSize :: H.Property
propEpochStakepoolSize = H.withTests 1 $ H.property $ do
  conn <- getDbSyncPgConnection
  indexer <- openEpochStateIndexer
  [(minEpochNo :: C.EpochNo, maxEpochNo :: C.EpochNo)] <-
    liftIO $ PG.query_ conn "SELECT min(epoch_no), max(epoch_no) FROM epoch_stake"
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
      | otherwise =
          error "getEpochStakepoolSizes: This should never happen, lovelace can't be fractional."

indexerStakepoolSizes
  :: C.EpochNo -> Storable.State EpochState.EpochStateHandle -> IO (Map.Map C.PoolId C.Lovelace)
indexerStakepoolSizes epochNo indexer = do
  let query = EpochState.ActiveSDDByEpochNoQuery epochNo
  result <- throwIndexerError $ Storable.query indexer query
  case result of
    EpochState.ActiveSDDByEpochNoResult rows -> return $ Map.fromList $ map toPair rows
    _ -> return undefined
  where
    toPair row = (EpochState.epochSDDRowPoolId row, EpochState.epochSDDRowLovelace row)

openEpochStateIndexer :: H.PropertyT IO (Storable.State EpochState.EpochStateHandle)
openEpochStateIndexer = do
  socketPath <- envOrFail "CARDANO_NODE_SOCKET_PATH"
  nodeConfigPath <- envOrFail "CARDANO_NODE_CONFIG_PATH"
  dbDir <- envOrFail "MARCONI_DB_DIRECTORY_PATH"
  networkMagicStr <- envOrFail "NETWORK_MAGIC"
  networkMagic <- case networkMagicStr of
    "mainnet" -> return C.Mainnet
    _ -> case readMaybe networkMagicStr of
      Nothing -> fail $ "Can't parse network magic: " <> networkMagicStr
      Just word32 -> return $ C.Testnet $ C.NetworkMagic word32
  liftIO $ do
    securityParam <- throwIndexerError $ Utils.querySecurityParam networkMagic socketPath
    topLevelConfig <- topLevelConfigFromNodeConfig nodeConfigPath
    let dbPath = dbDir </> epochStateDbName
        ledgerStateDirPath = dbDir </> "ledgerStates"
    throwIndexerError $ EpochState.open topLevelConfig dbPath ledgerStateDirPath securityParam

throwIndexerError :: (Monad m) => ExceptT Marconi.IndexerError m a -> m a
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
  nodeConfigE <-
    runExceptT $ GenesisConfig.readNetworkConfig (GenesisConfig.NetworkConfigFile nodeConfigPath)
  nodeConfig <- either (error . show) pure nodeConfigE
  genesisConfigE <- runExceptT $ GenesisConfig.readCardanoGenesisConfig nodeConfig
  genesisConfig <- either (error . show . GenesisConfig.renderGenesisConfigError) pure genesisConfigE
  return $ O.pInfoConfig (GenesisConfig.mkProtocolInfoCardano genesisConfig)

-- * FromField & ToField instances

instance PG.FromField C.EpochNo where
  fromField f meta = fromIntegral @Integer <$> PG.fromField f meta

instance PG.ToField C.EpochNo where
  toField = PG.toField . coerce @C.EpochNo @Word64

instance PG.FromField C.Lovelace where
  fromField f meta = fromIntegral @Integer <$> PG.fromField f meta

instance PG.FromField Ledger.Nonce where
  fromField f meta = do
    maybeNonce <- fmap bsToMaybeNonce $ PG.fromField f meta
    case maybeNonce of
      Just a -> pure a
      Nothing -> PG.returnError PG.ConversionFailed f "Can't parse Nonce"
    where
      bsToMaybeNonce :: BS.ByteString -> Maybe Ledger.Nonce
      bsToMaybeNonce bs = Ledger.Nonce <$> Crypto.hashFromBytes bs

instance PG.FromField C.PoolId where
  fromField f meta = do
    maybePoolId <- C.deserialiseFromRawBytes (C.AsHash C.AsStakePoolKey) <$> PG.fromField f meta
    case maybePoolId of
      Right a -> pure a
      Left err -> PG.returnError PG.ConversionFailed f $ "Can't parse PoolId, error: " <> show err

instance PG.FromField (C.Hash C.BlockHeader) where
  fromField f meta = do
    maybeBhh <-
      C.deserialiseFromRawBytes (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader))
        <$> PG.fromField f meta
    case maybeBhh of
      Right a -> pure a
      Left err -> PG.returnError PG.ConversionFailed f $ "Can't parse BlockHeaderHash, error: " <> show err

deriving newtype instance Real C.EpochNo
deriving newtype instance Integral C.EpochNo
