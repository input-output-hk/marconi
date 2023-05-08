{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-orphans    #-}

{-| Run the Marconi and cardano-db-sync comparison by:

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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Data.Map.Strict qualified as Map
import Data.Ratio (denominator, numerator)
import Data.Word (Word64)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.FromField qualified as PG
import Database.PostgreSQL.Simple.ToField qualified as PG
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Read (readMaybe)

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.Shelley.API qualified as Ledger
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Config qualified as O
import Ouroboros.Consensus.Node qualified as O

import Marconi.ChainIndex.Error qualified as Marconi
import Marconi.ChainIndex.Indexers.EpochState qualified as EpochState
import Marconi.ChainIndex.Node.Client.GenesisConfig qualified as GenesisConfig
import Marconi.ChainIndex.Types (epochStateDbName)
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Core.Storable qualified as Storable

import Hedgehog ((===))
import Hedgehog qualified as H
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Marconi to cardano-db-sync comparisons"
  [ testPropertyNamed
      "Compare all epoch nonces between Marconi and cardano-db-sync"
      "propEpochNonce" propEpochNonce
  , testPropertyNamed
      "Compare all epoch stakepool sizes between Marconi and cardano-db-sync"
      "propEpochStakepoolSize" propEpochStakepoolSize
  ]

-- | Connect to cardano-db-sync's postgres instance, get minimum and
-- maximum epoch no from epoch_stake table, then compare random 10
-- epoch stakepool sizes to what we have in the indexer.
propEpochStakepoolSize :: H.Property
propEpochStakepoolSize = H.withTests 1 $ H.property $ do
  conn <- getDbSyncPgConnection
  indexer <- openEpochStateIndexer
  [(minEpochNo :: C.EpochNo, maxEpochNo :: C.EpochNo)] <- liftIO $ PG.query_ conn "SELECT min(epoch_no), max(epoch_no) FROM epoch_stake"
  let compareEpoch epochNo = do
        dbSyncResult <- liftIO $ dbSyncStakepoolSizes conn epochNo
        marconiResult <- liftIO $ indexerStakepoolSizes epochNo indexer
        liftIO $ putStr
          $ "\nComparing epoch " <> show epochNo
          <> ", number of stakepools in epoch " <> show (Map.size dbSyncResult)
        dbSyncResult === marconiResult
  liftIO $ putStrLn
     $ "Min and max epoch in cardano-db-sync postgres: "
    <> show (coerce @_ @Word64 minEpochNo) <> " and " <> show (coerce @_ @Word64 maxEpochNo) <> ")"
  forM_ [minEpochNo .. maxEpochNo] compareEpoch

dbSyncStakepoolSizes :: PG.Connection -> C.EpochNo -> IO (Map.Map C.PoolId C.Lovelace)
dbSyncStakepoolSizes conn epochNo = do
  dbSyncRows :: [(C.PoolId, Rational)] <- liftIO $ PG.query conn
    "   SELECT ph.hash_raw AS pool_hash             \
    \        , sum(amount) AS sum_amount            \
    \     FROM epoch_stake es                       \
    \     JOIN pool_hash   ph ON es.pool_id = ph.id \
    \    WHERE epoch_no = ?                         \
    \ GROUP BY epoch_no, pool_hash                  \
    \ ORDER BY sum_amount desc                      "
    $ PG.Only epochNo
  return $ Map.fromList $ map (\(a, b) -> (a, rationalToLovelace b)) dbSyncRows
  where
    rationalToLovelace :: Rational -> C.Lovelace
    rationalToLovelace n | 1 <- denominator n = fromIntegral $ numerator n
                   | otherwise = error "getEpochStakepoolSizes: This should never happen, lovelace can't be fractional."

indexerStakepoolSizes :: C.EpochNo -> Storable.State EpochState.EpochStateHandle -> IO (Map.Map C.PoolId C.Lovelace)
indexerStakepoolSizes epochNo indexer = do
  let query = EpochState.SDDByEpochNoQuery epochNo
  result <- throwIndexerError $ Storable.queryStorage [] (indexer ^. Storable.handle) query
  case result of
    EpochState.SDDByEpochNoResult rows -> return $ Map.fromList $ map toPair rows
    _                                  -> return undefined
  where
    toPair row = (EpochState.epochSDDRowPoolId row, EpochState.epochSDDRowLovelace row)


-- | Connect to cardano-db-sync's postgres instance, get all (EpochNo,
-- Nonce) tuples, query and compare all of these to the one found in
-- Marconi.
--
-- As the number of epochs is low (406 at the time of writing), then
-- all nonces found in postgres are compared.
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
      Nothing           ->
        fail $ "Epoch not found in indexer, is it synchronised? Epoch no: " <> show epochNo

queryIndexerEpochNonce :: C.EpochNo -> Storable.State EpochState.EpochStateHandle -> IO (Maybe Ledger.Nonce)
queryIndexerEpochNonce epochNo indexer = do
  let query = EpochState.NonceByEpochNoQuery epochNo
  res' <- throwIndexerError $ Storable.queryStorage [] (indexer ^. Storable.handle) query
  case res' of
    EpochState.NonceByEpochNoResult res -> return $ EpochState.epochNonceRowNonce <$> res
    _                                   -> return Nothing

openEpochStateIndexer :: H.PropertyT IO (Storable.State EpochState.EpochStateHandle)
openEpochStateIndexer = do
  socketPath <- envOrFail "CARDANO_NODE_SOCKET_PATH"
  nodeConfigPath <- envOrFail "CARDANO_NODE_CONFIG_PATH"
  dbDir <- envOrFail "MARCONI_DB_DIRECTORY_PATH"
  networkMagicStr <- envOrFail "NETWORK_MAGIC"
  networkMagic <- case networkMagicStr of
    "mainnet" -> return C.Mainnet
    _ -> case readMaybe networkMagicStr of
      Nothing     -> fail $ "Can't parse network magic: " <> networkMagicStr
      Just word32 -> return $ C.Testnet $ C.NetworkMagic word32
  liftIO $ do
    securityParam <- throwIndexerError $ Utils.querySecurityParamEra C.BabbageEraInCardanoMode networkMagic socketPath
    topLevelConfig <- topLevelConfigFromNodeConfig nodeConfigPath
    let dbPath = dbDir </> epochStateDbName
        ledgerStateDirPath = dbDir </> "ledgerStates"
    throwIndexerError $ EpochState.open topLevelConfig dbPath ledgerStateDirPath securityParam

throwIndexerError :: Monad m => ExceptT Marconi.IndexerError m a -> m a
throwIndexerError action = either throw return =<< runExceptT action

-- | Connect to cardano-db-sync postgres with password from
-- DBSYNC_PGPASSWORD env variable.
getDbSyncPgConnection :: H.PropertyT IO PG.Connection
getDbSyncPgConnection = do
  pgPassword <- envOrFail "DBSYNC_PGPASSWORD"
  liftIO $ PG.connect $ PG.ConnectInfo
        { PG.connectHost = "localhost"
        , PG.connectPort = 5432
        , PG.connectUser = "postgres"
        , PG.connectPassword = pgPassword
        , PG.connectDatabase = "cexplorer"
        }

-- | Get string from the environment or fail test with instruction.
envOrFail :: String -> H.PropertyT IO String
envOrFail str = liftIO (lookupEnv str) >>= \case
  Just v  -> return v
  Nothing -> fail $ str <> " environment variable not set!"

topLevelConfigFromNodeConfig
  :: FilePath  -> IO (O.TopLevelConfig (O.HardForkBlock (O.CardanoEras O.StandardCrypto)))
topLevelConfigFromNodeConfig nodeConfigPath = do
  nodeConfigE <- runExceptT $ GenesisConfig.readNetworkConfig (GenesisConfig.NetworkConfigFile nodeConfigPath)
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
  fromField f meta = bsToMaybeNonce <$> PG.fromField f meta >>= \case
    Just a -> return a
    _      -> PG.returnError PG.ConversionFailed f "Can't parse Nonce"
    where
    bsToMaybeNonce :: BS.ByteString -> Maybe Ledger.Nonce
    bsToMaybeNonce bs = Ledger.Nonce <$> Crypto.hashFromBytes bs

instance PG.FromField C.PoolId where
  fromField f meta = C.deserialiseFromRawBytes (C.AsHash C.AsStakePoolKey) <$> PG.fromField f meta >>= \case
    Right a  -> return a
    Left err -> PG.returnError PG.ConversionFailed f $ "Can't parse PoolId, error: " <> show err

deriving newtype instance Real C.EpochNo
deriving newtype instance Integral C.EpochNo
