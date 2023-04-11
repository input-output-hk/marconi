{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module CompareToDbSync where

import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple qualified as PG
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import Cardano.Api qualified as C
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.Shelley.API qualified as Ledger
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Config qualified as O
import Ouroboros.Consensus.Node qualified as O

import Marconi.ChainIndex.Indexers.EpochState qualified as EpochState
import Marconi.ChainIndex.Node.Client.GenesisConfig qualified as GenesisConfig
import Marconi.ChainIndex.Types (epochStateDbName)
import Marconi.ChainIndex.Utils qualified as Utils
import Marconi.Core.Storable qualified as Storable

import Hedgehog ((===))
import Hedgehog qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)


tests :: TestTree
tests = testGroup "CompareToDbSync"
  [ testPropertyNamed
      "Compare epoch nonces between marconi and dbsync"
      "propEpochNonce" propEpochNonce
  ]

-- | Connect to cardano-db-sync's postgres instance, get all (EpochNo,
-- Nonce) tuples, query and compare all of these to the one found in
-- Marconi.
--
-- As the number of epochs is low (406 at the time of writing), then
-- all nonces found in postgres are compared.
propEpochNonce :: H.Property
propEpochNonce = H.withTests 1 $ H.property $ do

  socketPath <- envOrFail "CARDANO_NODE_SOCKET_PATH"
  nodeConfigPath <- envOrFail "CARDANO_NODE_CONFIG_PATH"
  dbDir <- envOrFail "MARCONI_DB_DIRECTORY_PATH"

  indexer <- liftIO $ do
    securityParam <- Utils.querySecurityParamEra C.BabbageEraInCardanoMode C.Mainnet socketPath
    topLevelConfig <- topLevelConfigFromNodeConfig nodeConfigPath
    let dbPath = dbDir </> epochStateDbName
        ledgerStateDirPath = dbDir </> "ledgerStates"
    EpochState.open topLevelConfig dbPath ledgerStateDirPath securityParam

  pgPassword <- envOrFail "DBSYNC_PGPASSWORD"
  let connectionInfo = PG.ConnectInfo
        { PG.connectHost = "localhost"
        , PG.connectPort = 5432
        , PG.connectUser = "postgres"
        , PG.connectPassword = pgPassword
        , PG.connectDatabase = "cexplorer"
        }
  dbSyncEpochNonces <- liftIO $ getDbSyncAllEpochNonces =<< PG.connect connectionInfo
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
      interval = error "We don't use interval anymore" :: Storable.QueryInterval C.ChainPoint
  res' <- Storable.queryStorage interval [] (indexer ^. Storable.handle) query
  case res' of
    EpochState.NonceByEpochNoResult res -> return $ EpochState.epochNonceRowNonce <$> res
    _                                   -> return Nothing

getDbSyncAllEpochNonces :: PG.Connection -> IO [(C.EpochNo, Ledger.Nonce)]
getDbSyncAllEpochNonces conn = do
  res :: [(Int, BS.ByteString)] <- PG.query_ conn "select epoch_no, nonce from epoch_param order by epoch_no ASC"
  return $ map (\(int, bs) -> (fromIntegral int, fromBs bs)) res
  where
    fromBs :: BS.ByteString -> Ledger.Nonce
    fromBs = fromMaybe (error "dbSyncNonces: Can't deserialise") . bsToMaybeNonce

    bsToMaybeNonce :: BS.ByteString -> Maybe Ledger.Nonce
    bsToMaybeNonce bs = Ledger.Nonce <$> Crypto.hashFromBytes bs

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
