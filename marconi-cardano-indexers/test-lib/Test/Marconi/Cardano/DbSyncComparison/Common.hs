{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Marconi.Cardano.DbSyncComparison.Common (
  -- * Runner
  queryIndexerOnSnapshot,

  -- * Utils
  toRuntimeException,
  getNodeConfigPath,
  pathToOutFile,
  pathToGoldenFile,

  -- * Types
  DbSyncComparisonConfig (..),
  NodeType (..),
  nodeTypeToString,
  Era (..),
  eraToString,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent)
import Control.Concurrent (readMVar)
import Control.Concurrent.Async (wait, withAsync)
import Control.Monad.Except (ExceptT, runExceptT)
import Marconi.Cardano.Core.Runner qualified as Core
import Marconi.Core qualified as Core
import System.Environment (getEnv)
import System.FilePath ((</>))
import Test.Marconi.Cardano.Chain.Snapshot (setupSnapshot)

data DbSyncComparisonConfig = DbSyncComparisonConfig
  { dbSyncComparisonNodeType :: NodeType
  , dbSyncComparisonEra :: Era
  , dbSyncComparisonSlotNo :: C.SlotNo
  , dbSyncComparisonDbPath :: FilePath
  , dbSyncComparisonGoldenDir :: String
  -- ^ Base directory in which golden files are located.
  -- See 'pathToOutFile' and 'pathToGoldenFile'.
  , dbSyncComparisonName :: String
  -- ^ Identifier for the comparison, e.g. "blockinfo"
  }

pathToOutFile :: DbSyncComparisonConfig -> FilePath
pathToOutFile
  ( DbSyncComparisonConfig
      (nodeTypeToString -> nodeType)
      (eraToString -> era)
      (C.unSlotNo -> slotNo)
      _
      gdir
      nm
    ) =
    gdir </> nodeType </> era </> nm <> "-slot" <> show slotNo <> ".out"

pathToGoldenFile :: DbSyncComparisonConfig -> FilePath
pathToGoldenFile cfg = pathToOutFile cfg <> ".golden"

-- | The Cardano network type used to create the snapshot.
data NodeType = Preview | Preprod | Mainnet

nodeTypeToString :: NodeType -> String
nodeTypeToString Preview = "preview"
nodeTypeToString Preprod = "preprod"
nodeTypeToString Mainnet = "mainnet"

-- | The Cardano era the snapshot is a part of.
data Era
  = Byron1
  | Byron2
  | Shelley
  | Allegra
  | Mary
  | Alonzo1
  | Alonzo2
  | Babbage1
  | Babbage2
  deriving (Show)

eraToString :: Era -> String
eraToString =
  \case
    Byron1 -> "byron1"
    Byron2 -> "byron2"
    Shelley -> "shelley"
    Allegra -> "allegra"
    Mary -> "mary"
    Alonzo1 -> "alonzo1"
    Alonzo2 -> "alonzo2"
    Babbage1 -> "babbage1"
    Babbage2 -> "babbage2"

{- | Runs an indexer on a snapshot, and allows the user to query
 the information which was accumulated from the snapshot.
-}
queryIndexerOnSnapshot
  :: forall event indexer query
   . ( Core.IsIndex (ExceptT Core.IndexerError IO) event indexer
     , Core.Closeable IO indexer
     , Core.Point event ~ C.ChainPoint
     , Core.Queryable (ExceptT (Core.QueryError query) IO) event query indexer
     , Core.IsSync (ExceptT (Core.QueryError query) IO) event indexer
     , Show (Core.Result query)
     )
  => NodeType
  -> FilePath
  -- ^ directory which contains the serialised sub-chain
  -> FilePath
  -- ^ directory to be used as the indexer's DB
  -> Core.RunIndexerOnSnapshotConfig BlockEvent event
  -> Maybe C.ChainPoint
  -- ^ Optional point to pass to @Core.'query'@. Else, use @Core.'queryLatest'@.
  -> query
  -> indexer event
  -> IO (Core.Result query)
queryIndexerOnSnapshot nodeType subChainPath dbPath config point query indexer = do
  configFile <- getNodeConfigPath nodeType
  blockStream <- setupSnapshot configFile subChainPath dbPath
  let runIndexer = Core.runIndexerOnSnapshot config indexer blockStream
  withAsync runIndexer $ \runner -> do
    finalState <- wait runner >>= readMVar
    toRuntimeException $
      maybe (Core.queryLatest @(ExceptT (Core.QueryError query) IO)) Core.query point query finalState

{- | The path to a data directory containing configuration files
is set to an environment variable. This function retrieves the right
configuration file path.
-}
getNodeConfigPath :: NodeType -> IO FilePath
getNodeConfigPath nodeType = do
  configDir <- getEnv "CARDANO_NODE_CONFIG"
  pure $
    configDir
      </> "cardano-node"
      </> nodeTypeToString nodeType
      </> "config.json"

{- | Utility function for avoiding the usage of 'ExceptT'. This
 is a reasonable approach because if any failure happened the
 test can no longer continue and should abort.
-}
toRuntimeException :: (Monad m, Show e) => ExceptT e m a -> m a
toRuntimeException action = do
  result <- runExceptT action
  case result of
    Left err -> error (show err)
    Right a -> pure a
