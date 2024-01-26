{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

-- BLOCKSTART import

import Control.Concurrent (MVar)
import Control.Concurrent.Async (race_)
import Control.Lens ((^.))
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.List qualified as List
import Data.Maybe (listToMaybe)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Servant (Application, Proxy (Proxy), Server, serve, (:>))
import System.Environment (getArgs)

import Cardano.Api qualified as C
import Cardano.BM.Setup qualified as Trace
import Cardano.BM.Tracing qualified as Trace
import Data.Void (Void)
import Marconi.Cardano.ChainIndex.Utils qualified as Utils
import Marconi.Cardano.Core.Logger (mkMarconiTrace)
import Marconi.Cardano.Core.Node.Client.Retry qualified as Core
import Marconi.Cardano.Core.Orphans ()
import Marconi.Cardano.Core.Runner qualified as Core
import Marconi.Cardano.Core.Types (
  BlockEvent (BlockEvent),
  MarconiTrace,
  RetryConfig (RetryConfig),
  SecurityParam,
 )
import Marconi.Cardano.Indexers.SyncHelper qualified as Core
import Marconi.Core qualified as Core
import Marconi.Core.Indexer.SQLiteIndexer (SQLiteDBLocation, inMemoryDB)
import Marconi.Core.JsonRpc qualified as Core
import Network.JsonRpc.Types (JsonRpc, RawJsonRpc)

-- BLOCKEND import

main :: IO ()
main = runBlockInfoSqliteIndexerHttp

-- BLOCKSTART env
data Env indexer = Env
  { envTrace :: !(MarconiTrace IO)
  -- ^ The 'Trace' used for logging by `marconi-core`
  , envSocketFilePath :: !String
  -- ^ The local file path of the local node's socket file used to connect to the local node and
  -- fetch the blocks through the chain-sync protocol.
  , envNetworkId :: !C.NetworkId
  -- ^ Network Id of the local node which is required when connecting to the local node in order to
  -- make sure we are connecting to the correct network.
  , envSecurityParam :: !SecurityParam
  -- ^ Security parameter (K) of the local node. Used by Marconi to differenciate
  -- between immutable/stable blocks and immutable blocks.
  , envIndexerWorker :: !(Core.WorkerIndexer IO BlockEvent BlockInfoEvent indexer)
  -- ^ Worker of the indexer we want to run.
  --
  -- * 'BlockEvent': the data we get from the local node.
  -- * 'BlockInfoEvent': the event type
  -- * 'indexer' the indexer backend (ListIndexer, SQLiteIndexer, FileIndexer, etc.)
  }

withIndexerBuildEnv
  :: Core.WorkerIndexer IO BlockEvent BlockInfoEvent indexer
  -> (Env indexer -> IO ())
  -> IO ()
withIndexerBuildEnv worker action = do
  -- We start by getting the network id and socket file path of the local node.
  -- We get the values through the CLI.
  [socketFilePath, networkMagicStr] <- getArgs
  let networkMagic = read networkMagicStr

  -- For logging, we use IOG's logging framework
  -- called `iohk-monitoring-framework`: https://github.com/input-output-hk/iohk-monitoring-framework.
  -- We initialise the trace with default values.
  traceConfig <- Trace.defaultConfigStdout
  Trace.withTrace traceConfig "marconi-tutorial" $ \stdoutTrace -> do
    let marconiTrace = mkMarconiTrace stdoutTrace
    let networkId = C.Testnet $ C.NetworkMagic networkMagic

    -- We query the local node for the security parameter with a retry mechanism
    -- in case the node has not been started.
    k <- Core.withNodeConnectRetry marconiTrace (RetryConfig 30 Nothing) socketFilePath $ do
      Utils.toException $ Utils.querySecurityParam @Void networkId socketFilePath

    action (Env marconiTrace socketFilePath networkId k worker)

-- BLOCKEND env

-- BLOCKSTART runBlockInfoListIndexer
runBlockInfoListIndexer :: IO ()
runBlockInfoListIndexer = do
  worker <- mkBlockInfoListIndexerWorker
  withIndexerBuildEnv worker $ \env -> liftIO $ runIndexer env

-- BLOCKEND runBlockInfoListIndexer

-- BLOCKSTART runBlockInfoSqliteIndexer
runBlockInfoSqliteIndexer :: IO ()
runBlockInfoSqliteIndexer = do
  worker <- mkBlockInfoSqliteIndexerWorker inMemoryDB
  withIndexerBuildEnv worker $ \env -> liftIO $ runIndexer env

-- BLOCKEND runBlockInfoSqliteIndexer

-- BLOCKSTART runBlockInfoIndexer
runIndexer :: Env indexer -> IO ()
runIndexer env = do
  -- The 'runIndexer' function accepts an indexer (in short, anything that is
  -- an instance of 'Core.IsIndex'). However, the worker encapsulates an indexer,
  -- but is not an indexer itself. Therefore, we need to wrap it in an
  -- 'Core.Coordinator'.
  indexer <-
    liftIO $ Core.mkCoordinator [Core.worker $ envIndexerWorker env]
  liftIO $
    Core.runIndexerOnChainSync
      ( Core.RunIndexerConfig
          -- The 'Trace' used for logging
          (envTrace env)
          -- The block preprocessor. Will be explained in another tutorial.
          Core.withNoPreprocessor
          -- The retry configuration. If can't connect to local node, or lost
          -- connection, we retry given this configuration.
          (RetryConfig 30 Nothing)
          -- The security parameter
          (envSecurityParam env)
          -- The NetworkId
          (envNetworkId env)
          -- The point from which we want to start the chain-sync protocol.
          C.ChainPointAtGenesis
          -- UNIX socket file path exposed by the local node.
          (envSocketFilePath env)
      )
      -- The indexer to run given the previous configuration.
      indexer

-- BLOCKEND runBlockInfoIndexer

-- BLOCKSTART runBlockInfoListIndexerHttp
runBlockInfoListIndexerHttp :: IO ()
runBlockInfoListIndexerHttp = do
  worker <- mkBlockInfoListIndexerWorker
  withIndexerBuildEnv worker $ \env -> do
    let blockInfoWorker = envIndexerWorker env
    let blockInfoIndexerVar = Core.workerIndexerVar blockInfoWorker

    -- Run two threads: the indexing thread and query thread
    -- They each share an `MVar indexer`.
    race_
      (runHttpServer blockInfoIndexerVar)
      (runIndexer env)

-- BLOCKEND runBlockInfoListIndexerHttp

-- BLOCKSTART runBlockInfoSqliteIndexerHttp
runBlockInfoSqliteIndexerHttp :: IO ()
runBlockInfoSqliteIndexerHttp = do
  worker <- mkBlockInfoSqliteIndexerWorker inMemoryDB
  withIndexerBuildEnv worker $ \env -> do
    let blockInfoWorker = envIndexerWorker env
    let blockInfoIndexerVar = Core.workerIndexerVar blockInfoWorker

    -- Run two threads: the indexing thread and query thread
    -- They each share an `MVar indexer`.
    race_
      (runHttpServer blockInfoIndexerVar)
      (runIndexer env)

-- BLOCKEND runBlockInfoSqliteIndexerHttp

-- BLOCKSTART event

-- Marconi reads the blocks from the local node through the chain-sync protocol in
-- @Marconi.Cardano.Core.Runner.'runIndexer'@ (in `marconi-cardano-core`).
--
-- These blocks are forwarded to each indexer, and each indexer defines how a
-- block gets translated to an event (which is what we are defining below).

{- | Event which contains information about a given block. This is given by
the chain-sync protocol.
-}
newtype BlockInfoEvent = BlockInfoEvent
  { blockNo :: C.BlockNo
  }
  deriving (Show)

-- Marconi has a generic representation of a point given some event type. We
-- use the type available in `cardano-api`.
type instance Core.Point BlockInfoEvent = C.ChainPoint

{- | Extract the event from the block provided by a Cardano node.

Note that for Marconi, it is more optimal to return @Nothing@ instead
of @Just $ BlockInfoEvent mempty@ if there are no events for a given block.
-}
getEventsFromBlock :: BlockEvent -> Maybe BlockInfoEvent
getEventsFromBlock (BlockEvent (C.BlockInMode (C.Block (C.BlockHeader _ _ bn) _) _) _ _) =
  Just $ BlockInfoEvent bn

-- BLOCKEND event

-- BLOCKSTART ListIndexer
mkBlockInfoListIndexer :: Core.ListIndexer BlockInfoEvent
mkBlockInfoListIndexer = Core.mkListIndexer

-- BLOCKEND ListIndexer

-- BLOCKSTART ListIndexer worker
mkBlockInfoListIndexerWorker
  :: IO
      ( Core.WorkerIndexer
          IO
          BlockEvent
          BlockInfoEvent
          Core.ListIndexer
      )
mkBlockInfoListIndexerWorker =
  Core.createWorker
    "BlockInfo" -- Name of the indexer. Useful for logging.
    getEventsFromBlock -- Converting the block into the event
    mkBlockInfoListIndexer

-- BLOCKEND ListIndexer worker

-- BLOCKSTART query

-- The query parameter datatype.
newtype GetBlockInfoFromBlockNoQuery = GetBlockInfoFromBlockNoQuery C.BlockNo
  deriving stock (Eq, Ord, Show, Generic)

-- We return the full 'BlockInfoEvent' as a response.
-- The result is @Nothing@ if we provide a slot number which is in an unknown
-- future, or if no block was created at the requested slot.
type instance
  Core.Result GetBlockInfoFromBlockNoQuery =
    Maybe (Core.Timed C.ChainPoint BlockInfoEvent)

-- BLOCKEND query

-- BLOCKSTART ListIndexer query

-- | Query the in-memory indexer
instance
  (Monad m)
  => Core.Queryable
      m
      BlockInfoEvent -- The event type of the indexer
      GetBlockInfoFromBlockNoQuery -- The query type
      Core.ListIndexer -- The indexer backend
  where
  query
    :: Core.Point BlockInfoEvent -- Give me what you know up to this point, potentially a point in future
    -> GetBlockInfoFromBlockNoQuery -- The query we want to resolve
    -> Core.ListIndexer BlockInfoEvent -- The backend
    -> m (Core.Result GetBlockInfoFromBlockNoQuery)
  -- There is not data at genesis. Return 'Nothing'.
  query C.ChainPointAtGenesis _ _ = pure Nothing
  query cp (GetBlockInfoFromBlockNoQuery bn) lsIndexer = do
    -- Get all events from the ListIndexer which occurred *before* the
    -- requested point @cp@.
    let events = lsIndexer ^. Core.events
        eventsBeforePoint = filter (\e -> e ^. Core.point <= cp) events

    -- Then, we find the event which has the same block number as the one
    -- requested.
    pure $ List.find (\e -> blockNo (e ^. Core.event) == bn) eventsBeforePoint

-- BLOCKEND ListIndexer query

-- BLOCKSTART SQLiteIndexer

-- | Creation of the worker which wraps the 'Core.SQLiteIndexer' of our indexer.
mkBlockInfoSqliteIndexerWorker
  :: SQLiteDBLocation
  -- ^ Path of the SQLite database file
  -> IO
      ( Core.WorkerIndexer
          IO
          BlockEvent
          -- Input provided by the local node's chain-sync protocol
          BlockInfoEvent
          -- Event type of the indexer
          Core.SQLiteIndexer
          -- Storage type of the indexer
      )
mkBlockInfoSqliteIndexerWorker dbPath = do
  ix <- Utils.toException $ mkBlockInfoSqliteIndexer dbPath
  Core.createWorker "BlockInfo" getEventsFromBlock ix

-- | Creation of 'Core.SQLiteIndexer' for the 'BlockInfoEvent' indexer.
mkBlockInfoSqliteIndexer
  :: (MonadIO m, MonadError Core.IndexerError m)
  => SQLiteDBLocation
  -- ^ Path of the SQLite database file
  -> m (Core.SQLiteIndexer BlockInfoEvent)
mkBlockInfoSqliteIndexer dbPath = do
  -- This is actually a helper function on top of 'Core.mkSqliteIndexer' which
  -- stores the lastest stable indexed point from the chain. This is particularly
  -- useful when wanting to resume the indexer from previously indexed points.
  -- For now, we don't use the resuming capabilities, therefore 'Core.mkSqliteIndexer' has
  -- the same outcome as 'Core.mkSyncedSqliteIndexer'.
  Core.mkSyncedSqliteIndexer
    -- Path of the SQLite database file
    dbPath
    -- Request launched when the indexer is created. This creates the SQLite
    -- table if it does not exist.
    [dbCreation]
    -- Requests launched when an event is stored. The "plans" specify how to
    -- write the events in the SQLite database
    [
      [ Core.SQLInsertPlan
          -- Translate the event into a list of rows. In this specific indexer, each event is a
          -- single row.
          List.singleton
          -- The query that is called for each row that is the output of the previous parameter.
          blockInfoInsertQuery
      ]
    ]
    -- Requests launched when a rollback occurs
    [ Core.SQLRollbackPlan
        ( Core.defaultRollbackPlan
            -- Name of the SQLite table
            "block_info_table"
            -- Field name of the SQLite table which we will use for handling
            -- rollbacks (which deletes any database after that point)
            "slotNo"
            -- Translate the point of the 'Core.Timed point event' to a 'SlotNo'
            C.chainPointToSlotNo
        )
    ]
  where
    dbCreation =
      [sql|CREATE TABLE IF NOT EXISTS block_info_table
             ( slotNo INT NOT NULL
             , blockHeaderHash BLOB NOT NULL
             , blockNo INT NOT NULL
             )|]
    blockInfoInsertQuery :: SQL.Query
    blockInfoInsertQuery =
      [sql|INSERT INTO
             block_info_table
               ( slotNo
               , blockHeaderHash
               , blockNo
               ) VALUES (?, ?, ?)|]

-- This is needed by 'Core.mkSyncedSqliteIndexer' and 'Core.mkSqliteIndexer' in
-- order to specify how to store the event in the SQLite database.
instance SQL.ToRow (Core.Timed C.ChainPoint BlockInfoEvent) where
  toRow u =
    let (BlockInfoEvent bn) = u ^. Core.event
        (snoField, bhhField) = case u ^. Core.point of
          C.ChainPointAtGenesis -> (SQL.SQLNull, SQL.SQLNull)
          (C.ChainPoint sno bhh) -> (SQL.toField sno, SQL.toField bhh)
     in SQL.toRow
          ( snoField
          , bhhField
          , SQL.toField bn
          )

-- BLOCKEND SQLiteIndexer

-- BLOCKSTART SQLiteIndexer query

-- | Query the SQLite indexer
instance
  (MonadIO m)
  => Core.Queryable
      m
      BlockInfoEvent -- The event type of the indexer
      GetBlockInfoFromBlockNoQuery -- The query type
      Core.SQLiteIndexer -- The indexer backend
  where
  query
    :: Core.Point BlockInfoEvent -- give me what you know up to this point, potentially a point in future
    -> GetBlockInfoFromBlockNoQuery -- The query type
    -> Core.SQLiteIndexer BlockInfoEvent -- The indexer backend
    -> m (Core.Result GetBlockInfoFromBlockNoQuery)
  -- There is not data at genesis. Return 'Nothing'.
  query C.ChainPointAtGenesis _ _ = pure Nothing
  query (C.ChainPoint sn _) (GetBlockInfoFromBlockNoQuery bn) sqliteIndexer = do
    (results :: [Core.Timed C.ChainPoint BlockInfoEvent]) <-
      liftIO $
        SQL.query
          (sqliteIndexer ^. Core.connection)
          [sql|SELECT slotNo, blockHeaderHash, blockNo
               FROM block_info_table
               WHERE slotNo <= ? AND blockNo = ?|]
          (sn, bn)
    pure $ listToMaybe results

-- We need to define how to read the stored events which used the 'ToRow'
-- instance. Therefore, a property test making sure that you can roundtrip
-- between the 'ToRow' and 'FromRow' instances is useful.

instance SQL.FromRow (Core.Timed C.ChainPoint BlockInfoEvent) where
  fromRow = Core.Timed <$> SQL.fromRow <*> SQL.fromRow

instance SQL.FromRow BlockInfoEvent where
  fromRow = BlockInfoEvent <$> SQL.fromRow

-- BLOCKEND SQLiteIndexer query

-- BLOCKSTART HTTP

{----------------------
   API specification
-----------------------}

-- Types like 'RawJsonRpc', 'RpcMethod' and 'JsonRpc' come from the `json-rpc`
-- which is an extension to servant-server, but targeting JSON-RPC HTTP servers.

type API = "api" :> RawJsonRpc RpcMethod

type RpcMethod = RpcAddressCountMethod

type RpcAddressCountMethod =
  JsonRpc
    "getBlockInfoFromBlockNoQuery"
    GetBlockInfoFromBlockNoQuery
    String
    (Core.Result GetBlockInfoFromBlockNoQuery)

{----------------------
   API implementation
-----------------------}

{- | Run a JSON-RPC HTTP server for answering queries from the indexer provided
as a parameter.
-}
runHttpServer
  :: ( Core.Queryable
        (ExceptT (Core.QueryError GetBlockInfoFromBlockNoQuery) IO)
        BlockInfoEvent
        GetBlockInfoFromBlockNoQuery
        indexer
     , Core.IsSync IO BlockInfoEvent indexer
     )
  => MVar (indexer BlockInfoEvent)
  -> IO ()
runHttpServer indexerVar = do
  let httpSettings = setPort 3_000 defaultSettings
  liftIO $ runSettings httpSettings (mkHttpApp indexerVar)

mkHttpApp
  :: ( Core.Queryable
        (ExceptT (Core.QueryError GetBlockInfoFromBlockNoQuery) IO)
        BlockInfoEvent
        GetBlockInfoFromBlockNoQuery
        indexer
     , Core.IsSync IO BlockInfoEvent indexer
     )
  => MVar (indexer BlockInfoEvent)
  -> Application
mkHttpApp indexerVar = serve (Proxy @API) (mkHttpServer indexerVar)

mkHttpServer
  :: ( Core.Queryable
        (ExceptT (Core.QueryError GetBlockInfoFromBlockNoQuery) IO)
        BlockInfoEvent
        GetBlockInfoFromBlockNoQuery
        indexer
     , Core.IsSync IO BlockInfoEvent indexer
     )
  => MVar (indexer BlockInfoEvent)
  -> Server API
mkHttpServer =
  -- This function comes from `marconi-core-json-rpc` which can translate the
  -- `Queryable` instance of an indexer to a Servant `Handler` that is used to
  -- define the `Server`.
  Core.queryIndexerVarHttpHandler

-- Let's just use the default FromJSON instance
instance FromJSON GetBlockInfoFromBlockNoQuery

-- This is our custom ToJSON response for the event.
instance ToJSON BlockInfoEvent where
  toJSON (BlockInfoEvent bn) =
    Aeson.object
      [ "blockNo" .= bn
      ]

-- BLOCKEND HTTP
