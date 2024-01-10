-- | Utilities for comparing the @Spent.'SpentInfoEvent'@ indexer to cardano-db-sync results.
module Test.Marconi.Cardano.DbSyncComparison.SpentInfoResult (
  -- * Test builder
  mkSpentInfoEventAtQueryTest,

  -- ** Generating results from cardano-db-sync data
  -- $sql-query
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (BlockEvent (BlockEvent))
import Cardano.Api.Shelley qualified as C
import Control.Exception (throwIO)
import Control.Monad.Except (runExceptT)
import Data.Aeson qualified as Aeson
import Data.ByteString.Short qualified as BSS
import Data.List.NonEmpty qualified as NEList
import Marconi.Cardano.Core.Runner qualified as Runner
import Marconi.Cardano.Indexers.Spent qualified as Spent
import Marconi.Core qualified as Core
import Marconi.Core.Indexer.SQLiteIndexer qualified as SQLiteIndexer
import System.FilePath ((</>))
import Test.Marconi.Cardano.DbSyncComparison.Common (
  DbSyncComparisonConfig (DbSyncComparisonConfig),
  eraToString,
  pathToGoldenFile,
  pathToOutFile,
  queryIndexerOnSnapshot,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsFileDiff)

-- | TODO: docstring with sql query?
mkSpentInfoEventAtQueryTest :: DbSyncComparisonConfig -> TestTree
mkSpentInfoEventAtQueryTest cfg@(DbSyncComparisonConfig nodeType era slotNo dbPath _ _) =
  goldenVsFileDiff
    ("At slot number " <> show slotNo)
    (\expected actual -> ["diff", "--color=always", expected, actual])
    goldenFile
    outFile
    runTest
  where
    runTest = do
      indexer <-
        either throwIO pure
          =<< runExceptT (Spent.mkSpentIndexer SQLiteIndexer.inMemoryDB)
      queryResult :: Maybe Spent.SpentInfoEvent <-
        queryIndexerOnSnapshot
          nodeType
          subChainPath
          dbPath
          spentInfoIndexerConfig
          (Just $ chainPointAtSlotNo slotNo)
          Core.EventAtQuery
          indexer
      -- NOTE: See documentation $sql-query for mention of sorting results.
      Aeson.encodeFile outFile (NEList.sort <$> queryResult)

    outFile = pathToOutFile cfg
    goldenFile = pathToGoldenFile cfg
    subChainPath = "../../mainnet-snapshots" </> eraToString era

-- | Config used for testing SpentInfo indexer on snapshot with no rollback.
spentInfoIndexerConfig :: Runner.RunIndexerOnSnapshotConfig BlockEvent Spent.SpentInfoEvent
spentInfoIndexerConfig = Runner.RunIndexerOnSnapshotConfig preprocessor 1
  where
    preprocessor = Runner.RunIndexerEventPreprocessing extractSpent (const Nothing) (const Nothing)
    mkIndexedTimedSpent
      :: C.ChainPoint -> C.TxBody era -> Core.ProcessedInput C.ChainPoint Spent.SpentInfoEvent
    mkIndexedTimedSpent point = Core.Index . Core.Timed point . NEList.nonEmpty . Spent.getInputs
    extractSpent :: BlockEvent -> [Core.ProcessedInput C.ChainPoint Spent.SpentInfoEvent]
    extractSpent (BlockEvent (C.BlockInMode (C.Block (C.BlockHeader slotNo hash _) txs) _) _ _) =
      map (mkIndexedTimedSpent (C.ChainPoint slotNo hash) . C.getTxBody) txs

{- | Create a 'ChainPoint' from a 'SlotNo' with arbitrary hash.
Used solely for testing here, where the hash information of chain point is
known not to be used.

NOTE: This is needed since the only way to query SpentInfo at a given slot number, as of
writing, is to use the 'ChainPoint' provided to 'query' with 'EventAtQuery'.
-}
chainPointAtSlotNo :: C.SlotNo -> C.ChainPoint
chainPointAtSlotNo = flip C.ChainPoint (C.HeaderHash BSS.empty)

{- $sql-query
   Results were generated using the following SQL query (PostgreSQL 11.19) on the
   database created from 'cardano-db-sync', varying the slot number according to the test.
   This creates the resulting JSON golden file.

   Note the result is sorted according to fields 'spentTxOutRef' then 'spentAtTxId', to avoid
   false negatives because of different orderings in the golden test.

  @
  SET client_encoding = 'UTF8';

  \set slot 18728839

  CREATE TEMP TABLE out_result AS
    WITH tx_at_slot AS (
      SELECT
        tx.id,
        tx.hash,
        tx.valid_contract,
        tx.block_index,
        block.slot_no
      FROM block
      JOIN tx ON tx.block_id = block.id
      WHERE block.slot_no = :slot
    -- tx_in_* tables attaches info on spent UTxOs from either the
    -- collateral_tx_in or tx_in tables, based on whether the TxIn
    -- or collateral TxIns would have been spent based on whether
    -- the script was valid or invalid, respectively.
    ), tx_in_collat AS (
      -- Script is invalid, so collateral is spent
      SELECT
        tx_at_slot.id,
        tx_at_slot.hash,
        collateral_tx_in.tx_out_id,
        collateral_tx_in.tx_out_index
      FROM tx_at_slot
      JOIN collateral_tx_in ON tx_at_slot.id = collateral_tx_in.tx_in_id
      WHERE NOT tx_at_slot.valid_contract
    ), tx_in_nocollat AS (
      -- Script is valid (or there is no script), so usual TxIn is spent
      SELECT
        tx_at_slot.id,
        tx_at_slot.hash,
        tx_in.tx_out_id,
        tx_in.tx_out_index
      FROM tx_at_slot
      JOIN tx_in ON tx_at_slot.id = tx_in.tx_in_id
      WHERE tx_at_slot.valid_contract
    ), tx_in_data AS (
      SELECT * FROM tx_in_collat
      UNION ALL
      SELECT * FROM tx_in_nocollat
    ), result AS (
      -- Attach the TxIx hash identifying each TxIn from the tx table
      -- based on the tx_out_id.
      SELECT
        -- Transaction where this TxIn was spent
        encode(tx_in_data.hash, 'hex') as spentAtTxId,
        -- Identifier for where this TxIn was created (appeared as TxOut)
        -- See FromJSON instance for TxIn
        -- https://cardano-api.cardano.intersectmbo.org/cardano-api/src/Cardano.Api.TxIn.html#TxIn
        concat(encode(tx.hash, 'hex'), '#', tx_in_data.tx_out_index) as spentTxOutRef
      FROM
        tx_in_data
      JOIN tx on tx_in_data.tx_out_id = tx.id
      -- Ordering used so that Haskell value can be encoded with the same ordering.
      ORDER BY spentTxOutRef, spentAtTxId
    )
    -- JSON formatting to match To/FromJSON instance in cardano-api
    SELECT
      json_agg(
        json_build_object(
          'spentAtTxId',
          spentAtTxId,
          'spentTxOutRef',
          spentTxOutRef
        )
      )
    FROM result;

  \copy out_result TO result.out.golden
  @
 -
-}
