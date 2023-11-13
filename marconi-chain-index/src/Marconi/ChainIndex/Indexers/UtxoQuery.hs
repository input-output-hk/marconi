{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- | An aggregate indexer that gathers information of 'Utxo', 'Spent', 'Datum' and 'BlockInfo'
to provide the UTxO information with resolved datum and information about the TxIn used to produce
an UTxO.
-}
module Marconi.ChainIndex.Indexers.UtxoQuery (
  -- * Indexer
  UtxoQueryIndexer,
  UtxoQueryAggregate (..),
  UtxoQueryEvent,
  mkUtxoSQLiteQuery,

  -- * Query
  UtxoQueryInput (..),
  address,
  lowerBound,
  upperBound,

  -- * Result
  UtxoResult (UtxoResult),
  utxo,
  datum,
  blockInfo,
  spentInfo,
  inputs,
) where

import Cardano.Api qualified as C
import Control.Applicative (Alternative ((<|>)))
import Control.Concurrent (MVar)
import Control.Lens ((^.), (^?))
import Control.Lens qualified as Lens
import Control.Monad.Cont (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField qualified as SQL
import Database.SQLite.Simple.FromRow qualified as SQL
import Database.SQLite.Simple.QQ (sql)
import GHC.Generics (Generic)
import Marconi.ChainIndex.Indexers.BlockInfo (BlockInfo (BlockInfo))
import Marconi.ChainIndex.Indexers.Datum (DatumInfo)
import Marconi.ChainIndex.Indexers.Spent (SpentInfo)
import Marconi.ChainIndex.Indexers.Utxo (Utxo)
import Marconi.ChainIndex.Orphans ()
import Marconi.Core qualified as Core
import Marconi.Core.Indexer.SQLiteAggregateQuery (
  SQLiteSourceProvider (SQLiteSourceProvider),
 )
import Text.Read (readMaybe)

-- | Uninhabited type for the UtxoQueryAggregate (as it's an aggregate, it doesn't index anything)
data UtxoQueryEvent

type instance Core.Point UtxoQueryEvent = C.ChainPoint

-- | Explicit list of resources needed for the UtxoQueryAggregate
data UtxoQueryAggregate m = forall
    utxoIndexer
    utxo
    spentIndexer
    spent
    datumIndexer
    datum
    blockInfoIndexer
    blockInfo. -- with use a b c and d to allow WithTransform in the indexer stack
  ( Core.IsSourceProvider m (NonEmpty Utxo) utxoIndexer
  , Core.IsSourceProvider m utxo utxoIndexer
  , Core.Point utxo ~ Core.Point (NonEmpty Utxo)
  , Core.IsSourceProvider m (NonEmpty SpentInfo) spentIndexer
  , Core.IsSourceProvider m spent spentIndexer
  , Core.Point spent ~ Core.Point (NonEmpty SpentInfo)
  , Core.IsSourceProvider m (NonEmpty DatumInfo) datumIndexer
  , Core.IsSourceProvider m datum datumIndexer
  , Core.Point datum ~ Core.Point (NonEmpty DatumInfo)
  , Core.IsSourceProvider m BlockInfo blockInfoIndexer
  , Core.IsSourceProvider m blockInfo blockInfoIndexer
  , Core.Point blockInfo ~ Core.Point BlockInfo
  ) =>
  UtxoQueryAggregate
  { utxoIndexer :: MVar (utxoIndexer utxo)
  -- ^ the source provider for the 'Utxo' table, usually a @UtxoIndexer@
  , spentIndexer :: MVar (spentIndexer spent)
  -- ^ the source provider for the 'Spent' table, usually a @SpentIndexer@
  , datumIndexer :: MVar (datumIndexer datum)
  -- ^ the source provider for the 'Datum' table, usually a @DatumIndexer@
  , blockInfoIndexer :: MVar (blockInfoIndexer blockInfo)
  -- ^ the source provider for the 'BlockInfo' table, usually a @BlockInfoIndexer@
  }

data UtxoResult = UtxoResult
  { _utxo :: Utxo
  , _datum :: !(Maybe C.ScriptData)
  , _blockInfo :: !(Core.Timed C.ChainPoint BlockInfo)
  , _spentInfo :: !(Maybe (Core.Timed C.ChainPoint (BlockInfo, C.TxId)))
  , _inputs :: [C.TxIn]
  }
  deriving (Eq, Show, Generic)

Lens.makeLenses ''UtxoResult

instance FromJSON UtxoResult where
  parseJSON =
    let parseFields v = do
          utxo' <- v .: "utxo"
          datum' <- v .:? "datum"
          creationPoint <- v .: "creationPoint"
          creationBlock <- v .: "creationBlockInfo"
          let createdAt = Core.Timed creationPoint creationBlock
          mSpentPoint <- v .:? "spentPoint"
          mSpentBlockInfo <- v .:? "spentBlockInfo"
          mSpentTxId <- v .:? "spentTx"
          let spentInfo' = do
                spentPoint <- mSpentPoint
                spentBlockInfo <- mSpentBlockInfo
                spentTxId <- mSpentTxId
                pure $ Core.Timed spentPoint (spentBlockInfo, spentTxId)
          inputs' <- v .: "inputs"
          pure $ UtxoResult utxo' datum' createdAt spentInfo' inputs'
     in Aeson.withObject "UtxoResult" parseFields

instance ToJSON UtxoResult where
  toJSON utxoResult =
    Aeson.object $
      catMaybes
        [ Just $ "utxo" .= (utxoResult ^. utxo)
        , ("datum" .=) <$> utxoResult ^. datum
        , Just $ "creationPoint" .= (utxoResult ^. blockInfo . Core.point)
        , Just $ "creationBlockInfo" .= (utxoResult ^. blockInfo . Core.event)
        , ("spentPoint" .=) <$> utxoResult ^? spentInfo . Lens.folded . Core.point
        , ("spentBlockInfo" .=) <$> utxoResult ^? spentInfo . Lens.folded . Core.event . Lens._1
        , ("spentTx" .=) <$> utxoResult ^? spentInfo . Lens.folded . Core.event . Lens._2
        , Just $ "inputs" .= (utxoResult ^. inputs)
        ]

instance SQL.FromRow UtxoResult where
  fromRow = do
    let decodeTxId :: Text.Text -> Maybe C.TxId
        decodeTxId =
          either (const Nothing) pure . C.deserialiseFromRawBytesHex C.AsTxId . Text.encodeUtf8
        txIdsFromField :: SQL.RowParser [C.TxId]
        txIdsFromField = do
          concatenatedTxIds <- SQL.field
          case concatenatedTxIds of
            Nothing -> pure []
            Just xs | xs == "" -> pure []
            Just xs ->
              case traverse decodeTxId $ Text.splitOn "," xs of
                Nothing -> SQL.fieldWith $ \field' ->
                  SQL.returnError
                    SQL.ConversionFailed
                    field'
                    ("Can't decode the spent txIds sequence: " <> show xs)
                Just xs' -> pure xs'
        decodeTxIx :: Text.Text -> Maybe C.TxIx
        decodeTxIx = fmap C.TxIx . readMaybe . Text.unpack
        txIxesFromField :: SQL.RowParser [C.TxIx]
        txIxesFromField = do
          concatenatedTxIxs <- SQL.field
          case concatenatedTxIxs of
            Nothing -> pure []
            Just xs ->
              case traverse decodeTxIx $ Text.splitOn "," xs of
                Nothing -> SQL.fieldWith $ \field' ->
                  SQL.returnError SQL.ConversionFailed field' "Can't decode the spent txIxs sequence"
                Just xs' -> pure xs'
        txInsFromRow :: SQL.RowParser [C.TxIn]
        txInsFromRow = do
          txIds <- txIdsFromField
          txIxes <- txIxesFromField
          pure $ zipWith C.TxIn txIds txIxes
        maybeChainPoint :: SQL.RowParser (Maybe C.ChainPoint)
        maybeChainPoint = do
          sn <- SQL.field
          bhh <- SQL.field
          pure $ C.ChainPoint <$> sn <*> bhh
        maybeBlockInfo :: SQL.RowParser (Maybe BlockInfo)
        maybeBlockInfo = do
          bn <- SQL.field
          ts <- SQL.field
          en <- SQL.field
          pure $ BlockInfo <$> bn <*> ts <*> en
    utxo' <- SQL.fromRow
    datum' <- SQL.field
    blockInfo' <- SQL.fromRow
    spentChainPoint <- maybeChainPoint
    spentBlockInfo <- maybeBlockInfo
    spentAt <- SQL.field
    txIns <- txInsFromRow
    let spentInfo' = do
          spentChainPoint' <- spentChainPoint
          spentBlockInfo' <- spentBlockInfo
          spentAt' <- spentAt
          pure $ Core.Timed spentChainPoint' (spentBlockInfo', spentAt')
    pure $ UtxoResult utxo' datum' blockInfo' spentInfo' txIns

data UtxoQueryInput = UtxoQueryInput
  { _address :: C.AddressAny
  , _lowerBound :: Maybe C.SlotNo
  -- ^ Inclusive lowerBound
  , _upperBound :: Maybe C.SlotNo
  -- ^ Inclusive upperBound (utxo spent after this point displaed with their spent information)
  }
  deriving (Generic, ToJSON, FromJSON)

Lens.makeLenses ''UtxoQueryInput

-- | An alias for the 'SQLiteAggregateQuery' that handle the 'UtxoQueryEvent'
type UtxoQueryIndexer m =
  Core.WithTrace
    IO
    ( Core.SQLiteAggregateQuery
        m
        C.ChainPoint
    )
    UtxoQueryEvent

-- | Generate a @UtxoQueryIndexer@ from the given source
mkUtxoSQLiteQuery
  :: UtxoQueryAggregate m
  -> IO (Core.SQLiteAggregateQuery m C.ChainPoint event)
mkUtxoSQLiteQuery (UtxoQueryAggregate _utxo _spent _datum _blockInfo) =
  Core.mkSQLiteAggregateQuery $
    Map.fromList
      [ ("utxo", SQLiteSourceProvider _utxo)
      , ("spent", SQLiteSourceProvider _spent)
      , ("datum", SQLiteSourceProvider _datum)
      , ("blockInfo", SQLiteSourceProvider _blockInfo)
      ]

type instance Core.Result UtxoQueryInput = [UtxoResult]

baseQuery :: SQL.Query
baseQuery =
  [sql|
  SELECT
    -- UtxoResult.utxo
    u.address,
    u.txIndex,
    u.txId,
    u.txIx,
    u.datumHash,
    u.value,
    u.inlineScript,
    u.inlineScriptHash,

    -- UtxoResult.datum
    d.datum,

    -- UtxoResult.blockInfo
    utxoBlockInfo.blockNo,
    utxoBlockInfo.blockTimestamp,
    utxoBlockInfo.epochNo,
    utxoBlockInfo.slotNo,
    utxoBlockInfo.blockHeaderHash,

    -- UtxoResult.spentInfo
    spentBlockInfo.slotNo,
    spentBlockInfo.blockHeaderHash,
    spentBlockInfo.blockNo,
    spentBlockInfo.blockTimestamp,
    spentBlockInfo.epochNo,
    futureSpent.spentAtTxId,

    -- UtxoResult.inputs
    GROUP_CONCAT(HEX(txIn.txId)),
    GROUP_CONCAT(txIn.txIx)

  FROM utxo.utxo u
  -- Attach spent information to an UTXO
  LEFT JOIN spent.spent futureSpent ON futureSpent.txId = u.txId AND futureSpent.txIx = u.txIx
  -- Attach datum
  LEFT JOIN datum.datum d ON d.datumHash = u.datumHash
  -- Attach TxIns
  LEFT JOIN spent.spent txIn ON txIn.spentAtTxId = u.txId
  -- Attach utxo block info
  LEFT JOIN blockInfo.blockInfo utxoBlockInfo ON utxoBlockInfo.slotNo = u.slotNo
  -- Attach spent block info
  LEFT JOIN blockInfo.blockInfo spentBlockInfo ON spentBlockInfo.slotNo = futureSpent.slotNo
  |]

instance
  (MonadIO m)
  => Core.Queryable
      m
      UtxoQueryEvent
      UtxoQueryInput
      (Core.SQLiteAggregateQuery n C.ChainPoint)
  where
  query point q indexer =
    let addressFilter = (["u.address = :address"], [":address" := q ^. address])
        lowerBoundFilter = case q ^. lowerBound of
          Nothing -> mempty
          Just lo -> (["u.slotNo >= :lowerBound"], [":lowerBound" := lo])
        upperBoundFilter = case q ^. upperBound <|> C.chainPointToSlotNo point of
          Nothing -> (["s.slotNo IS NULL"], [])
          Just hi ->
            ( -- created before the upperBound

              [ "u.slotNo <= :upperBound"
              , -- unspent or spent after the upper bound
                "(futureSpent.slotNo IS NULL OR futureSpent.slotNo > :upperBound)"
              ]
            , [":upperBound" := hi]
            )
        (filters, params) = addressFilter <> lowerBoundFilter <> upperBoundFilter
        sqlFilters = " WHERE " <> SQL.Query (Text.intercalate " AND " $ SQL.fromQuery <$> filters)
        query =
          if null filters
            then baseQuery
            else
              baseQuery
                <> sqlFilters
                <> " GROUP BY u.txId, u.txIx"
                <> " ORDER BY u.slotNo ASC"
     in liftIO $ SQL.queryNamed (indexer ^. Core.aggregateConnection) query params
