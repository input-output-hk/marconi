{-# LANGUAGE DataKinds #-}

module Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo (
  AddressUtxoResult (AddressUtxoResult),
  GetUtxosFromAddressResult (GetUtxosFromAddressResult),
  RpcGetUtxosFromAddressMethod,
  SpentInfoResult (SpentInfoResult),
  getUtxosFromAddressQueryHandler,
) where

import Cardano.Api qualified as C
import Control.Lens (view, (^.))
import Control.Monad (join, unless)
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadTrans (lift), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (bimap, first)
import Data.Text (pack)
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.SpentInfoResult (
  SpentInfoResult (SpentInfoResult),
 )
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.Types (
  AddressUtxoResult (AddressUtxoResult),
  GetUtxosFromAddressParams (GetUtxosFromAddressParams),
  GetUtxosFromAddressResult (GetUtxosFromAddressResult),
 )
import Marconi.ChainIndex.Api.JsonRpc.Endpoint.Utxo.Wrappers (
  UtxoTxInput (UtxoTxInput),
  ValueWrapper (ValueWrapper),
 )
import Marconi.ChainIndex.Api.Types (HttpServerConfig, configQueryables, configTrackedAddresses)
import Marconi.ChainIndex.Indexers (queryableUtxo)
import Marconi.ChainIndex.Indexers.BlockInfo qualified as BI
import Marconi.ChainIndex.Indexers.Utxo (datumHash, txIn, txIndex, value)
import Marconi.ChainIndex.Indexers.UtxoQuery (
  UtxoQueryInput (UtxoQueryInput),
  UtxoResult (UtxoResult),
 )
import Marconi.Core qualified as Core
import Marconi.Core.JsonRpc (ReaderHandler, hoistHttpHandler, queryErrToRpcErr)
import Marconi.Core.Type (Result)
import Network.JsonRpc.Types (JsonRpc, JsonRpcErr, mkJsonRpcInternalErr, mkJsonRpcInvalidParamsErr)
import Servant.Server (Handler)

------------------
-- Method types --
------------------

type RpcGetUtxosFromAddressMethod =
  JsonRpc
    "getUtxosFromAddress"
    GetUtxosFromAddressParams
    String
    GetUtxosFromAddressResult

--------------
-- Handlers --
--------------

-- | Handler for retrieving UTXOs by Address
getUtxoQueryInputHandler
  :: UtxoQueryInput
  -- ^ Bech32 addressCredential and possibly a pair of slotNumbers
  -> ReaderHandler HttpServerConfig (Either (JsonRpcErr String) (Result UtxoQueryInput))
getUtxoQueryInputHandler query = do
  indexer <- view (configQueryables . queryableUtxo)
  let sqliteAggregateLastSyncPoint :: ExceptT (Core.QueryError UtxoQueryInput) Handler C.ChainPoint
      sqliteAggregateLastSyncPoint = do
        res <- liftIO $ runExceptT $ lift $ Core.lastSyncPoint indexer
        case res of
          Left err -> throwError err
          Right res' -> pure res'
  lastPointM <- hoistHttpHandler $ runExceptT sqliteAggregateLastSyncPoint
  case lastPointM of
    Left err ->
      pure $
        Left $
          mkJsonRpcInternalErr $
            Just $
              "Can't resolve last point in getUtxosFromAddress: " <> show err
    Right lastPoint ->
      hoistHttpHandler $
        liftIO $
          first queryErrToRpcErr <$> Core.queryEither lastPoint query indexer

-- | Return 'GetBurnTokenEventsResult' based on 'GetBurnTokenEventsParams'
getUtxosFromAddressQueryHandler
  :: GetUtxosFromAddressParams
  -> ReaderHandler HttpServerConfig (Either (JsonRpcErr String) GetUtxosFromAddressResult)
getUtxosFromAddressQueryHandler query = do
  addresses <- view configTrackedAddresses
  res <- join <$> traverse getUtxoQueryInputHandler (mapGetBurnTokenEventsQuery addresses query)
  pure $ mapGetBurnTokenEventsResult =<< res
  where
    mapGetBurnTokenEventsQuery
      :: [C.AddressAny] -> GetUtxosFromAddressParams -> Either (JsonRpcErr String) UtxoQueryInput
    mapGetBurnTokenEventsQuery
      targetAddresses
      (GetUtxosFromAddressParams addr createdAtAfter unspentBefore) = do
        mappedAddr <- addressMapping (pack addr)
        unless
          (null targetAddresses || mappedAddr `elem` targetAddresses)
          ( Left $
              mkJsonRpcInvalidParamsErr $
                Just "The 'address' param value must belong to the provided target addresses."
          )
        pure $ UtxoQueryInput mappedAddr createdAtAfter unspentBefore
        where
          addressMapping addressText = do
            bimap
              ( const $
                  mkJsonRpcInvalidParamsErr $
                    Just "The 'address' param value must be in the Bech32 format."
              )
              C.toAddressAny
              $ C.deserialiseFromBech32 C.AsShelleyAddress addressText
    -- Map internal events to our 'getBurnTokenEventsHandler' response object
    mapGetBurnTokenEventsResult :: [UtxoResult] -> Either (JsonRpcErr String) GetUtxosFromAddressResult
    mapGetBurnTokenEventsResult = fmap GetUtxosFromAddressResult . traverse mapResult
    mapResult :: UtxoResult -> Either (JsonRpcErr String) AddressUtxoResult
    mapResult (UtxoResult _ _ (Core.Timed C.ChainPointAtGenesis _) _ _) =
      Left $ mkJsonRpcInvalidParamsErr $ Just "Block at genesis not supported"
    mapResult
      ( UtxoResult
          utxo
          datum
          blockInfo@(Core.Timed (C.ChainPoint blockSlotNo header) _)
          spentInfo
          inputs
        ) =
        let mapSpentInfo :: Core.Timed C.ChainPoint (BI.BlockInfo, C.TxId) -> Maybe SpentInfoResult
            mapSpentInfo (Core.Timed C.ChainPointAtGenesis _) = Nothing
            mapSpentInfo (Core.Timed (C.ChainPoint spentInfoSlotNo _) (_, txId)) =
              Just $ SpentInfoResult spentInfoSlotNo txId
            C.TxIn txInTxId txInTxIx = utxo ^. txIn
            asTuple (C.TxIn id' ix') = (id', ix')
         in Right $
              AddressUtxoResult
                blockSlotNo
                header
                (blockInfo ^. Core.event . BI.epochNo)
                (blockInfo ^. Core.event . BI.blockNo)
                (utxo ^. txIndex)
                txInTxId
                txInTxIx
                (utxo ^. datumHash)
                datum
                (ValueWrapper $ utxo ^. value)
                (mapSpentInfo =<< spentInfo)
                (uncurry UtxoTxInput . asTuple <$> inputs)
