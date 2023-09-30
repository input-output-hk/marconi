{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An indexer that tracks the chainTip of the ledger, storing the tip in a file.
module Marconi.ChainIndex.Experimental.Indexers.ChainTip (
  ChainTipIndexer,
  mkChainTipIndexer,
  ChainTipConfig (ChainTipConfig),
  chainTipWorker,
) where

import Cardano.Api.Extended qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Trace qualified as BM
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (MonadError)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Short qualified as BS.Short
import Data.Text (Text)
import Marconi.ChainIndex.Experimental.Indexers.Orphans ()
import Marconi.Core qualified as Core

type instance Core.Point C.ChainTip = C.ChainPoint

type ChainTipIndexer m = Core.WithTrace m Core.LastEventIndexer C.ChainTip

data ChainTipConfig = ChainTipConfig
  { path :: FilePath
  -- ^ path to save the chain tip
  , saveEvery :: Word
  -- ^ how often (in blocks) we save the chain tip (an extra save would be performed on closing)
  }

-- | Configure and start the 'ChainTip' indexer
mkChainTipIndexer
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => BM.Trace m (Core.IndexerEvent C.ChainPoint)
  -> ChainTipConfig
  -> n (ChainTipIndexer m)
mkChainTipIndexer tracer cfg = do
  let lastEventConfig =
        Core.LastEventConfig
          (path cfg)
          False
          serialisePoint
          deserialisePoint
          serialiseTip
          deserialiseTip
          (saveEvery cfg)

  indexer <- Core.mkLastEventIndexer lastEventConfig
  pure $ Core.withTrace tracer indexer

-- | Start a 'ChainTipIndexer' and put it in a worker
chainTipWorker
  :: (MonadIO n, MonadError Core.IndexerError n, MonadIO m)
  => BM.Trace m (Core.IndexerEvent C.ChainPoint)
  -> (event -> Maybe C.ChainTip)
  -> ChainTipConfig
  -> n
      ( Core.WorkerIndexer
          m
          event
          C.ChainTip
          (Core.WithTrace m Core.LastEventIndexer)
      )
chainTipWorker tracer extractor cfg =
  Core.createWorker "chainTip" extractor =<< mkChainTipIndexer tracer cfg

serialiseTip :: C.ChainTip -> BS.ByteString
serialiseTip =
  let tipEncoding :: C.ChainTip -> CBOR.Encoding
      tipEncoding C.ChainTipAtGenesis = CBOR.encodeBool False
      tipEncoding (C.ChainTip (C.SlotNo slotNo) (C.HeaderHash hashBytes) (C.BlockNo blockNo)) =
        CBOR.encodeBool True
          <> CBOR.encodeWord64 slotNo
          <> CBOR.encodeBytes (BS.Short.fromShort hashBytes)
          <> CBOR.encodeWord64 blockNo
   in CBOR.toStrictByteString . tipEncoding

deserialiseTip :: BS.ByteString -> Either Text C.ChainTip
deserialiseTip bs =
  let tipDecoding = do
        b <- CBOR.decodeBool
        if b
          then do
            s <- C.SlotNo <$> CBOR.decodeWord64
            bhh <- C.HeaderHash . BS.Short.toShort <$> CBOR.decodeBytes
            bn <- C.BlockNo <$> CBOR.decodeWord64
            pure $ C.ChainTip s bhh bn
          else pure C.ChainTipAtGenesis
   in case CBOR.deserialiseFromBytes tipDecoding . BS.fromStrict $ bs of
        Right (remain, res) | BS.Lazy.null remain -> Right res
        _other -> Left "Can't read chainpoint"

serialisePoint :: C.ChainPoint -> BS.ByteString
serialisePoint =
  let pointEncoding :: C.ChainPoint -> CBOR.Encoding
      pointEncoding C.ChainPointAtGenesis = CBOR.encodeBool False
      pointEncoding (C.ChainPoint (C.SlotNo s) (C.HeaderHash bhh)) =
        CBOR.encodeBool True <> CBOR.encodeWord64 s <> CBOR.encodeBytes (BS.Short.fromShort bhh)
   in CBOR.toStrictByteString . pointEncoding

deserialisePoint :: BS.ByteString -> Either Text C.ChainPoint
deserialisePoint bs =
  let pointDecoding = do
        b <- CBOR.decodeBool
        if b
          then do
            s <- C.SlotNo <$> CBOR.decodeWord64
            bhh <- C.HeaderHash . BS.Short.toShort <$> CBOR.decodeBytes
            pure $ C.ChainPoint s bhh
          else pure C.ChainPointAtGenesis
   in case CBOR.deserialiseFromBytes pointDecoding . BS.fromStrict $ bs of
        Right (remain, res) | BS.Lazy.null remain -> Right res
        _other -> Left "Can't read chainpoint"
