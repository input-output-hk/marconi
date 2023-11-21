{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.ChainIndex.Logger (tests) where

import Cardano.Api qualified as C
import Data.ByteString.Lazy (ByteString)
import Data.Data (Proxy (Proxy))
import Data.String (fromString)
import Data.Text qualified as Text
import Data.Word (Word64)
import Marconi.Cardano.Core.Logger (
  marconiFormatting,
  nullTracer,
 )
import Marconi.Cardano.Core.Transformer.WithSyncLog (
  LastSyncStats (LastSyncStats),
  SyncLog (SyncLog),
 )
import Prettyprinter (Pretty (pretty))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.ChainIndex.Logger"
    [ goldenVsStringDiff
        "Golden test for syncing logs when starting from genesis"
        (\expected actual -> ["diff", "--color=always", expected, actual])
        "test/Spec/Marconi/ChainIndex/Logger/Golden/start-from-genesis-logging.txt"
        goldenStartFromGenesisLogging
    , goldenVsStringDiff
        "Golden test for syncing logs when starting from chain point other than genesis"
        (\expected actual -> ["diff", "--color=always", expected, actual])
        "test/Spec/Marconi/ChainIndex/Logger/Golden/start-from-later-point-logging.txt"
        goldenStartFromLaterPointLogging
    ]

goldenStartFromGenesisLogging :: IO ByteString
goldenStartFromGenesisLogging = do
  let fakeBlockHeaderHashRawBytes = "6161616161616161616161616161616161616161616161616161616161616161"
  fakeBlockHeaderHash <-
    either
      (error . show)
      pure
      $ C.deserialiseFromRawBytesHex
        (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader))
        fakeBlockHeaderHashRawBytes
  let logs =
        [ mkLastSyncLog fakeBlockHeaderHash 0 0 Nothing Nothing
        , mkLastSyncLog fakeBlockHeaderHash 0 0 Nothing (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 10 1 (Just 10000) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 40 0 (Just 50000) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 30 0 (Just 80000) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 10 0 (Just 90000) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 5 2 (Just 95000) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 5 1 (Just 99995) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 5 1 (Just 99999) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 5 3 (Just 100000) (Just 10)
        ]
  pure $ fromString $ Text.unpack $ Text.intercalate "\n" $ fmap (marconiFormatting . pretty) logs

goldenStartFromLaterPointLogging :: IO ByteString
goldenStartFromLaterPointLogging = do
  let fakeBlockHeaderHashRawBytes = "6161616161616161616161616161616161616161616161616161616161616161"
  fakeBlockHeaderHash <-
    either
      (error . show)
      pure
      $ C.deserialiseFromRawBytesHex
        (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader))
        fakeBlockHeaderHashRawBytes
  let logs =
        [ mkLastSyncLog fakeBlockHeaderHash 0 0 (Just 50000) Nothing
        , mkLastSyncLog fakeBlockHeaderHash 30 0 (Just 80000) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 10 0 (Just 90000) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 5 2 (Just 95000) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 5 1 (Just 99995) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 5 1 (Just 99999) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 5 3 (Just 100000) (Just 10)
        ]
  pure $ fromString $ Text.unpack $ Text.intercalate "\n" $ fmap (marconiFormatting . pretty) logs

mkLastSyncLog
  :: C.Hash C.BlockHeader -> Word64 -> Word64 -> Maybe Word64 -> Maybe Word64 -> SyncLog event
mkLastSyncLog blockHeaderHash nbProcessedBlocks nbProcessedRollbacks currentSyncedSlot timeSinceLastSync =
  SyncLog
    ( LastSyncStats
        nbProcessedBlocks
        nbProcessedRollbacks
        ( case currentSyncedSlot of
            Nothing -> C.ChainPointAtGenesis
            Just s -> C.ChainPoint (C.SlotNo s) blockHeaderHash
        )
        (C.ChainTip (C.SlotNo 100000) blockHeaderHash (C.BlockNo 100000))
        Nothing
    )
    nullTracer
    (fmap fromIntegral timeSinceLastSync)
