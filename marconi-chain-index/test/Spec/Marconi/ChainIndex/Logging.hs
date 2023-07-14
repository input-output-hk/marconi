{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.ChainIndex.Logging (tests) where

import Cardano.Api qualified as C
import Data.ByteString.Lazy (ByteString)
import Data.Data (Proxy (Proxy))
import Data.String (fromString)
import Data.Text qualified as Text
import Data.Word (Word64)
import Marconi.ChainIndex.Logging (
  LastSyncLog (LastSyncLog),
  LastSyncStats (LastSyncStats),
  renderLastSyncLog,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

tests :: TestTree
tests =
  testGroup
    "Spec.Marconi.ChainIndex.Logging"
    [ goldenVsStringDiff
        "Golden test for syncing logs when starting from genesis"
        (\expected actual -> ["diff", "--color=always", expected, actual])
        "test/Spec/Marconi/ChainIndex/Logging/Golden/start-from-genesis-logging.txt"
        goldenStartFromGenesisLogging
    , goldenVsStringDiff
        "Golden test for syncing logs when starting from chain point other than genesis"
        (\expected actual -> ["diff", "--color=always", expected, actual])
        "test/Spec/Marconi/ChainIndex/Logging/Golden/start-from-later-point-logging.txt"
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
        , mkLastSyncLog fakeBlockHeaderHash 10 1 (Just 100) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 40 0 (Just 500) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 30 0 (Just 800) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 10 0 (Just 900) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 5 2 (Just 950) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 5 3 (Just 1000) (Just 10)
        ]
  pure $ fromString $ Text.unpack $ Text.intercalate "\n" $ fmap renderLastSyncLog logs

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
        [ mkLastSyncLog fakeBlockHeaderHash 0 0 (Just 500) Nothing
        , mkLastSyncLog fakeBlockHeaderHash 30 0 (Just 800) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 10 0 (Just 900) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 5 2 (Just 950) (Just 10)
        , mkLastSyncLog fakeBlockHeaderHash 5 3 (Just 1000) (Just 10)
        ]
  pure $ fromString $ Text.unpack $ Text.intercalate "\n" $ fmap renderLastSyncLog logs

mkLastSyncLog
  :: C.Hash C.BlockHeader -> Word64 -> Word64 -> Maybe Word64 -> Maybe Word64 -> LastSyncLog
mkLastSyncLog blockHeaderHash nbProcessedBlocks nbProcessedRollbacks currentSyncedSlot timeSinceLastSync =
  LastSyncLog
    ( LastSyncStats
        nbProcessedBlocks
        nbProcessedRollbacks
        ( case currentSyncedSlot of
            Nothing -> C.ChainPointAtGenesis
            Just s -> C.ChainPoint (C.SlotNo s) blockHeaderHash
        )
        (C.ChainTip (C.SlotNo 1000) blockHeaderHash (C.BlockNo 1000))
        Nothing
    )
    (fmap fromIntegral timeSinceLastSync)
