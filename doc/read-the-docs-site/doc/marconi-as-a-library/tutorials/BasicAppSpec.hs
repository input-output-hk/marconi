{-# LANGUAGE NumericUnderscores #-}

module Main where

import BasicApp (runBlockInfoSqliteIndexerHttp)
import Cardano.Api qualified as C
import Cardano.Node.Socket.Emulator (startTestnet)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Hedgehog (Property)
import Hedgehog qualified as H
import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.Hedgehog (testPropertyNamed)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testPropertyNamed "..." "endToEndBlockInfo" endToEndBlockInfo

endToEndBlockInfo :: Property
endToEndBlockInfo = H.property $ do
  let socketFilePath = "/tmp/cardano-node.socket"
      -- TODO This should be moved to cardano-node-emulator (currently in plutus-ledger)
      networkId = C.Testnet $ C.NetworkMagic 1097911063

  H.evalIO $ startTestnet socketFilePath 1 networkId

  (_, resp) <- H.evalIO
    $ Async.withAsync
      (liftIO $ fmap (const Nothing) $ runBlockInfoSqliteIndexerHttp socketFilePath networkId)
    $ \a ->
      Async.withAsync runQuery $ \b ->
        Async.waitAny [a, b]

  H.assert $ isJust resp
  where
    runQuery = do
      -- TODO Write this part
      threadDelay 5_000_000
      pure undefined
