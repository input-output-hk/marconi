{-# LANGUAGE NumericUnderscores #-}

module Test.Marconi.Cardano.ChainIndex.Api.HttpServer where

import Cardano.Api qualified as C
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, throwIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Marconi.Core.JsonRpc (ReaderHandler)
import Network.JsonRpc.Types (JsonRpcErr)
import Test.Gen.Marconi.Cardano.Core.Mockchain qualified as Mockchain
import Test.Marconi.Cardano.ChainIndex.Indexers qualified as Test.Indexers

{- | Wrapper for instantiating indexers, indexing mockchain events, querying via a handler,
and closing the indexers.
-}
queryHandlerWithIndexers
  :: Mockchain.MockchainWithInfoAndDistance C.BabbageEra
  -> IO (config, Test.Indexers.TestBuildIndexersResult)
  -> ReaderHandler config (Either (JsonRpcErr String) result)
  -> IO result
queryHandlerWithIndexers chain buildAction queryAction = bracket buildAction (Test.Indexers.closeIndexers . snd) $
  \(httpConfig, indexersConfig) -> do
    Test.Indexers.indexAllWithMockchain indexersConfig chain
    threadDelay 500_000
    runReaderT (runExceptT queryAction) httpConfig
      >>= either throwIO pure
      >>= either (fail . show) pure
