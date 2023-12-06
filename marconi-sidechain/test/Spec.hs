{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import Cardano.BM.Setup (withTrace)
import Cardano.BM.Tracing (defaultConfigTesting)
import Control.Monad.Reader (runReaderT)
import Marconi.ChainIndex.Legacy.Logging (mkMarconiTrace)
import Marconi.Sidechain.Api.HttpServer (marconiApp)
import Marconi.Sidechain.CLI (parseCliArgs)
import Marconi.Sidechain.CLI qualified as CLI
import Marconi.Sidechain.Env (SidechainEnv, mkSidechainEnv)
import Network.JsonRpc.Client.Types ()
import Network.Wai.Handler.Warp qualified as Warp
import Spec.Marconi.Sidechain.Api.Query.Indexers.MintBurn qualified as Api.Query.Indexers.MintBurn
import Spec.Marconi.Sidechain.Api.Query.Indexers.Utxo qualified as Api.Query.Indexers.Utxo
import Spec.Marconi.Sidechain.CLI qualified as CLI
import Spec.Marconi.Sidechain.CLIInputValidation qualified as CLIInputValidation
import Spec.Marconi.Sidechain.Env qualified as Env
import Spec.Marconi.Sidechain.Integration qualified as Integration
import Spec.Marconi.Sidechain.Routes qualified as Routes
import Spec.Marconi.Sidechain.RpcClientAction (RpcClientAction, mkRpcClientAction)
import System.Directory (createDirectory, getTemporaryDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withTempDirectory)
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

main :: IO ()
main = do
  tmp <- getTemporaryDirectory
  withTempDirectory tmp "marconi-sidechain" $ \tempDir -> do
    traceConfig <- defaultConfigTesting
    withTrace traceConfig "marconi-sidechain" $ \trace -> do
      let marconiTrace = mkMarconiTrace trace
      cliArgs <- mkCliArgs tempDir
      env <- mkSidechainEnv 2160 (CLI.httpPort cliArgs) Nothing Nothing cliArgs marconiTrace
      Warp.testWithApplication (pure $ marconiApp env) $ \port -> do
        rpcClientAction <- flip runReaderT env $ mkRpcClientAction port
        defaultMain $ tests env rpcClientAction

tests :: SidechainEnv -> RpcClientAction -> TestTree
tests env rpcClientAction =
  testGroup
    "marconi-sidechain"
    [ localOption (HedgehogTestLimit $ Just 200) $
        testGroup
          "marconi-sidechain"
          [ Env.tests env
          , CLI.tests
          , Routes.tests
          , Api.Query.Indexers.Utxo.tests rpcClientAction
          , Api.Query.Indexers.MintBurn.tests rpcClientAction
          ]
    , CLIInputValidation.tests
    , Integration.tests
    ]

mkCliArgs :: FilePath -> IO CLI.CliArgs
mkCliArgs tempDir = do
  let nodeConfigFile = tempDir </> "cardano-node-config.json"
      nodeStateDir = tempDir </> "cardano-node-state"
  writeFile nodeConfigFile "{}"
  createDirectory nodeStateDir
  parseCliArgs
    [ "--testnet-magic"
    , "1"
    , "--node-config-path"
    , nodeConfigFile
    , "--socket-path"
    , nodeStateDir </> "cardano-node.socket"
    , "--db-dir"
    , nodeStateDir
    ]
