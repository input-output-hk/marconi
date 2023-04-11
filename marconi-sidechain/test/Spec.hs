{-# LANGUAGE ExplicitNamespaces #-}
module Main (main) where

import Marconi.Sidechain.Api.HttpServer (marconiApp)
import Marconi.Sidechain.Bootstrap (initializeSidechainEnv)
import Network.JsonRpc.Client.Types ()
import Network.Wai.Handler.Warp qualified as Warp
import Spec.Marconi.Sidechain.Api.Query.Indexers.Utxo qualified as Api.Query.Indexers.Utxo
import Spec.Marconi.Sidechain.CLI qualified as CLI
import Spec.Marconi.Sidechain.Routes qualified as Routes
import Spec.Marconi.Sidechain.RpcClientAction (RpcClientAction, mkRpcClientAction)
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

main :: IO ()
main = do
    env <- initializeSidechainEnv Nothing Nothing
    Warp.testWithApplication (pure $ marconiApp env) $ \port -> do
        rpcClientAction <- mkRpcClientAction env port
        defaultMain $ tests rpcClientAction

tests :: RpcClientAction -> TestTree
tests rpcClientAction = localOption (HedgehogTestLimit $ Just 200) $
    testGroup "marconi-sidechain"
        [ CLI.tests
        , Routes.tests
        , Api.Query.Indexers.Utxo.tests rpcClientAction
        ]
