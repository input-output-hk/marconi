{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

-- TODO: PLT-8095
tests :: TestTree
tests =
  testGroup
    "marconi-cardano-core"
    []
