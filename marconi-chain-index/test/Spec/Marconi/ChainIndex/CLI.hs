{-# LANGUAGE OverloadedStrings #-}

module Spec.Marconi.ChainIndex.CLI (tests) where

import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative (
  ParserResult (CompletionInvoked, Failure, Success),
  defaultPrefs,
  execParserPure,
  renderFailure,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

import Marconi.ChainIndex.CLI (programParser)
import Test.Tasty.Hedgehog (testProperty)

import Cardano.Api qualified as C
import Control.Monad (join)
import Data.List qualified as List
import Data.List.NonEmpty (toList)
import Hedgehog (Property, annotate, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (linear)
import Marconi.ChainIndex.CLI qualified as CLI
import Options.Applicative qualified as Opt
import Test.Gen.Cardano.Api.Typed qualified as Gen

tests :: TestTree
tests =
  testGroup
    "marconi-chain-index CLI Specs"
    $ [ genTest commands
      | commands <-
          [ ["--disable-address-data"] -- invalid command
          , ["--help"] -- display help
          ]
      ]
      ++ [ testProperty
            "Check asset Parsing"
            parseAssets
         ]

parseAssets :: Property
parseAssets = property $ do
  assets <-
    forAll $
      Gen.list
        (linear 1 4)
        ((,) <$> Gen.genPolicyId <*> Gen.maybe Gen.genAssetName)
  let toString (p, an) = T.unpack $ case an of
        Nothing -> C.serialiseToRawBytesHexText p
        Just x ->
          C.serialiseToRawBytesHexText p
            <> "."
            <> C.serialiseToRawBytesHexText x
      assetsText = unwords $ toString <$> assets
      parsed =
        Opt.execParserPure
          (Opt.prefs mempty)
          (Opt.info CLI.commonMaybeTargetAssetParser mempty)
          ["--match-asset-id", assetsText]
      expected = Just $ List.nub assets
      result = toList <$> join (Opt.getParseResult parsed)
  annotate $ show parsed
  result === expected

-- | Test generate golden tests from the list of commands
genTest :: [T.Text] -> TestTree
genTest commands = do
  let goldenFile =
        T.unpack $
          "test/Spec/Golden/Cli/"
            <> T.intercalate "_" ("marconi-chain-index" : (T.replace "-" "_" <$> commands))
            <> ".help"

  goldenVsStringDiff
    (T.unpack $ T.unwords commands)
    (\expected actual -> ["diff", "--color=always", expected, actual])
    goldenFile
    (generateHelpScreen commands)

{- | Generate CLI tests and parse the help screen.

 Generated tests are incomplete CLI invocations that will result in
   * printing the CLI produced error
   * help screen
-}
generateHelpScreen :: [T.Text] -> IO ByteString
generateHelpScreen commands = do
  let parser = programParser "fake-sha" -- parameter is ignored in this test
      text = case execParserPure defaultPrefs parser (T.unpack <$> commands) of
        Failure failure -> failure
        Success _ -> error "Parser expected to fail"
        CompletionInvoked _ -> error "Parser expected to fail"
  pure $ fromStrict (encodeUtf8 . T.pack <$> fst $ renderFailure text "marconi-chain-index")
