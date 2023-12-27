{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Spec.Marconi.Cardano.ChainIndex.CLIInputValidation where

import Data.ByteString.Lazy qualified as BSL
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import System.FilePath.Posix ((</>))
import System.Process qualified as IO
import Test.Marconi.Cardano.ChainIndex.CLI qualified as Test.CLI
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

tests :: TestTree
tests =
  testGroup
    "marconi-cardano-chain-index input validation error"
    [ testGroup "check input validation when starting marconi-cardano-chain-index with invalid cli flags" $
        invalidOptionalCliFlagsWithExpectedErrors
          <&> ( \testInput ->
                  goldenVsString
                    (testDescription testInput)
                    (goldenFile testInput)
                    (invalidCliArgWhenStartIngMarconiChainIndexTest testInput)
              )
    ]

data InvalidArgTestInput = InvalidArgTestInput
  { testDescription :: String
  , invalidFlag :: String
  , invalidArg :: String
  , goldenFile :: FilePath
  }

goldenFileDir :: FilePath
goldenFileDir = "test/Spec/Golden/CLIInputValidation"

invalidOptionalCliFlagsWithExpectedErrors :: [InvalidArgTestInput]
invalidOptionalCliFlagsWithExpectedErrors =
  [ InvalidArgTestInput
      { testDescription = "address-to-index 1 char short"
      , invalidFlag = "--addresses-to-index"
      , invalidArg =
          "addr1x8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gt"
            ++ "7r0vd4msrxnuwnccdxlhdjar77j6lg0wypcc9uar5d2shskhj42"
      , goldenFile = goldenFileDir </> "invalidAddress_1CharShort.golden"
      }
  , InvalidArgTestInput
      "address-to-index missing Bech32 prefix"
      "--addresses-to-index"
      ( "x8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gt7r0"
          ++ "vd4msrxnuwnccdxlhdjar77j6lg0wypcc9uar5d2shskhj42g"
      )
      (goldenFileDir </> "invalidAddress-MissingBech32Prefix.golden")
  , InvalidArgTestInput
      "address-to-index not address format"
      "--addresses-to-index"
      "notAnAddress"
      (goldenFileDir </> "invalidAddress-badFormat.golden")
  , InvalidArgTestInput
      "address-to-index using stake_test prefix"
      "--addresses-to-index"
      "stake_test17rphkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcljw6kf"
      (goldenFileDir </> "invalidAddressPrefix-stake_test.golden")
  , InvalidArgTestInput
      "address-to-index using stake1 prefix"
      "--addresses-to-index"
      "stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw"
      (goldenFileDir </> "invalidAddressPrefix-stake1.golden")
  , InvalidArgTestInput
      "match-asset-id policyId is 1 byte short"
      "--match-asset-id"
      "1cdc58c3b6d1ab11dd047ac9e3a2ec26aabf0839abe37b791cb810"
      (goldenFileDir </> "invalidAssetId-policyId-1ByteShort.golden") -- PLT-7086
  , InvalidArgTestInput
      "match-asset-id policyId is 1 byte long"
      "--match-asset-id"
      "1cdc58c3b6d1ab11dd047ac9e3a2ec26aabf0839abe37b791cb810f2f2"
      (goldenFileDir </> "invalidAssetId-policyId-1ByteLong.golden") -- PLT-7086
  , InvalidArgTestInput
      "match-asset-id policy id is invalid hex, 1 char short"
      "--match-asset-id"
      "1cdc58c3b6d1ab11dd047ac9e3a2ec26aabf0839abe37b791cb810f"
      (goldenFileDir </> "invalidAssetId-policyId-1HexCharShort.golden")
  , InvalidArgTestInput
      "match-asset-id policy id is invalid hex, 1 char long"
      "--match-asset-id"
      "1cdc58c3b6d1ab11dd047ac9e3a2ec26aabf0839abe37b791cb810f2f"
      (goldenFileDir </> "invalidAssetId-policyId-1HexCharLong.golden")
  , InvalidArgTestInput
      "match-asset-id policy id is not Base16 format but correct length"
      "--match-asset-id"
      "00aPolicyIdOfCorrectLength000000000000000000000000000000000000"
      (goldenFileDir </> "invalidAssetId-policyId-badBase16Format.golden")
  , InvalidArgTestInput
      "match-asset-id asset name is too long"
      "--match-asset-id"
      ( "1cdc58c3b6d1ab11dd047ac9e3a2ec26aabf0839abe37b791cb810f2."
          ++ "deadbeef0000000000000000000000000000000000000000000000000000000000"
      )
      (goldenFileDir </> "invalidAssetId-assetName-tooLong.golden")
  , InvalidArgTestInput
      "match-asset-id asset name is not valid Base16 length"
      "--match-asset-id"
      "1cdc58c3b6d1ab11dd047ac9e3a2ec26aabf0839abe37b791cb810f2.deadbeeef"
      (goldenFileDir </> "invalidAssetId-assetName-invalidBase16Length.golden")
  , InvalidArgTestInput
      "match-asset-id asset name is not Base16 format"
      "--match-asset-id"
      "1cdc58c3b6d1ab11dd047ac9e3a2ec26aabf0839abe37b791cb810f2.0000notAToken0"
      (goldenFileDir </> "invalidAssetId-assetName-invalidBase16Format.golden")
  ]

invalidCliArgWhenStartIngMarconiChainIndexTest :: InvalidArgTestInput -> IO BSL.ByteString
invalidCliArgWhenStartIngMarconiChainIndexTest InvalidArgTestInput{..} = do
  (_mStdin, mStdout, mStderr, _processHandle) <-
    IO.createProcess
      . ( \cp ->
            cp
              { IO.std_out = IO.CreatePipe
              , IO.std_err = IO.CreatePipe
              }
        )
      =<< Test.CLI.procFlex "marconi-cardano-chain-index" "MARCONI_CHAIN_INDEX" [invalidFlag, invalidArg]
  Test.CLI.captureHandleContents (fromJust mStderr)
