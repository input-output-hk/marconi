{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Spec.Marconi.Sidechain.CLIInputValidation where

import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Hedgehog (Property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Spec.Marconi.Sidechain.Utils qualified as U
import System.IO qualified as IO
import System.Process qualified as IO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "marconi-sidechain input validation error"
    [ testGroup "check input validation when starting marconi-sidechain with invalid cli flags" $
        invalidOptionalCliFlagsWithExpectedErrors
          <&> ( \testInput ->
                  testPropertyNamed
                    (testDescription testInput)
                    "invalidCliArgWhenStartIngMarconiSideChainTest"
                    (invalidCliArgWhenStartIngMarconiSideChainTest testInput)
              )
    ]

data InvalidArgTestInput = InvalidArgTestInput
  { testDescription :: String
  , invalidFlag :: String
  , invalidArg :: String
  , expectedErrors :: [String]
  }

invalidAddressError :: [String]
invalidAddressError = ["Invalid address (not a valid Bech32 address representation)"]

invalidBytestringSizeError :: [String]
invalidBytestringSizeError = ["RawBytesHexErrorBase16DecodeFail", "invalid bytestring size"]

invalidCharAtOffsetError :: Int -> [String]
invalidCharAtOffsetError n = ["RawBytesHexErrorBase16DecodeFail", "invalid character at offset: " ++ show n]

invalidOptionalCliFlagsWithExpectedErrors :: [InvalidArgTestInput]
invalidOptionalCliFlagsWithExpectedErrors =
  [ InvalidArgTestInput
      { testDescription = "address-to-index 1 char short"
      , invalidFlag = "--addresses-to-index"
      , invalidArg =
          "addr1x8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gt7r0vd4msrxnuwnccdxlhdjar77j6lg0wypcc9uar5d2shskhj42"
      , expectedErrors = invalidAddressError
      }
  , InvalidArgTestInput
      "address-to-index missing Bech32 prefix"
      "--addresses-to-index"
      "x8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gt7r0vd4msrxnuwnccdxlhdjar77j6lg0wypcc9uar5d2shskhj42g"
      invalidAddressError
  , InvalidArgTestInput
      "address-to-index not address format"
      "--addresses-to-index"
      "notAnAddress"
      invalidAddressError
  , InvalidArgTestInput
      "address-to-index using stake_test prefix"
      "--addresses-to-index"
      "stake_test17rphkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcljw6kf"
      invalidAddressError
  , InvalidArgTestInput
      "address-to-index using stake1 prefix"
      "--addresses-to-index"
      "stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw"
      invalidAddressError
  , InvalidArgTestInput
      "match-asset-id policyId is 1 byte short"
      "--match-asset-id"
      "1cdc58c3b6d1ab11dd047ac9e3a2ec26aabf0839abe37b791cb810"
      [ "RawBytesHexErrorRawBytesDecodeFail"
      , "unSerialiseAsRawBytesError = \"Enable to deserialise ScriptHash\"" -- error could be improved because is also "invalid bytestring size"
      ]
  , InvalidArgTestInput
      "match-asset-id policyId is 1 byte long"
      "--match-asset-id"
      "1cdc58c3b6d1ab11dd047ac9e3a2ec26aabf0839abe37b791cb810f2f2"
      [ "RawBytesHexErrorRawBytesDecodeFail"
      , "unSerialiseAsRawBytesError = \"Enable to deserialise ScriptHash\"" -- error could be improved because is also "invalid bytestring size"
      ]
  , InvalidArgTestInput
      "match-asset-id policy id is invalid hex, 1 char short"
      "--match-asset-id"
      "1cdc58c3b6d1ab11dd047ac9e3a2ec26aabf0839abe37b791cb810f"
      invalidBytestringSizeError
  , InvalidArgTestInput
      "match-asset-id policy id is invalid hex, 1 char long"
      "--match-asset-id"
      "1cdc58c3b6d1ab11dd047ac9e3a2ec26aabf0839abe37b791cb810f2f"
      invalidBytestringSizeError
  , InvalidArgTestInput
      "match-asset-id policy id is not Base16 format but correct length"
      "--match-asset-id"
      "00aPolicyIdOfCorrectLength000000000000000000000000000000000000"
      (invalidCharAtOffsetError 3)
  , InvalidArgTestInput
      "match-asset-id asset id is too long"
      "--match-asset-id"
      "1cdc58c3b6d1ab11dd047ac9e3a2ec26aabf0839abe37b791cb810f2.deadbeef0000000000000000000000000000000000000000000000000000000000"
      [ "RawBytesHexErrorRawBytesDecodeFail"
      , "unSerialiseAsRawBytesError = \"Unable to deserialise AssetName (the bytestring should be no longer than 32 bytes long which corresponds to a hex representation of 64 characters)\""
      ]
  , InvalidArgTestInput
      "match-asset-id asset id is not valid Base16 length"
      "--match-asset-id"
      "1cdc58c3b6d1ab11dd047ac9e3a2ec26aabf0839abe37b791cb810f2.deadbeefy"
      invalidBytestringSizeError
  , InvalidArgTestInput
      "match-asset-id asset id is not Base16 format"
      "--match-asset-id"
      "1cdc58c3b6d1ab11dd047ac9e3a2ec26aabf0839abe37b791cb810f2.0000notAToken0"
      (invalidCharAtOffsetError 4)
  ]

invalidCliArgWhenStartIngMarconiSideChainTest :: InvalidArgTestInput -> Property
invalidCliArgWhenStartIngMarconiSideChainTest InvalidArgTestInput{..} =
  U.integrationTest $ H.workspace "marconi-sidechain-input-validation" $ \tempPath -> do
    stdoutFile <- H.noteTempFile tempPath "marconi-sidechain.stdout.log"
    stderrFile <- H.noteTempFile tempPath "marconi-sidechain.stderr.log"
    hStdout <- H.openFile stdoutFile IO.WriteMode
    hStderr <- H.openFile stderrFile IO.WriteMode
    _ <-
      H.createProcess
        . ( \cp ->
              cp
                { IO.std_out = IO.UseHandle hStdout
                , IO.std_err = IO.UseHandle hStderr
                }
          )
        =<< H.procFlex "marconi-sidechain" "MARCONI_SIDECHAIN" [invalidFlag, invalidArg]

    validationErrorsOccurred <- H.evalIO $ U.waitForLog stderrFile 1 expectedErrors
    H.annotate "stdout content: " >> liftIO (IO.readFile stdoutFile) >>= H.annotate
    H.annotate "stderr content: " >> liftIO (IO.readFile stderrFile) >>= H.annotate
    validationErrorsOccurred === True
