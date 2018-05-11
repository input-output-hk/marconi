{-# LANGUAGE OverloadedStrings #-}

module Main ( main
            ) where

import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable        (fold, traverse_)
import           Data.Text.Encoding   (encodeUtf8)
import           Hedgehog             hiding (Var)
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range
import           Language.PlutusCore
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit

main :: IO ()
main = traverse_ defaultMain [tests, propertyTests]

genVersion :: MonadGen m => m (Version AlexPosn)
genVersion = Version emptyPosn <$> int' <*> int' <*> int'
    where int' = Gen.integral_ (Range.linear 0 10)

genName :: MonadGen m => m (Name AlexPosn)
genName = Name emptyPosn <$> name' <*> int'
    where int' = Unique <$> Gen.int (Range.linear 0 3000)
          name' = BSL.fromStrict <$> Gen.utf8 (Range.linear 0 20) Gen.lower

genTerm :: MonadGen m => m (Term AlexPosn)
genTerm = Gen.choice [varGen]
    where varGen = Var emptyPosn <$> genName

genProgram :: MonadGen m => m (Program AlexPosn)
genProgram = Program emptyPosn <$> genVersion <*> genTerm

emptyPosn :: AlexPosn
emptyPosn = AlexPn 0 0 0

propParser :: Property
propParser = property $ do
    prog <- forAll genProgram
    let nullPosn = fmap (pure emptyPosn)
    Right (nullPosn prog) === (nullPosn <$> parse (BSL.fromStrict $ encodeUtf8 $ prettyText prog))

propertyTests :: TestTree
propertyTests = testGroup "property tests"
    [ testProperty "property test" propParser
    ]

tests :: TestTree
tests = testCase "builtin" $ fold
    [ format "(program 0.1.0 [(builtin addInteger) x y])" @?= Right "(program 0.1.0 [ (builtin addInteger) x y ])"
    , format "(program 0.1.0 doesn't)" @?= Right "(program 0.1.0 doesn't)"
    ]
