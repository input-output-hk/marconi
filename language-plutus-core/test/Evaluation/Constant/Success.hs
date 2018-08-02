module Evaluation.Constant.Success
    ( test_applyBuiltinNameSuccess
    ) where

import           Language.PlutusCore.Constant
import           Evaluation.Constant.AllTypedBuiltinSized
import           Evaluation.Constant.Apply

import           Data.Semigroup
import qualified Data.ByteString.Lazy as BSL
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog

test_applyBuiltinNameSuccess :: TestTree
test_applyBuiltinNameSuccess =
    testGroup "applyBuiltinNameSuccess"
        [ test_typedAddIntegerSuccess
        , test_typedSubtractIntegerSuccess
        , test_typedMultiplyIntegerSuccess
        , test_typedDivideIntegerSuccess
        , test_typedRemainderIntegerSuccess
        , test_typedLessThanIntegerSuccess
        , test_typedLessThanEqIntegerSuccess
        , test_typedGreaterThanIntegerSuccess
        , test_typedGreaterThanEqIntegerSuccess
        , test_typedEqIntegerSuccess
        , test_typedConcatenateSuccess
        , test_typedTakeByteStringSuccess
        , test_typedDropByteStringSuccess
        , test_typedEqByteStringSuccess
        ]

test_typedAddIntegerSuccess :: TestTree
test_typedAddIntegerSuccess
    = testProperty "typedAddInteger"
    $ prop_applyBuiltinNameSuccess typedAddInteger (+)
    $ allTypedBuiltinSizedIntSum

test_typedSubtractIntegerSuccess :: TestTree
test_typedSubtractIntegerSuccess
    = testProperty "typedSubtractInteger"
    $ prop_applyBuiltinNameSuccess typedSubtractInteger (-)
    $ allTypedBuiltinSizedIntSum

test_typedMultiplyIntegerSuccess :: TestTree
test_typedMultiplyIntegerSuccess
    = testProperty "typedMultiplyInteger"
    $ prop_applyBuiltinNameSuccess typedMultiplyInteger (*)
    $ updateAllTypedBuiltinSized TypedBuiltinSizedInt
          (\low high -> Gen.integral $ Range.linear (negate . isqrt . abs $ low) (isqrt high))
    $ allTypedBuiltinSizedDef

test_typedDivideIntegerSuccess :: TestTree
test_typedDivideIntegerSuccess
    = testProperty "typedDivideInteger"
    $ prop_applyBuiltinNameSuccess typedDivideInteger div
    $ allTypedBuiltinSizedIntDiv

test_typedRemainderIntegerSuccess :: TestTree
test_typedRemainderIntegerSuccess
    = testProperty "typedRemainderInteger"
    $ prop_applyBuiltinNameSuccess typedRemainderInteger mod
    $ allTypedBuiltinSizedIntDiv

test_typedLessThanIntegerSuccess :: TestTree
test_typedLessThanIntegerSuccess
    = testProperty "typedLessThanInteger"
    $ prop_applyBuiltinNameSuccess typedLessThanInteger (<)
    $ allTypedBuiltinSizedDef

test_typedLessThanEqIntegerSuccess :: TestTree
test_typedLessThanEqIntegerSuccess
    = testProperty "typedLessThanEqInteger"
    $ prop_applyBuiltinNameSuccess typedLessThanEqInteger (<=)
    $ allTypedBuiltinSizedDef

test_typedGreaterThanIntegerSuccess :: TestTree
test_typedGreaterThanIntegerSuccess
    = testProperty "typedGreaterThanInteger"
    $ prop_applyBuiltinNameSuccess typedGreaterThanInteger (>)
    $ allTypedBuiltinSizedDef

test_typedGreaterThanEqIntegerSuccess :: TestTree
test_typedGreaterThanEqIntegerSuccess
    = testProperty "typedGreaterThanEqInteger"
    $ prop_applyBuiltinNameSuccess typedGreaterThanEqInteger (>=)
    $ allTypedBuiltinSizedDef

test_typedEqIntegerSuccess :: TestTree
test_typedEqIntegerSuccess
    = testProperty "typedEqInteger"
    $ prop_applyBuiltinNameSuccess typedEqInteger (==)
    $ allTypedBuiltinSizedDef

test_typedConcatenateSuccess :: TestTree
test_typedConcatenateSuccess
    = testProperty "typedConcatenate"
    $ prop_applyBuiltinNameSuccess typedConcatenate (<>)
    $ updateAllTypedBuiltinSized TypedBuiltinSizedBS
          (\high -> Gen.bytes $ Range.linear 0 (high `div` 2))
    $ allTypedBuiltinSizedDef

test_typedTakeByteStringSuccess :: TestTree
test_typedTakeByteStringSuccess
    = testProperty "typedTakeByteString"
    $ prop_applyBuiltinNameSuccess typedTakeByteString (BSL.take . fromIntegral)
    $ allTypedBuiltinSizedDef

test_typedDropByteStringSuccess :: TestTree
test_typedDropByteStringSuccess
    = testProperty "typedDropByteString"
    $ prop_applyBuiltinNameSuccess typedDropByteString (BSL.drop . fromIntegral)
    $ allTypedBuiltinSizedDef

test_typedEqByteStringSuccess :: TestTree
test_typedEqByteStringSuccess
    = testProperty "typedEqByteString"
    $ prop_applyBuiltinNameSuccess typedEqByteString (==)
    $ allTypedBuiltinSizedDef

isqrt :: Integer -> Integer
isqrt n
    | n < 0     = error "isqrt: negative number"
    | n <= 1    = n
    | otherwise = head $ dropWhile (not . isRoot) iters
    where
        sqr :: Integer -> Integer
        sqr = (^ (2 :: Int))
        twopows = iterate sqr 2
        (lowerRoot, lowerN) = last. takeWhile ((n >=) . snd) $ zip (1 : twopows) twopows
        newtonStep x = div (x + n `div` x) 2
        iters = iterate newtonStep (isqrt (n `div` lowerN) * lowerRoot)
        isRoot r = sqr r <= n && n < sqr (r+1)
