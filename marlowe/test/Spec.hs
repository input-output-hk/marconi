{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import qualified Spec.Marlowe.AutoExecute
import qualified Spec.Marlowe.Marlowe
-- import qualified Spec.PAB.Workflow

import           Test.Tasty
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Marlowe"
    [ testGroup "Contracts" [ Spec.Marlowe.Marlowe.tests
                            , Spec.Marlowe.AutoExecute.tests
-- Does not work when invoking it from nix
--                            , testProperty "Correct Show instance for Contract"
--                                           Spec.Marlowe.Marlowe.prop_showWorksForContracts
                            ]
    , testGroup "Static Analysis"
        [ testProperty "No false positives" Spec.Marlowe.Marlowe.prop_noFalsePositives
        ]
    , testGroup "Marlowe JSON"
        [ testProperty "Serialise deserialise loops" Spec.Marlowe.Marlowe.prop_jsonLoops
        ]
    -- TODO: Will be reinstated very soon; have to comment out so that the
    -- mac-mini tests don't time out :(
    -- , testGroup "PAB Workflow"
    --   [ Spec.PAB.Workflow.tests
    --   ]
    ]
