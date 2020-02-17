{-# LANGUAGE OverloadedStrings #-}

module Evaluation.Golden
    ( test_golden
    ) where

import           Prelude                                    hiding (even)

import           Language.PlutusCore.StdLib.Data.Bool
import           Language.PlutusCore.StdLib.Data.Function
import           Language.PlutusCore.StdLib.Data.List
import           Language.PlutusCore.StdLib.Data.Nat
import           Language.PlutusCore.StdLib.Meta
import           Language.PlutusCore.StdLib.Meta.Data.Tuple
import           Language.PlutusCore.StdLib.Type

import           Language.PlutusCore
import           Language.PlutusCore.Evaluation.Machine.Cek
import           Language.PlutusCore.Generators.Interesting
import           Language.PlutusCore.MkPlc
import           Language.PlutusCore.Pretty

import           Control.Monad.Except
import qualified Data.ByteString.Lazy                       as BSL
import           Data.Text.Encoding                         (encodeUtf8)
import           Test.Tasty
import           Test.Tasty.Golden

evenAndOdd :: Tuple (Term TyName Name) ()
evenAndOdd = runQuote $ do
    let nat = _recursiveType natData

    evenn <- freshName () "even"
    oddd  <- freshName () "odd"

    let eoFunc b recc = do
          n <- freshName () "n"
          pure $
              LamAbs () n nat $
              Apply () (Apply () (TyInst () (Unwrap () (Var () n)) bool) b) $ Var () recc

    evenF <- FunctionDef () evenn (FunctionType () nat bool) <$> eoFunc true oddd
    oddF  <- FunctionDef () oddd  (FunctionType () nat bool) <$> eoFunc false evenn

    getMutualFixOf () (fixN 2 fixBy) [evenF, oddF]

even :: Term TyName Name ()
even = runQuote $ tupleTermAt () 0 evenAndOdd

evenAndOddList :: Tuple (Term TyName Name) ()
evenAndOddList = runQuote $ do
    let list = _recursiveType listData
        nat  = _recursiveType natData
        listNat = TyApp () list nat

    evenn <- freshName () "even"
    oddd  <- freshName () "odd"

    let eoFunc recc = do
          l <- freshName () "l"
          pure $
              LamAbs () l listNat $
              Apply () (
                  Apply () (TyInst () (Unwrap () (Var () l)) listNat)
                  (TyInst() nil nat))
              recc

    evenF <- FunctionDef () evenn (FunctionType () listNat listNat) <$> do
        h <- freshName () "head"
        t <- freshName () "tail"
        eoFunc $
            LamAbs () h nat $
            LamAbs () t listNat $
            Apply () (Apply () (TyInst () cons nat) (Var () h)) $
            Apply () (Var () oddd) (Var () t)

    oddF <- FunctionDef () oddd (FunctionType () listNat listNat) <$> do
        h <- freshName () "head"
        t <- freshName () "tail"
        eoFunc $
            LamAbs () h nat $
            LamAbs () t listNat $
            Apply () (Var () evenn) (Var () t)

    getMutualFixOf () (fixN 2 fixBy) [evenF, oddF]

evenList :: Term TyName Name ()
evenList = runQuote $ tupleTermAt () 0 evenAndOddList

smallNatList :: Term TyName Name ()
smallNatList = metaListToList nat nats where
    nats = map metaIntegerToNat [1,2,3]
    nat = _recursiveType natData

polyError :: Term TyName Name ()
polyError = runQuote $ do
    a <- freshTyName () "a"
    pure $ TyAbs () a (Type ()) $ Error () (TyVar () a)

goldenVsPretty :: PrettyPlc a => String -> ExceptT BSL.ByteString IO a -> TestTree
goldenVsPretty name value =
    goldenVsString name ("test/Evaluation/Golden/" ++ name ++ ".plc.golden") $
        either id (BSL.fromStrict . encodeUtf8 . docText . prettyPlcClassicDebug) <$> runExceptT value

goldenVsEvaluated :: String -> Term TyName Name () -> TestTree
goldenVsEvaluated name = goldenVsPretty name . pure . unsafeEvaluateCek mempty

-- TODO: ideally, we want to test this for all the machines.
test_golden :: TestTree
test_golden = testGroup "golden"
    [ goldenVsEvaluated "even2" $ Apply () even $ metaIntegerToNat 2
    , goldenVsEvaluated "even3" $ Apply () even $ metaIntegerToNat 3
    , goldenVsEvaluated "evenList" $ Apply () natSum $ Apply () evenList smallNatList
    , goldenVsEvaluated "polyError" $ polyError
    , goldenVsEvaluated "polyErrorInst" $ TyInst () polyError (TyBuiltin () TyInteger)
    ]
