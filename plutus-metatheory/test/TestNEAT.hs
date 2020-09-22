module Main where

import           Control.Monad.Except
import           Data.Coolean
import           Data.Either
import           Data.List

import           Language.PlutusCore
import           Language.PlutusCore.Evaluation.Machine.Ck
import           Language.PlutusCore.Generators.NEAT.Spec
import           Language.PlutusCore.Generators.NEAT.Type
import           Language.PlutusCore.Lexer
import           Language.PlutusCore.Normalize
import           Test.Tasty
import           Test.Tasty.HUnit

import           MAlonzo.Code.Main                         (checkKindAgda, checkTypeAgda, inferKindAgda, inferTypeAgda,
                                                            normalizeTypeAgda, runCKAgda, runLAgda, runTCEKCAgda,
                                                            runTCEKVAgda, runTCKAgda)
import           MAlonzo.Code.Scoped                       (deBruijnifyK, unDeBruijnifyK)

import           Language.PlutusCore.DeBruijn
import           Raw                                       hiding (TypeError, tynames)

main :: IO ()
main = defaultMain $ allTests defaultGenOptions

allTests :: GenOptions -> TestTree
allTests genOpts = testGroup "NEAT"
  [ testCaseGen "soundness"
      genOpts
      (Type ())
      prop_checkKindSound
  ]
{-
  , testCaseGen "normalization"
      genOpts
      (Type ())
      prop_normalizePreservesKind
  , testCaseGen "normalizationSound"
      genOpts
      (Type ())
      prop_normalizeTypeSound
  , testCaseGen "normalizationAgree"
      genOpts
      (Type ())
      prop_normalizeTypeSame
  , testCaseGen "kindInferAgree"
      genOpts
      (Type ())
      prop_kindInferSame
  , testCaseGen "typeInfer"
      genOpts
      (Type (),TyFunG (TyBuiltinG TyIntegerG) (TyBuiltinG TyIntegerG))
      prop_typeinfer
  , testCaseGen "typeCheck"
      genOpts
      (Type (),TyFunG (TyBuiltinG TyIntegerG) (TyBuiltinG TyIntegerG))
      prop_typecheck
  , testCaseGen "run_plcCK_vs_CK"
      genOpts
      (Type (),TyFunG (TyBuiltinG TyIntegerG) (TyBuiltinG TyIntegerG))
      prop_run_plcCK_vs_CK
   , testCaseGen "Agda model evaluators agree"
      genOpts
      (Type (),TyFunG (TyBuiltinG TyIntegerG) (TyBuiltinG TyIntegerG))
      (prop_runList [runLAgda,runCKAgda,runTCKAgda,runTCEKVAgda,runTCEKCAgda])
  ]
-}
-- check that Agda agrees that the given type is correct
prop_checkKindSound :: Kind () -> ClosedTypeG -> ExceptT TestFail Quote ()
prop_checkKindSound k tyG = do
   ty <- withExceptT GenError $ convertClosedType tynames k tyG
   tyDB <- withExceptT FVErrorP $ deBruijnTy ty
   withExceptT (const $ Ctrex (CtrexKindCheckFail k tyG)) $ liftEither $ checkKindAgda tyDB (deBruijnifyK (convK k))

{-
-- check that the Agda type normalizer doesn't mangle the kind
prop_normalizePreservesKind :: Kind ()
                            -> ClosedTypeG
                            -> ExceptT TestFail Quote ()
prop_normalizePreservesKind k tyG = do
  ty  <- withExceptT GenError $ convertClosedType tynames k tyG
  tyDB <- withExceptT FVErrorP $ deBruijnTy ty
  tyN <- withExceptT Ctrex $ case normalizeTypeAgda tyDB of
    Just tyN -> return tyN
    Nothing  -> throwError (CtrexTypeNormalizationFail k tyG)
  case checkKindAgda tyN (deBruijnifyK (convK k)) of
    Just _  -> return ()
    Nothing -> throwCtrex (CtrexKindPreservationFail k tyG)

-- compare the NEAT type normalizer against the Agda normalizer
prop_normalizeTypeSound :: Kind ()
                        -> ClosedTypeG
                        -> ExceptT TestFail Quote ()
prop_normalizeTypeSound k tyG = do
  ty <- withExceptT GenError $ convertClosedType tynames k tyG
  tyDB <- withExceptT FVErrorP $ deBruijnTy ty
  tyN1 <- withExceptT Ctrex $ case normalizeTypeAgda tyDB of
    Just tyN -> return tyN
    Nothing  -> throwError (CtrexTypeNormalizationFail k tyG)
  ty1 <- withExceptT FVErrorP $ unDeBruijnTy tyN1
  ty2 <- withExceptT GenError $ convertClosedType tynames k (normalizeTypeG tyG)
  unless (ty1 == ty2) $
    throwCtrex (CtrexNormalizeConvertCommuteTypes k tyG ty1 ty2)

-- compare the production type normalizer against the Agda type normalizer
prop_normalizeTypeSame :: Kind ()
                        -> ClosedTypeG
                        -> ExceptT TestFail Quote ()
prop_normalizeTypeSame k tyG = do
  ty <- withExceptT GenError $ convertClosedType tynames k tyG
  tyDB <- withExceptT FVErrorP $ deBruijnTy ty
  tyN1 <- withExceptT Ctrex $
    case normalizeTypeAgda tyDB of
      Just tyN -> return tyN
      Nothing  -> throwError (CtrexTypeNormalizationFail k tyG)
  ty1 <- withExceptT FVErrorP $ unDeBruijnTy tyN1
  ty2 <- withExceptT TypeError $ unNormalized <$> normalizeType ty
  unless (ty1 == ty2) $
    throwCtrex (CtrexTypeNormalizationMismatch k tyG ty1 ty2)

-- compare the production kind inference against the Agda
prop_kindInferSame :: Kind ()
                   -> ClosedTypeG
                   -> ExceptT TestFail Quote ()
prop_kindInferSame k tyG = do
  ty <- withExceptT GenError $ convertClosedType tynames k tyG
  tyDB <- withExceptT FVErrorP $ deBruijnTy ty
  k' <- withExceptT Ctrex $ case inferKindAgda tyDB of
    Just k' -> return k'
    Nothing -> throwError (CtrexKindCheckFail k tyG)
  k'' <- withExceptT TypeError $ inferKind defConfig ty
  unless (unconvK (unDeBruijnifyK k') == k'') $ throwCtrex (CtrexKindMismatch k tyG (unconvK (unDeBruijnifyK k')) k'')

-- try to infer the type of a term
prop_typeinfer :: (Kind (), ClosedTypeG) -> ClosedTermG -> ExceptT TestFail Quote ()
prop_typeinfer (k , tyG) tmG = do
  tm <- withExceptT GenError $ convertClosedTerm tynames names tyG tmG
  tmDB <- withExceptT FVErrorP $ deBruijnTerm tm
  withExceptT Ctrex $
    case inferTypeAgda tmDB of
      Just _  -> return ()
      Nothing -> throwError (CtrexTypeCheckFail tyG tmG)

-- try to typecheck a term
prop_typecheck :: (Kind (), ClosedTypeG) -> ClosedTermG -> ExceptT TestFail Quote ()
prop_typecheck (k , tyG) tmG = do
  ty <- withExceptT GenError $ convertClosedType tynames k tyG
  tyDB <- withExceptT FVErrorP $ deBruijnTy ty
  tm <- withExceptT GenError $ convertClosedTerm tynames names tyG tmG
  tmDB <- withExceptT FVErrorP $ deBruijnTerm tm
  case checkTypeAgda tyDB tmDB of
    Just _  -> return ()
    Nothing -> throwCtrex (CtrexTypeCheckFail tyG tmG)

prop_runCK :: (Kind (), ClosedTypeG) -> ClosedTermG -> ExceptT TestFail Quote ()
prop_runCK (k , tyG) tmG = do
  tm <- withExceptT GenError $ convertClosedTerm tynames names tyG tmG
  tmDB <- withExceptT FVErrorP $ deBruijnTerm tm
  case runCKAgda tmDB of
    Just _  -> return ()
    Nothing -> throwCtrex (CtrexTermEvaluationFail tyG tmG)

prop_runTCK :: (Kind (), ClosedTypeG) -> ClosedTermG -> ExceptT TestFail Quote ()
prop_runTCK (k , tyG) tmG = do
  tm <- withExceptT GenError $ convertClosedTerm tynames names tyG tmG
  tmDB <- withExceptT FVErrorP $ deBruijnTerm tm
  case runTCKAgda tmDB of
    Just _  -> return ()
    Nothing -> throwCtrex (CtrexTermEvaluationFail tyG tmG)

prop_run_plcCK_vs_CK :: (Kind (), ClosedTypeG) -> ClosedTermG -> ExceptT TestFail Quote ()
prop_run_plcCK_vs_CK (k , tyG) tmG = do
  tm <- withExceptT GenError $ convertClosedTerm tynames names tyG tmG
  tmPlcCK <- withExceptT CkP $ liftEither $ evaluateCk mempty tm
  tmDB <- withExceptT FVErrorP $ deBruijnTerm tm
  tmCK <- withExceptT Ctrex $
    case runCKAgda tmDB of
      Just tmCK -> return tmCK
      Nothing   -> throwError (CtrexTermEvaluationFail tyG tmG)
  tmCKN <- withExceptT FVErrorP $ unDeBruijnTerm tmCK
  unless (tmPlcCK == tmCKN) $ throwCtrex (CtrexTermEvaluationMismatch tyG tmG [tmPlcCK,tmCKN])

type TERM = Term TyDeBruijn DeBruijn DefaultUni ()

prop_runList :: [(TERM -> Maybe TERM)]
            -> (Kind (), ClosedTypeG)
            -> ClosedTermG -> ExceptT TestFail Quote ()
prop_runList evs (k , tyG) tmG = do
  tm <- withExceptT GenError $ convertClosedTerm tynames names tyG tmG
  tmDB <- withExceptT FVErrorP $ deBruijnTerm tm
  let tmEvsM = evs <*> [tmDB]
  tmEvs <- withExceptT Ctrex $ case sequence tmEvsM of
    Just vs -> return vs
    Nothing -> throwError (CtrexTermEvaluationFail tyG tmG)
  tmEvsN <- withExceptT FVErrorP $ sequence (unDeBruijnTerm <$> tmEvs)
  unless (length (nub tmEvsN) == 1) $ throwCtrex (CtrexTermEvaluationMismatch tyG tmG tmEvsN)
-}
