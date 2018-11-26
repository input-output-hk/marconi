module Language.PlutusTx.Evaluation (evaluateCek, evaluateCekLog) where

import           Language.PlutusCore
import           Language.PlutusCore.Constant
import           Language.PlutusCore.Constant.Dynamic
import           Language.PlutusCore.Interpreter.CekMachine hiding (evaluateCek)

import           Control.Exception

import           System.IO.Unsafe

stringBuiltins :: DynamicBuiltinNameMeanings
stringBuiltins =
    insertDynamicBuiltinNameDefinition dynamicCharToStringDefinition $ insertDynamicBuiltinNameDefinition dynamicAppendDefinition mempty

-- TODO: pretty sure we shouldn't need the unsafePerformIOs here, we should expose a pure interface even if it has IO hacks under the hood

-- | Evaluate a program in the CEK machine with the usual string dynamic builtins.
evaluateCek
    :: Program TyName Name ()
    -> EvaluationResult
evaluateCek p = unsafePerformIO $ evaluate $ runCek stringBuiltins p

-- | Evaluate a program in the CEK machine with the usual string dynamic builtins and tracing, additionally
-- returning the trace output.
evaluateCekLog
    :: Program TyName Name ()
    -> ([String], EvaluationResult)
evaluateCekLog p =
    unsafePerformIO $ withEmit $ \emit -> do
        let logName       = dynamicTraceName
            logDefinition = dynamicCallAssign TypedBuiltinDyn logName emit
            env  = insertDynamicBuiltinNameDefinition logDefinition stringBuiltins
        evaluate $ runCek env p
