{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module PlutusTx.Evaluation
    ( evaluateCek
    , unsafeEvaluateCek
    , evaluateCekTrace
    , ErrorWithCause(..)
    , EvaluationError(..)
    , CekExTally
    , TallyingSt(..)
    , CekEvaluationException
    )
where

import qualified PlutusCore                               as PLC
import           PlutusCore.Default
import           PlutusCore.Name

import           UntypedPlutusCore
import           UntypedPlutusCore.Evaluation.Machine.Cek hiding (evaluateCek, unsafeEvaluateCek)
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as Cek

import           Data.Text                                (Text)

-- We do not use qualified import because the whole module contains off-chain code
import           Prelude                                  as Haskell

-- | Evaluate a program in the CEK machine with the usual text dynamic builtins.
evaluateCek
    :: (uni ~ DefaultUni, fun ~ DefaultFun)
    => Program Name uni fun () -> Either (CekEvaluationException uni fun) (Term Name uni fun ())
evaluateCek (Program _ _ t) = Cek.evaluateCekNoEmit PLC.defaultCekParameters t

-- | Evaluate a program in the CEK machine with the usual text dynamic builtins. May throw.
unsafeEvaluateCek
    :: (uni ~ DefaultUni, fun ~ DefaultFun)
    => Program Name uni fun () -> EvaluationResult (Term Name uni fun ())
unsafeEvaluateCek (Program _ _ t) = Cek.unsafeEvaluateCekNoEmit PLC.defaultCekParameters t

-- | Evaluate a program in the CEK machine with the usual text dynamic builtins and tracing, additionally
-- returning the trace output.
evaluateCekTrace
    :: (uni ~ DefaultUni, fun ~ DefaultFun)
    => Program Name uni fun ()
    -> ([Text], TallyingSt fun, Either (CekEvaluationException uni fun) (Term Name uni fun ()))
evaluateCekTrace (Program _ _ t) =
    case runCek PLC.defaultCekParameters Cek.tallying Cek.logEmitter t of
        (errOrRes, st, logs) -> (logs, st, errOrRes)
