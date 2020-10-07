-- | Kind/type inference/checking.

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Language.PlutusCore.TypeCheck
    (
    -- * Configuration.
      BuiltinTypes (..)
    , TypeCheckConfig (..)
    , tccBuiltinTypes
    , builtinMeaningsToTypes
    , getDefTypeCheckConfig
    -- * Kind/type inference/checking.
    , inferKind
    , checkKind
    , inferType
    , checkType
    , inferTypeOfProgram
    , checkTypeOfProgram
    ) where

import           Language.PlutusCore.Builtins
import           Language.PlutusCore.Constant
import           Language.PlutusCore.Core
import           Language.PlutusCore.Error
import           Language.PlutusCore.Name
import           Language.PlutusCore.Normalize
import           Language.PlutusCore.Quote
import           Language.PlutusCore.Rename
import           Language.PlutusCore.TypeCheck.Internal
import           Language.PlutusCore.Universe

import           Control.Monad.Except
import           Data.Ix

-- | Extract the 'TypeScheme' from a 'BuiltinMeaning' and convert it to the
-- corresponding @Type TyName@ for each row of a 'BuiltinMeanings'.
builtinMeaningsToTypes
    :: ( uni ~ UniOf term, AsTypeError err (Term TyName Name uni fun ()) uni fun ann
       , MonadError err m, MonadQuote m
       , Bounded fun, Enum fun, Ix fun
       )
    => ann -> (fun -> BuiltinMeaning term dyn cost) -> m (BuiltinTypes uni fun)
builtinMeaningsToTypes ann mean =
    fmap (BuiltinTypes . Just) . sequence . universeMapping $ \fun -> case mean fun of
        BuiltinMeaning sch _ _ -> do
            let ty = typeSchemeToType sch
            _ <- inferKind (TypeCheckConfig $ BuiltinTypes Nothing) $ ann <$ ty
            pure <$> normalizeType ty

getDefTypeCheckConfig
    :: ( uni ~ DefaultUni, fun ~ DefaultFun
       , MonadError err m, MonadQuote m
       , AsTypeError err (Term TyName Name uni fun ()) uni fun ann
       )
    => ann -> m (TypeCheckConfig uni fun)
getDefTypeCheckConfig ann =
    fmap TypeCheckConfig . builtinMeaningsToTypes ann $
        defaultFunMeaning @_ @(Term TyName Name _ _ ())

-- | Infer the kind of a type.
inferKind
    :: (AsTypeError err term uni fun ann, MonadError err m, MonadQuote m)
    => TypeCheckConfig uni fun -> Type TyName uni ann -> m (Kind ())
inferKind config = runTypeCheckM config . inferKindM

-- | Check a type against a kind.
-- Infers the kind of the type and checks that it's equal to the given kind
-- throwing a 'TypeError' (annotated with the value of the @ann@ argument) otherwise.
checkKind
    :: (AsTypeError err term uni fun ann, MonadError err m, MonadQuote m)
    => TypeCheckConfig uni fun -> ann -> Type TyName uni ann -> Kind () -> m ()
checkKind config ann ty = runTypeCheckM config . checkKindM ann ty

-- | Infer the type of a term.
inferType
    :: ( AsTypeError err (Term TyName Name uni fun ()) uni fun ann, MonadError err m, MonadQuote m
       , GShow uni, GEq uni, Ix fun
       )
    => TypeCheckConfig uni fun -> Term TyName Name uni fun ann -> m (Normalized (Type TyName uni ()))
inferType config = rename >=> runTypeCheckM config . inferTypeM

-- | Check a term against a type.
-- Infers the type of the term and checks that it's equal to the given type
-- throwing a 'TypeError' (annotated with the value of the @ann@ argument) otherwise.
checkType
    :: ( AsTypeError err (Term TyName Name uni fun ()) uni fun ann, MonadError err m, MonadQuote m
       , GShow uni, GEq uni, Ix fun
       )
    => TypeCheckConfig uni fun
    -> ann
    -> Term TyName Name uni fun ann
    -> Normalized (Type TyName uni ())
    -> m ()
checkType config ann term ty = do
    termRen <- rename term
    runTypeCheckM config $ checkTypeM ann termRen ty

-- | Infer the type of a program.
inferTypeOfProgram
    :: ( AsTypeError err (Term TyName Name uni fun ()) uni fun ann, MonadError err m, MonadQuote m
       , GShow uni, GEq uni, Ix fun
       )
    => TypeCheckConfig uni fun -> Program TyName Name uni fun ann -> m (Normalized (Type TyName uni ()))
inferTypeOfProgram config (Program _ _ term) = inferType config term

-- | Check a program against a type.
-- Infers the type of the program and checks that it's equal to the given type
-- throwing a 'TypeError' (annotated with the value of the @ann@ argument) otherwise.
checkTypeOfProgram
    :: ( AsTypeError err (Term TyName Name uni fun ()) uni fun ann, MonadError err m, MonadQuote m
       , GShow uni, GEq uni, Ix fun
       )
    => TypeCheckConfig uni fun
    -> ann
    -> Program TyName Name uni fun ann
    -> Normalized (Type TyName uni ())
    -> m ()
checkTypeOfProgram config ann (Program _ _ term) = checkType config ann term
