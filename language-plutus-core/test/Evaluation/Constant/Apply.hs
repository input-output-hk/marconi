-- | This module contains definitions which allow to test the 'applyBuiltinName' function.
-- See the "Success" and "SuccessFailure" module for actual tests implemented
-- in terms of functions defined here.

{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module Evaluation.Constant.Apply
    ( prop_applyBuiltinNameSuccess
    , prop_applyBuiltinNameSuccessFailure
    ) where

import           Evaluation.Constant.GenTypedBuiltinSized
import           Evaluation.Generator
import           Language.PlutusCore

import           Data.Foldable
import           Data.List
import           Data.Text.Prettyprint.Doc
import           Hedgehog                                 hiding (Size, Var)

forAllPretty :: (Monad m, Pretty a) => Gen a -> PropertyT m a
forAllPretty = forAllWith prettyString

-- | This a generic property-based testing procedure for 'applyBuiltinName'.
-- It generates Haskell values of builtin types (see 'TypedBuiltin' for the list of such types)
-- taking size-induced bounds (controlled by the 'GenTypedBuiltinSized' parameter) into account
-- for arguments and either taking those bounds into account for the final result or using the
-- default ones (as per the spec) or ignoring them completely depending on how you instantiate
-- the first parameter. An argument is generated as a Haskell value, then coerced to the
-- corresponding PLC value which gets appended to the spine of arguments 'applyBuiltinName' expects.
-- The generated Haskell value is passed to the semantics of the 'TypedBuiltinName'
-- (the second argument), i.e. to the third argument. Thus we collect arguments on the PLC side
-- and supply them to a function on the Haskell side. When all appropriate arguments are generated,
-- we check that the results of the two computations match. We also check that each
-- underapplication on the PLC side is a stuck application.
prop_applyBuiltinName
    :: (forall b. TypedBuiltin Size b -> b -> Gen ConstAppResult)
                             -- ^ How to get a 'ConstAppResult' having a Haskell value of
                             -- one of the builtin types. See 'TypedBuiltin' for the list of such types.
    -> TypedBuiltinName a    -- ^ A (typed) builtin name to apply.
    -> a                     -- ^ The semantics of the builtin name. E.g. the semantics of
                             -- 'AddInteger' (and hence 'typedAddInteger') is '(+)'.
    -> GenTypedBuiltinSized  -- ^ How to generate values of sized builtin types.
    -> Property
prop_applyBuiltinName getFinal tbn op allTbs = property $ do
    PrimIterAppValue _ iterApp tbv <- forAllPretty . runPlcT allTbs $ genPrimIterAppValue tbn op
    let IterApp name spine = iterApp
        TypedBuiltinValue tb y = tbv
        app = applyBuiltinName name
    final <- forAll $ getFinal tb y
    traverse_ (\prefix -> app prefix === ConstAppStuck) . init $ inits spine
    app spine === final

-- | A specialized version of 'prop_applyBuiltinName'. A final value of the computation on
-- the Haskell side must fit into the specified (by the 'GenTypedBuiltinSized' argument) bounds
-- and hence the result of the 'applyBuiltinName' computation must be a 'ConstAppSuccess'.
-- See 'genTypedBuiltinSizedSum' for how this is achieved for 'AddInteger' and 'SubtractInteger'.
-- See the "Success" module for tests defined in terms of this function.
prop_applyBuiltinNameSuccess :: TypedBuiltinName a -> a -> GenTypedBuiltinSized -> Property
prop_applyBuiltinNameSuccess =
    prop_applyBuiltinName $ \tbs -> fmap ConstAppSuccess . typedBuiltinAsValue tbs

-- | A specialized version of 'prop_applyBuiltinName'. A final value of the computation on
-- the Haskell side may or may not fit into the default bounds (as per the spec) and hence the
-- result of the 'applyBuiltinName' computation must be either a 'ConstAppSuccess' or 'ConstAppFailure'.
-- See the "SuccessFailure" module for tests defined in terms of this function.
prop_applyBuiltinNameSuccessFailure :: TypedBuiltinName a -> a -> Property
prop_applyBuiltinNameSuccessFailure tbn x =
    prop_applyBuiltinName (\tbs -> return . makeConstantApp tbs) tbn x genTypedBuiltinSizedDef
