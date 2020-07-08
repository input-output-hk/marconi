{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.PlutusCore.PropTest
  ( TyProp
  , testTyProp
  , toClosedType
  , normalizeTypeG
  ) where

import           Language.PlutusCore
import           Language.PlutusCore.Gen.Common
import qualified Language.PlutusCore.Gen.Type as Gen
import           Language.PlutusCore.Gen.Type hiding (toClosedType)
import           Language.PlutusCore.Pretty

import           Control.Search
import           Control.Monad.Except

--import           Data.Coolean (Cool)
import qualified Data.Coolean as Cool
import qualified Data.Text as Text
import           Test.Tasty.HUnit
import           Text.Printf


{-

This file contains the testing machinery for property based testing of
generated types

-}

-- |The type for properties with access to both representations.
type TyProp =  Kind ()
            -> ClosedTypeG
            -> Kind ()
            -> Quote (Type TyName DefaultUni ())
            -> Cool

-- |Internal version of type properties.
type TyPropG = Kind () -> ClosedTypeG -> Cool


-- |Test property on well-kinded types.
testTyProp :: Int     -- ^ Search depth
           -> Kind () -- ^ Kind for generated types
           -> TyProp  -- ^ Property to test
           -> IO ()
testTyProp depth k typrop = do
  -- NOTE: Any strategy which attempts fairness will crash the search!
  --       These strategies evaluate !=> in *parallel*, and hence attempt
  --       to convert ill-kinded types, but `toType` is partial!
  -- TODO: This *may* be a bug in the lazy-search library.
  result <- ctrex' O depth (toTyPropG typrop k)
  case result of
    Left  _   -> return ()
    Right tyG -> assertFailure (errorMsgTyProp k tyG)


-- |Generate the error message for a failed `TyProp`.
errorMsgTyProp :: Kind () -> ClosedTypeG -> String
errorMsgTyProp kG tyG =
  case runTCM (inferKind defConfig =<< liftQuote tyQ) of
    Left err ->
      printf "Counterexample found: %s, generated for kind %s\n%s"
        (show (pretty ty)) (show (pretty kG)) (show (prettyPlcClassicDef err))
    Right k2 ->
      printf "Counterexample found: %s, generated for kind %s, has inferred kind %s"
        (show (pretty ty)) (show (pretty kG)) (show (pretty k2))
  where
    runTCM :: ExceptT (TypeError DefaultUni ()) Quote a
           -> Either  (TypeError DefaultUni ()) a
    runTCM = runQuote . runExceptT

    tyQ = toClosedType kG tyG :: Quote (Type TyName DefaultUni ())
    ty  = runQuote tyQ


-- |Convert a `TyProp` to the internal representation of types.
toTyPropG :: TyProp -> TyPropG
toTyPropG typrop kG tyG = tyG_ok Cool.!=> typrop_ok
  where
    tyG_ok    = checkClosedTypeG kG tyG
    typrop_ok = typrop kG tyG kG (toClosedType kG tyG)

-- not used
-- |Stream of names x0, x1, x2, ..
--names :: [Text.Text]
--names = mkTextNameStream "x"


-- |Stream of type names t0, t1, t2, ..
tynames :: [Text.Text]
tynames = mkTextNameStream "t"


-- |Convert type.
toClosedType :: MonadQuote m
             => Kind ()
             -> ClosedTypeG
             -> m (Type TyName DefaultUni ())
toClosedType = Gen.toClosedType tynames
