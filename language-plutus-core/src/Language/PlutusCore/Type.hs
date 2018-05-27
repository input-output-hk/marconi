{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Language.PlutusCore.Type ( Term (..)
                                , Type (..)
                                , Kind (..)
                                , Program (..)
                                , Constant (..)
                                -- * Base functors
                                , TermF (..)
                                , TypeF (..)
                                ) where

import qualified Data.ByteString.Lazy               as BSL
import           Data.Functor.Foldable              (cata)
import           Data.Functor.Foldable.TH
import qualified Data.Text                          as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Internal (Doc (Text))
import           Debug.Trace
import           Language.PlutusCore.Lexer.Type
import           Language.PlutusCore.Name
import           Numeric                            (showHex)
import           PlutusPrelude

-- | A 'Type' assigned to expressions.
data Type a = TyVar a (Name a)
            | TyFun a (Type a) (Type a)
            | TyFix a (Name a) (Kind a) (Type a) -- ^ Fix-point type, for constructing self-recursive types
            | TyForall a (Name a) (Kind a) (Type a)
            | TyBuiltin a TypeBuiltin -- ^ Builtin type
            | TyLam a (Name a) (Kind a) (Type a)
            | TyApp a (Type a) (NonEmpty (Type a))
            deriving (Functor, Show, Eq, Generic, NFData)

-- | A constant value.
data Constant a = BuiltinInt a Natural Integer
                | BuiltinBS a Natural BSL.ByteString
                | BuiltinSize a Natural
                | BuiltinName a BuiltinName
                deriving (Functor, Show, Eq, Generic, NFData)

-- | A 'Term' is a value.
data Term a = Var a (Name a) -- ^ A named variable
            | TyAnnot a (Type a) (Term a) -- ^ A 'Term' annotated with a 'Type'
            | TyAbs a (Name a) (Term a)
            | LamAbs a (Name a) (Term a)
            | Apply a (Term a) (NonEmpty (Term a))
            | Fix a (Name a) (Term a)
            | Constant a (Constant a) -- ^ A constant term
            | TyInst a (Term a) (NonEmpty (Type a))
            deriving (Functor, Show, Eq, Generic, NFData)

-- TODO: implement renamer, i.e. annotate each variable with its type
-- Step 1: typeOf for builtins?
-- Step 2: use this for surrounding & inner data

-- | Kinds. Each type has an associated kind.
data Kind a = Type a
            | KindArrow a (Kind a) (Kind a)
            | Size a
            deriving (Functor, Eq, Show, Generic, NFData)

-- | A 'Program' is simply a 'Term' coupled with a 'Version' of the core
-- language.
data Program a = Program a (Version a) (Term a)
               deriving (Show, Eq, Functor, Generic, NFData)

makeBaseFunctor ''Kind
makeBaseFunctor ''Term
makeBaseFunctor ''Type

instance Pretty (Kind a) where
    pretty = cata a where
        a TypeF{}             = "(type)"
        a SizeF{}             = "(size)"
        a (KindArrowF _ k k') = parens ("fun" <+> k <+> k')

instance Pretty (Program a) where
    pretty (Program _ v t) = parens ("program" <+> pretty v <+> pretty t)

asBytes :: Word8 -> Doc a
asBytes = Text 2 . T.pack . ($ mempty) . showHex

instance Pretty (Constant a) where
    pretty (BuiltinInt _ s i) = pretty s <+> "!" <+> pretty i
    pretty (BuiltinSize _ s)  = pretty s
    pretty (BuiltinBS _ s b)  = pretty s <+> "!" <+> "#" <> fold (asBytes <$> BSL.unpack b)
    pretty (BuiltinName _ n)  = pretty n

-- TODO better identation
instance Pretty (Term a) where
    pretty = cata a where
        a (ConstantF _ b)   = parens ("con" <+> pretty b)
        a (ApplyF _ t ts)   = "[" <+> t <+> hsep (toList ts) <+> "]"
        a (TyAnnotF _ t te) = parens ("isa" <+> pretty t <+> te)
        a (VarF _ n)        = pretty n
        a (TyAbsF _ n t)    = parens ("abs" <+> pretty n <+> t)
        a (TyInstF _ t te)  = "{" <+> t <+> hsep (pretty <$> toList te) <+> "}"
        a (FixF _ n t)      = parens ("fix" <+> pretty n <+> t)
        a (LamAbsF _ n t)   = parens ("lam" <+> pretty n <+> t)

instance Pretty (Type a) where
    pretty = cata a where
        a (TyAppF _ t ts)     = "[" <+> t <+> hsep (toList ts) <+> "]"
        a (TyVarF _ n)        = pretty n
        a (TyFunF _ t t')     = parens ("fun" <+> t <+> t')
        a (TyFixF _ n k t)    = parens ("fix" <+> pretty n <+> pretty k <+> t)
        a (TyForallF _ n k t) = parens ("forall" <+> pretty n <+> pretty k <+> t)
        a (TyBuiltinF _ n)    = parens ("con" <+> pretty n)
        a (TyLamF _ n k t)    = parens ("lam" <+> pretty n <+> pretty k <+> t)
