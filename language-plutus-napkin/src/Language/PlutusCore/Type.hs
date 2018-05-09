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
                                , Token (..)
                                , Builtin (..)
                                , Kind (..)
                                , Keyword (..)
                                , Special (..)
                                , Name (..)
                                , Version (..)
                                , Program (..)
                                -- * Base functors
                                , TermF (..)
                                , TypeF (..)
                                ) where

import qualified Data.ByteString.Lazy           as BSL
import           Data.Functor.Foldable          (cata)
import           Data.Functor.Foldable.TH
import           Data.Text.Encoding             (decodeUtf8)
import           Data.Text.Prettyprint.Doc
import           Language.PlutusCore.Identifier
import           PlutusPrelude

data Builtin = AddInteger
             | SubtractInteger
             | MultiplyInteger
             | DivideInteger
             | RemainderInteger
             | LessThanInteger
             | LessThanEqInteger
             | GreaterThanInteger
             | GreaterThanEqInteger
             | EqInteger
             | IntToByteString
             | Ceiling
             | Floor
             | Round
             | Concatenate
             | TakeByteString
             | DropByteString
             | SHA2
             | SHA3
             | VerifySignature
             | EqByteString
             | TxHash
             | BlockNum
             | BlockTime
             deriving (Show, Eq, Generic, NFData)

data Version a = Version a Integer Integer Integer
               deriving (Generic, NFData)

data Keyword = KwIsa
             | KwAbs
             | KwInst
             | KwLam
             | KwFix
             | KwBuiltin
             | KwFun
             | KwForall
             | KwByteString
             | KwInteger
             | KwSize
             | KwType
             | KwProgram
             deriving (Show, Eq, Generic, NFData)

data Special = OpenParen
             | CloseParen
             | OpenBracket
             | CloseBracket
             | Dot
             deriving (Show, Eq, Generic, NFData)

-- | Annotated type for names
data Token a = LexName { loc :: a, identifier :: Unique }
             | LexInt { loc :: a, int :: Integer }
             | LexBS { loc :: a, bytestring :: BSL.ByteString }
             | LexBuiltin { loc :: a, builtin :: Builtin }
             | LexSize { loc :: a, size :: Natural }
             | LexSizeTerm { loc :: a, sizeTerm :: Natural }
             | LexKeyword { loc :: a, keyword :: Keyword }
             | LexSpecial { loc :: a, special :: Special }
             | EOF { loc :: a }
             deriving (Show, Eq, Generic, NFData)

data Name a = Name a Unique
            deriving (Show, Generic, NFData)

data Type a = TyVar a (Name a)
            | TyFun a (Type a) (Type a)
            | TyFix a (Name a) (Kind a) (Type a)
            | TyForall a (Name a) (Kind a) (Type a)
            | TyByteString a
            | TyInteger a
            | TySize a
            | TyLam a (Name a) (Kind a) (Type a)
            | TyApp a (Type a) (NonEmpty (Type a))
            deriving (Show, Generic, NFData)

data Term a = Var a (Name a)
            | TyAnnot a (Type a) (Term a)
            | TyAbs a (Name a) (Term a)
            | TyInst a (Term a) (Type a)
            | LamAbs a (Name a) (Term a)
            | Apply a (Term a) (NonEmpty (Term a))
            | Fix a (Name a) (Term a)
            | Builtin a Builtin
            | PrimInt a Integer
            | PrimBS a BSL.ByteString
            | PrimSize a (Name a)
            | PrintVar a BSL.ByteString
            deriving (Show, Generic, NFData)

data Kind a = Type a
            | KindArrow a (Kind a) (Kind a)
            | Size a
            deriving (Show, Generic, NFData)

data Program a = Program a (Version a) (Term a)
               deriving (Generic, NFData)

makeBaseFunctor ''Term
makeBaseFunctor ''Type

-- TODO figure out indentation etc.
instance Pretty Builtin where
    pretty AddInteger = "addInteger"
    pretty _          = undefined

instance Pretty (Version a) where
    pretty (Version _ i j k) = pretty i <> "." <> pretty j <> "." <> pretty k

instance Pretty (Program a) where
    pretty (Program _ v t) = parens ("program" <+> pretty v <+> pretty t)

instance Pretty (Term a) where
    pretty = cata a where
        a (PrintVarF _ s) = pretty (decodeUtf8 $ BSL.toStrict s)
        a (BuiltinF _ b)  = parens ("builtin" <+> pretty b)
        a (ApplyF _ t ts) = "[" <+> t <+> hsep (toList ts) <+> "]"
        a VarF{}          = undefined
        a _               = undefined
