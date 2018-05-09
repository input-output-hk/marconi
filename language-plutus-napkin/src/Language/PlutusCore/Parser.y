{
    {-# LANGUAGE DeriveAnyClass #-}
    {-# LANGUAGE DeriveGeneric  #-}
    module Language.PlutusCore.Parser ( parse
                                      , ParseError (..)
                                      ) where

import PlutusPrelude
import qualified Data.ByteString.Lazy as BSL
import Control.Monad.Except
import Control.Monad.Trans.Except
import Language.PlutusCore.Lexer
import Language.PlutusCore.Identifier
import Language.PlutusCore.Type
import qualified Data.List.NonEmpty as NE

}

%name parsePlutusCore
%tokentype { Token AlexPosn }
%error { parseError }
%monad { Parse } { (>>=) } { return }
%lexer { lift alexMonadScan >>= } { EOF _ }
%nonassoc integer
%nonassoc float
%nonassoc bytestring

%token

    isa { LexKeyword $$ KwIsa }
    abs { LexKeyword $$ KwAbs }
    inst { LexKeyword $$ KwInst }
    lam { LexKeyword $$ KwLam }
    fix { LexKeyword $$ KwFix }
    builtin { LexKeyword $$ KwBuiltin }
    fun { LexKeyword $$ KwFun }
    forall { LexKeyword $$ KwForall }
    size { LexKeyword $$ KwSize }
    integer { LexKeyword $$ KwInteger }
    bytestring { LexKeyword $$ KwByteString }
    type { LexKeyword $$ KwType }
    program { LexKeyword $$ KwProgram }

    openParen { LexSpecial $$ OpenParen }
    closeParen { LexSpecial $$ CloseParen }
    openBracket { LexSpecial $$ OpenBracket }
    closeBracket { LexSpecial $$ CloseBracket }
    dot { LexSpecial $$ Dot }

    builtinVar { $$@LexBuiltin{} }

    integerLit { $$@LexInt{} }

    var { $$@LexName{} }

%%

many(p)
    : many(p) p { $2 : $1 }
    | { [] }

some(p)
    : some(p) p { $2 :| toList $1 }
    | p { $1 :| [] }

parens(p)
    : openParen p closeParen { $2 }

Program : openParen program Version Term closeParen { Program $2 $3 $4 }

Version : integerLit dot integerLit dot integerLit { Version (loc $1) (int $1) (int $3) (int $5) }

Term : var { Var (loc $1) (asName $1) }
     | openParen isa Type Term closeParen { TyAnnot $2 $3 $4 }
     | openParen abs var Term closeParen { TyAbs $2 (asName $3) $4 }
     | openParen inst Term Type closeParen { TyInst $2 $3 $4 }
     | openParen lam var Term closeParen { LamAbs $2 (asName $3) $4 }
     | openBracket Term some(Term) closeBracket { Apply $1 $2 (NE.reverse $3) } -- TODO should we reverse here or somewhere else?
     | openParen fix var Term closeParen { Fix $2 (asName $3) $4 }
     | openParen builtin builtinVar closeParen { Builtin $2 (builtin $3) }
     | integerLit { PrimInt (loc $1) (int $1) }

Type : var { TyVar (loc $1) (Name (loc $1) (identifier $1)) }
     | openParen fun Type Type closeParen { TyFun $2 $3 $4 }
     | openParen forall var Kind Type closeParen { TyForall $2 (asName $3) $4 $5 }
     -- FIXME update to the spec
     | size { TySize $1 }
     | integer { TyInteger $1 }
     | bytestring { TyByteString $1 }

Kind : parens(type) { Type $1 }
     | fun Kind Kind { KindArrow $1 $2 $3 }
     | parens(size) { Size $1 }

{

asName :: Token a -> Name a
asName t = Name (loc t) (identifier t)

-- FIXME the identifier state should be included with a plain parse error as well.
parse :: BSL.ByteString -> Either ParseError (IdentifierState, (Program AlexPosn))
parse str = liftErr (go . first alex_ust <$> runAlexST str (runExceptT parsePlutusCore))
    where go (st, Left err) = Left err
          go (st, Right x) = Right (st, x)
          liftErr (Left s)  = Left (LexErr s)
          liftErr (Right x) = x

data ParseError = LexErr String
                | Unexpected (Token AlexPosn)
                | Expected AlexPosn [String] String
                | InternalError
                deriving (Show, Eq, Generic, NFData)

type Parse = ExceptT ParseError Alex

parseError :: Token AlexPosn -> Parse b
parseError = throwE . Unexpected

}
