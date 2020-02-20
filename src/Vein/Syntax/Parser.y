{
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Vein.Syntax.Parser where
import qualified Vein.Syntax.Lexer as L
import Control.Monad.Error
import Numeric.Natural (Natural)
import Data.Fix (Fix(..))
}

%name parse
%tokentype { L.Token }
%error { parseError }

%token
nat       { L.TNat $$ }
fp        { L.TFP $$ }
str       { L.TStr $$ }
char      { L.TChar $$ }
data      { L.TKeyword L.Data }
let       { L.TKeyword L.Let }
in        { L.TKeyword L.In }
case      { L.TKeyword L.Case }
of        { L.TKeyword L.Of }
match     { L.TKeyword L.Match }
where     { L.TKeyword L.Where }
import    { L.TKeyword L.Import }
module    { L.TKeyword L.Module }
typeclass { L.TKeyword L.Typeclass }
fqn       { L.TFQN $$ }
'='       { L.TSymbol L.Def }
'=='      { L.TSymbol L.Eq }
'/='      { L.TSymbol L.Neq }
'<'       { L.TSymbol L.LessThan }
'>'       { L.TSymbol L.GreaterThan }
'<='      { L.TSymbol L.LessThanEq }
'>='      { L.TSymbol L.GreaterThanEq }
'+'       { L.TSymbol L.Plus }
'-'       { L.TSymbol L.Minus }
'><'      { L.TSymbol L.Times }
'/'       { L.TSymbol L.Div }
'~'       { L.TSymbol L.Inverse }
'->'      { L.TSymbol L.Arrow }
'\\'      { L.TSymbol L.Lambda }
'*'       { L.TSymbol L.Asterisk }
'^'       { L.TSymbol L.Power }
'_'       { L.TSymbol L.Hole }
':'       { L.TSymbol L.Typing }
'.'       { L.TSymbol L.Compose }
'$'       { L.TSymbol L.AppRight }
'!'       { L.TSymbol L.LiftFunctor }
'('       { L.TParen L.Round L.LeftParen }
')'       { L.TParen L.Round L.RightParen }
'{'       { L.TParen L.Curly L.LeftParen }
'}'       { L.TParen L.Curly L.RightParen }
'['       { L.TParen L.Square L.LeftParen }
']'       { L.TParen L.Square L.RightParen }
','       { L.TSeparator L.Comma }
';'       { L.TSeparator L.Semicolon }
'|'       { L.TSeparator L.VerticalBar }


%%
literal:  
    nat                     { let (b,n) = $1 in LNat b n }
  | fp                      { let (b,fp) = $1 in LFP b fp }
  | str                     { LStr $1 }
  | char                    { LChar $1 }

expr:
    literal                 { Fix $ ELiteralF $1 }
  | '(' expr ')'            { $2 }
  | '~' expr                { Fix $ EUnaryOpF Inverse $2 }
  | expr '+' expr           { Fix $ EBinaryOpF Plus $1 $3 }
  | expr '-' expr           { Fix $ EBinaryOpF Minus $1 $3 }
  | expr '><' expr          { Fix $ EBinaryOpF Times $1 $3 }
  | expr '/' expr           { Fix $ EBinaryOpF Div $1 $3 }


{
data ExprF r =
    EUnaryOpF UnaryOp r
  | EBinaryOpF BinaryOp r r
  | ELiteralF Literal
  deriving (Eq,Show,Functor)

type Expr = Fix ExprF

data UnaryOp =
    Inverse
  deriving (Eq,Show)

data BinaryOp =
    Arrow
  | Plus
  | Minus
  | Times
  | Div
  deriving (Eq,Show)

data Literal =
    LNat L.Base Natural
  | LFP L.Base L.FloatingPoint
  | LStr String
  | LChar Char
  deriving (Eq,Show)

parseError :: [L.Token] -> a
parseError [] = error "Parse error at EOF"
parseError (t:ts) = error $ "Parse error: " ++ show t
}