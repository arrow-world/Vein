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
%tokentype { L.LocatedToken }
%error { parseError }

%token
nat       { (L.TNat _ _ , _) }
fp        { (L.TFP _ _ , _) }
str       { (L.TStr _ , _) }
char      { (L.TChar _ , _) }
qn        { (L.TQN _ , _) }
data      { (L.TKeyword L.Data , $$) }
let       { (L.TKeyword L.Let , $$) }
in        { (L.TKeyword L.In , $$) }
case      { (L.TKeyword L.Case , $$) }
of        { (L.TKeyword L.Of , $$) }
match     { (L.TKeyword L.Match , $$) }
where     { (L.TKeyword L.Where , $$) }
import    { (L.TKeyword L.Import , $$) }
module    { (L.TKeyword L.Module , $$) }
typeclass { (L.TKeyword L.Typeclass , $$) }
do        { (L.TKeyword L.Do , $$) }
'='       { (L.TSymbol L.Def , $$) }
'=='      { (L.TSymbol L.Eq , $$) }
'/='      { (L.TSymbol L.Neq , $$) }
'<'       { (L.TSymbol L.LessThan , $$) }
'>'       { (L.TSymbol L.GreaterThan , $$) }
'<='      { (L.TSymbol L.LessThanEq , $$) }
'>='      { (L.TSymbol L.GreaterThanEq , $$) }
'+'       { (L.TSymbol L.Plus , $$) }
'-'       { (L.TSymbol L.Minus , $$) }
'><'      { (L.TSymbol L.Times , $$) }
'/'       { (L.TSymbol L.Div , $$) }
'~'       { (L.TSymbol L.Inverse , $$) }
'->'      { (L.TSymbol L.Arrow , $$) }
'\\'      { (L.TSymbol L.Lambda , $$) }
'*'       { (L.TSymbol L.Asterisk , $$) }
'^'       { (L.TSymbol L.Power , $$) }
'?'       { (L.TSymbol L.Hole , $$) }
'_'       { (L.TSymbol L.Placeholder , $$) }
':'       { (L.TSymbol L.Typing , $$) }
'.'       { (L.TSymbol L.Compose , $$) }
'$'       { (L.TSymbol L.AppRight , $$) }
'!'       { (L.TSymbol L.LiftFunctor , $$) }
'<-'      { (L.TSymbol L.Assign , $$) }
'('       { (L.TParen L.Round L.LeftParen , $$) }
')'       { (L.TParen L.Round L.RightParen , $$) }
'{'       { (L.TParen L.Curly L.LeftParen , $$) }
'}'       { (L.TParen L.Curly L.RightParen , $$) }
'['       { (L.TParen L.Square L.LeftParen , $$) }
']'       { (L.TParen L.Square L.RightParen , $$) }
','       { (L.TSeparator L.Comma , $$) }
';'       { (L.TSeparator L.Semicolon , $$) }
'|'       { (L.TSeparator L.VerticalBar , $$) }

%nonassoc '<' '>'
%left ':'
%left '+' '-'
%left '><' '/'

%%

top:
    defs                    { Top $1 }

def:
    defData                 { DefData $1 }
  | defTypeclass            { DefTypeclass $1 }
  | prop                    { DefConst $1 }

defs:
    def                     { [$1] }
  | def ';' defs            { $1 : $3 }

defData:
    data expr where props         { GADT $2 $4 }
  | data expr '=' constructors    { ADT $2 $4 }

constructor:
    name params             { Constructor $1 $2 }

param:
    expr                    { Param $1 }
  | '{' expr '}'            { ParamImplicit $2 }

params:
    param                   { [$1] }
  | param ';' params        { $1 : $3 }

constructors:
    constructor                   { [$1] }
  | constructor '|' constructors  { $1 : $3 }

defTypeclass:
    typeclass expr where props    { Typeclass $2 $4 }

literal:  
    nat                     { let (L.TNat b n , _) = $1 in LNat b n }
  | fp                      { let (L.TFP b fp , _) = $1 in LFP b fp }
  | str                     { let (L.TStr s , _) = $1 in LStr s }
  | char                    { let (L.TChar c , _) = $1 in LChar c }

expr:
    literal                 { Fix $ ELiteralF $1 }
  | '(' expr ')'            { $2 }
  | '~' expr                { Fix $ EUnaryOpF Inverse $2 }
  | expr '+' expr           { Fix $ EBinaryOpF Plus $1 $3 }
  | expr '-' expr           { Fix $ EBinaryOpF Minus $1 $3 }
  | expr '><' expr          { Fix $ EBinaryOpF Times $1 $3 }
  | expr '/' expr           { Fix $ EBinaryOpF Div $1 $3 }
  | expr ':' expr           { Fix $ EBinaryOpF Typing $1 $3 }
  | expr expr               { Fix $ EBinaryOpF App $1 $2 }
  | expr '{' expr '}'       { Fix $ EBinaryOpF AppImplicit $1 $3 }
  | let props in expr       { Fix $ ELetInF $2 $4 }
  | expr where props        { Fix $ EWhereF $1 $3 }
  | case name of clauses    { Fix $ ECaseOfF $2 $4 }
  | match clauses           { Fix $ EMatchF $2 }
  | list                    { Fix $ EListF $1 }
  | tuple                   { Fix $ ETupleF $1 }
  | '\\' expr '->' expr     { Fix $ ELamF $2 $4 }
  | expr '->' expr          { Fix $ EArrowF $1 $3 }
  | '{' expr '}' '->' expr  { Fix $ EArrowImplicitF $2 $5 }
  | do stmts                { Fix $ EDo $2 }
  | '?'                     { Fix $ EHole }

name:
    qn                      { let (L.TQN qn , _) = $1 in qn }

prop:
    expr '=' expr                     { PropEq $1 $3 }
  | expr ':' expr ';' expr '=' expr   { PropEqWithTypeAnnotation $1 $3 $5 $7 }

props:
    prop                    { [$1] }
  | props ';'               { $1 }
  | prop ';' props          { $1 : $3 }

clause:
    expr '->' expr          { Clause $1 $3 }

clauses:
    clause                  { [$1] }
  | clause ';' clauses      { $1 : $3 }

list:
    '[' elems ']'           { $2 }

tuple:
    elems                   { $1 }

elems:
    expr                    { [$1] }
  | expr ',' elems          { $1 : $3 }

stmt:
    expr                    { Stmt $1 }
  | expr '<-' expr          { StmtAssign $1 $3 }

stmts:
    stmt                    { [$1] }
  | stmt ';' stmts          { $1 : $3 }

{
data Top e = Top
  { definitions :: [Definition e]
  }
  deriving (Eq,Show,Functor)

data Definition e =
    DefData (Datatype e)
  | DefTypeclass (Typeclass e)
  | DefConst (Prop e)
  deriving (Eq,Show,Functor)

data Prop e =
    PropEq e e
  | PropEqWithTypeAnnotation e e e e
  deriving (Eq,Show,Functor)

data Clause e = Clause e e
  deriving (Eq,Show,Functor)

data Datatype e =
    GADT e [Prop e]
  | ADT e [Constructor e]
  deriving (Eq,Show,Functor)

data Typeclass e = Typeclass e [Prop e]
  deriving (Eq,Show,Functor)

data Constructor e = Constructor L.QN [Param e]
  deriving (Eq,Show,Functor)

data Param e =
    Param e
  | ParamImplicit e
  deriving (Eq,Show,Functor)

data ExprF r =
    EUnaryOpF UnaryOp r
  | EBinaryOpF BinaryOp r r
  | ELiteralF Literal
  | ELetInF [Prop r] r
  | EWhereF r [Prop r]
  | ECaseOfF L.QN [Clause r]
  | EMatchF [Clause r]
  | EListF [r]
  | ETupleF [r]
  | ELamF r r
  | EArrowF r r
  | EArrowImplicitF r r
  | EDo [Stmt r]
  | EHole
  deriving (Eq,Show,Functor)

data LocatedExprF r = LocatedExprF { exprF :: ExprF r , span :: L.Span }
  deriving (Eq,Show)

type LocatedExpr = Fix LocatedExprF

data UnaryOp =
    Inverse
  deriving (Eq,Show)

data BinaryOp =
    Arrow
  | App
  | AppImplicit
  | Plus
  | Minus
  | Times
  | Div
  | Typing
  deriving (Eq,Show)

data Literal =
    LNat L.Base Natural
  | LFP L.Base L.FloatingPoint
  | LStr String
  | LChar Char
  deriving (Eq,Show)

data Stmt e =
    Stmt e
  | StmtAssign e e
  deriving (Eq,Show,Functor)

parseError :: [L.LocatedToken] -> a
parseError [] = error "Parse error at EOF"
parseError (t:ts) = error $ "Parse error: " ++ show t
}