{
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module Vein.Syntax.Parser where

import qualified Vein.Syntax.Lexer as L
import Vein.Syntax.Lexer (Span(..))

import Control.Monad.Error
import Numeric.Natural (Natural)
import Data.Fix (Fix(..))
}

%name parse
%lexer { lexwrap } { (L.TEof,Nothing) }
%monad { L.Alex }
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
instance  { (L.TKeyword L.Instance , $$) }
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

%right '$'
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
  | defInstance             { DefInstance $1 }
  | prop                    { DefConst $1 }

defs:
    def                     { [$1] }
  | def ';' defs            { $1 : $3 }

defData:
    data expr '{' props '}'       { GADT $2 $4 }
  | data expr '=' constructors    { ADT $2 $4 }

constructor:
    expr                          { Constructor $1 }

constructors:
    constructor                   { [$1] }
  | constructor '|' constructors  { $1 : $3 }

defTypeclass:
    typeclass expr where '{' props '}'  { Typeclass $2 $5 }

defInstance:
    instance expr where '{' props '}'   { Instance $2 $5 }

literal:  
    nat                     { let (L.TNat b n , l) = $1 in (LNat b n , l) }
  | fp                      { let (L.TFP b fp , l) = $1 in (LFP b fp , l) }
  | str                     { let (L.TStr s , l) = $1 in (LStr s , l) }
  | char                    { let (L.TChar c , l) = $1 in (LChar c , l) }

expr :: {LocatedExpr} :
    literal                       { mkExpr $1 $1 $ ELiteralF $ fst $1 }
  | '(' expr ')'                  { mkExpr $1 $3 $ leExprF $ unFix $2 }
  | '~' expr                      { mkExpr $1 $2 $ EUnaryOpF Inverse $2 }
  | expr expr                     { mkExpr $1 $2 $ EBinaryOpF App $1 $2 }
  | expr '{' expr '}'             { mkExpr $1 $4 $ EBinaryOpF AppImplicit $1 $3 }
  | let props in expr             { mkExpr $1 $4 $ ELetInF $2 $4 }
  | expr where '{' props '}'      { mkExpr $1 $5 $ EWhereF $1 $4 }
  | case expr of clauses          { mkExpr $1 $4 $ ECaseOfF $2 $4 }
  | match clauses                 { mkExpr $1 $2 $ EMatchF $2 }
  | list                          { mkExpr $1 $1 $ EListF $1 }
  | tuple                         { mkExpr $1 $1 $ ETupleF $1 }
  | '\\' expr '->' expr           { mkExpr $1 $4 $ ELamF $2 $4 }
  | expr '->' expr                { mkExpr $1 $3 $ EArrowF $1 $3 }
  | '{' expr '}' '->' expr        { mkExpr $1 $5 $ EArrowImplicitF $2 $5 }
  | do '{' stmts '}'              { mkExpr $1 $4 $ EDo $3 }
  | '?'                           { mkExpr $1 $1 $ EHole }
  | expr '+' expr                 { mkExpr $1 $3 $ EBinaryOpF Plus $1 $3 }
  | expr '-' expr                 { mkExpr $1 $3 $ EBinaryOpF Minus $1 $3 }
  | expr '><' expr                { mkExpr $1 $3 $ EBinaryOpF Times $1 $3 }
  | expr '/' expr                 { mkExpr $1 $3 $ EBinaryOpF Div $1 $3 }
  | expr ':' expr                 { mkExpr $1 $3 $ EBinaryOpF Typing $1 $3 }
  | expr '$' expr                 { mkExpr $1 $3 $ EBinaryOpF AppRight $1 $3 }
  | name                          { mkExpr $1 $1 $ EVar $ unLocated $1 }

name:
    qn                      { let (L.TQN qn , l) = $1 in Located l qn }

prop :: {Located (Prop LocatedExpr)} :
    expr '=' expr                           { Located (composeSpan $1 $3) $ PropEq $1 $3 }
  | expr ':' expr ';' expr '=' expr         { Located (composeSpan $1 $7) $ PropEqWithTypeAnnotation $1 $3 $5 $7 }

props :: {Located [Located (Prop LocatedExpr)]} :
    prop                    { Located (composeSpan $1 $1) [$1] }
  | prop ';' props          { Located (composeSpan $1 $3) $ $1 : unLocated $3 }

clause:
    expr '->' expr    { Located (composeSpan $1 $3) $ Clause $1 $3 }

clauses:
    clause ';'              { Located (composeSpan $1 $2) [$1] }
  | clause ';' clauses      { Located (composeSpan $1 $3) $ $1 : unLocated $3 }

list:
    '[' ']'               { Located (composeSpan $1 $2) [] }
  | '[' elems ']'         { Located (composeSpan $1 $3) $ unLocated $2 }

tuple:
    '(' ')'               { Located (composeSpan $1 $2) [] }
  | '(' elems ')'         { Located (composeSpan $1 $3) $ unLocated $2 }

elems:
    expr                  { Located (toSpan $1) [$1] }
  | expr ','              { Located (composeSpan $1 $2) [$1] }
  | expr ',' elems        { Located (composeSpan $1 $3) $ $1 : unLocated $3 }

stmt:
    expr                  { Located (toSpan $1) $ Stmt $1 }
  | expr '<-' expr        { Located (composeSpan $1 $3) $ StmtAssign $1 $3 }

stmts:
    stmt                    { Located (lSpan $1) [$1] }
  | stmt ';' stmts          { Located (composeSpan $1 $3) $ $1 : unLocated $3 }

{
data Top e = Top
  { definitions :: [Definition e]
  }
  deriving (Eq,Show,Functor)

data Definition e =
    DefData (Datatype e)
  | DefTypeclass (Typeclass e)
  | DefInstance (Instance e)
  | DefConst (Located (Prop e))
  deriving (Eq,Show,Functor)

data Prop e =
    PropEq e e
  | PropEqWithTypeAnnotation e e e e
  deriving (Eq,Show,Functor)

data Clause e = Clause e e
  deriving (Eq,Show,Functor)

data Datatype e =
    GADT e (Located [Located (Prop e)])
  | ADT e [Constructor e]
  deriving (Eq,Show,Functor)

data Typeclass e = Typeclass e (Located [Located (Prop e)])
  deriving (Eq,Show,Functor)

data Instance e = Instance e (Located [Located (Prop e)])
  deriving (Eq,Show,Functor)

newtype Constructor e = Constructor e
  deriving (Eq,Show,Functor)

data Param e =
    Param e
  | ParamImplicit e
  deriving (Eq,Show,Functor)

data ExprF r =
    EUnaryOpF UnaryOp r
  | EBinaryOpF BinaryOp r r
  | ELiteralF Literal
  | ELetInF (Located [Located (Prop r)]) r
  | EWhereF r (Located [Located (Prop r)])
  | ECaseOfF r (Located [Located (Clause r)])
  | EMatchF (Located [Located (Clause r)])
  | EListF (Located [r])
  | ETupleF (Located [r])
  | ELamF r r
  | EArrowF r r
  | EArrowImplicitF r r
  | EDo (Located [Located (Stmt r)])
  | EHole
  | EVar L.QN
  deriving (Eq,Show,Functor)

data LocatedExprF r = LocatedExprF { leExprF :: ExprF r , leSpan :: Maybe Span }
  deriving (Eq,Show)

type LocatedExpr = Fix LocatedExprF

data Located a = Located { lSpan :: Maybe Span , unLocated :: a }
  deriving (Eq,Show,Functor)

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
  | AppRight
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

class HasSpan a where
  toSpan :: a -> Maybe Span

instance HasSpan (Maybe Span) where
  toSpan = id

instance HasSpan (LocatedExprF r) where
  toSpan = leSpan

instance HasSpan LocatedExpr where
  toSpan = leSpan . unFix

instance HasSpan (a , Maybe Span) where
  toSpan = snd

instance HasSpan (Located a) where
  toSpan = lSpan

composeSpan :: HasSpan a => HasSpan b => a -> b -> Maybe Span
composeSpan x y = do
  Span begin _ <- toSpan x
  return $ Span begin $ do
    Span _ end <- toSpan y
    end

composeSpanList :: [Maybe Span] -> Maybe Span
composeSpanList ss = composeSpan (head ss) (last ss)

mkExpr :: HasSpan a => HasSpan b => a -> b -> ExprF LocatedExpr -> LocatedExpr
mkExpr first last e = Fix $ LocatedExprF e $ composeSpan (toSpan first) (toSpan last)

parseError :: L.LocatedToken -> L.Alex a
parseError t = L.alexError $ "parseError: " ++ show t

lexwrap :: (L.LocatedToken -> L.Alex a) -> L.Alex a
lexwrap = (L.alexMonadScan >>=)

}