{
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyCase #-}

module Vein.Syntax.Parser where

import qualified Vein.Syntax.Lexer as L
import Vein.Syntax.Lexer (Span(..))
import Vein.Syntax.AST
import Vein.Core.Module as Module

import Control.Monad.Error
import Numeric.Natural (Natural)
import Data.Fix (Fix(..))
import Data.Tuple (swap)
import Data.Functor.Identity (Identity(..))
import qualified Data.Text as T
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
'`'       { (L.TSymbol L.Infixator , $$) }
'>>'      { (L.TSymbol L.ComposeRight , $$) }
'=>'      { (L.TSymbol L.Constraint , $$) }
'('       { (L.TParen L.Round L.LeftParen , $$) }
')'       { (L.TParen L.Round L.RightParen , $$) }
'{'       { (L.TParen L.Curly L.LeftParen , $$) }
'}'       { (L.TParen L.Curly L.RightParen , $$) }
'['       { (L.TParen L.Square L.LeftParen , $$) }
']'       { (L.TParen L.Square L.RightParen , $$) }
','       { (L.TSeparator L.Comma , $$) }
';'       { (L.TSeparator L.Semicolon , $$) }
'|'       { (L.TSeparator L.VerticalBar , $$) }

%left ':'

%right '$'
%left '>>'
%right '.'

%nonassoc '<' '>' '<=' '>=' '==' '<-'
%left '+' '-'
%left '><' '/'

%%

top:
                                        { ParsedEnv $ Right emptyEnv }
  | prop_stmts                          { parsedStatementsToEnv $1 }

prop_stmts:
    ';'                                 { [] }
  | prop_needs_separator                { [$1] }
  | prop_unneeds_separator              { [$1] }
  | prop_needs_separator ';'            { [$1] }
  | ';' prop_stmts                      { $2 }
  | prop_unneeds_separator prop_stmts   { $1 : $2 }
  | prop_needs_separator ';' prop_stmts { $1 : $3 }
  
prop_unneeds_separator:
    typeclass name params_props         { Right $ Def $ Module.QNamed (unLocated $2) $ DefTypeclass $ uncurry Typeclass $3 }
  | instance name params_props          { Right $ Ann $ DeclInstance $ uncurry (Instance $ unLocated $2) $3 }
  | data name params_props              { Right $ Def $ Module.QNamed (unLocated $2) $ DefData $ uncurry GADT $3 }

prop_needs_separator:
    data name '=' constructors          { Right $ Def $ Module.QNamed (unLocated $2) $ DefData $ ADT (Located Nothing []) $4 }
  | data name params '=' constructors   { Right $ Def $ Module.QNamed (unLocated $2) $ DefData $ ADT $3 $5 }
  | prop                                { unLocated $1 >>= return . \case
                                            PropEq name params e -> Def $ Module.QNamed (unLocated name) $ DefConst [(params,e)]
                                            PropTypeAnnotation l r -> Ann $ TypeAnnotation l r
                                        }

constructor:
    name                          { Constructor $1 $ Located Nothing [] }
  | name params                   { Constructor $1 $2 }

constructors:
    constructor                   { [$1] }
  | constructor '|' constructors  { $1 : $3 }


params_props :: {(Located [Located (Param LocatedExpr)] , Located (ParsedEnv LocatedExpr))} :
    '{' props '}'                 { ( Located (composeSpan $1 $3) [] , fmap parsedPropsToEnv $2 ) }
  | param params_props            { swap $ fmap (\params -> Located (composeSpan $1 params) $ $1 : unLocated params) $ swap $2 }


prop :: {Located (Either (ParseError LocatedExpr) (Prop LocatedExpr))} :
    pat '=' expr_with_where                 { Located (composeSpan $1 $3) $ case leExprF $ unFix $1 of
                                                EVar name -> Right $ PropEq (Located (toSpan $1) name) (Located Nothing []) $3

                                                EApp var params -> do
                                                  var' <- maybe (Left $ NotAllowedExpr $1) Right $ unwrapQN var
                                                  return $ PropEq var' params $3
                                                
                                                EUnaryOpF op e -> do
                                                  let op' = fmap uopToName op
                                                  return $ PropEq op' (Located (toSpan e) [Located Nothing (Param e)]) $3

                                                EBinaryOpF op l r -> do
                                                  op' <- maybe (Left $ NotAllowedExpr $1) Right $ traverse bopToName op
                                                  return $  PropEq op'
                                                              (Located (composeSpan l r) $ map (Located Nothing . Param) [l,r]) $3
                                                
                                                _ -> Left $ NotAllowedExpr $1
                                            }
  | expr ':' expr_with_where                { Located (composeSpan $1 $3) $ Right $ PropTypeAnnotation $1 $3 }

props :: {Located [Located (Either (ParseError LocatedExpr) (Prop LocatedExpr))]} :
    prop                    { Located (composeSpan $1 $1) [$1] }
  | prop ';'                { Located (composeSpan $1 $2) [$1] }
  | prop ';' props          { Located (composeSpan $1 $3) $ $1 : unLocated $3 }

expr_with_typing:
    expr                          { $1 }
  | expr ':' expr                 { mkExpr $1 $3 $ EBinaryOpF (Located $2 Typing) $1 $3 }

expr_with_where:
    expr                          { $1 }
  | expr where '{' props '}'      { mkExpr $1 $5 $ EWhereF $1 (fmap parsedPropsToEnv $4) }

expr:
    expr4                         { $1 }


expr4:
    expr3                         { $1 }
  | '\\' pat '->' expr            { mkExpr $1 $4 $ ELamF $2 $4 }
  | let props in expr             { mkExpr $1 $4 $ ELetInF (fmap parsedPropsToEnv $2) $4 }
  | case expr of clauses          { mkExpr $1 $4 $ ECaseOfF $2 $4 }
  | match clauses                 { mkExpr $1 $2 $ EMatchF $2 }
  | do '{' stmts '}'              { mkExpr $1 $4 $ EDo $3 }

clause:
    pat '->' expr_with_where      { Located (composeSpan $1 $3) $ Clause $1 $3 }

clauses:
    clause ';'              { Located (composeSpan $1 $2) [$1] }
  | clause ';' clauses      { Located (composeSpan $1 $3) $ $1 : unLocated $3 }

expr3:
    expr23                                      { $1 }
  | expr23 '->' expr3                           { mkExpr $1 $3 $ mkArrow (Located (toSpan $1) $ Param $1) $3 }
  | '{' expr_with_typing '}' '->' expr3         { mkExpr $1 $5 $ mkArrow (Located (composeSpan $1 $3) $ ParamImplicit $2) $5 }


pat:
    expr23                          { $1 }


expr23:
    expr22                          { $1 }
  | expr23 '$' expr23               { mkExpr $1 $3 $ EBinaryOpF (Located $2 AppRight) $1 $3 }
  | expr23 '.' expr23               { mkExpr $1 $3 $ EBinaryOpF (Located $2 Compose) $1 $3 }
  | expr23 '>>' expr23              { mkExpr $1 $3 $ EBinaryOpF (Located $2 ComposeRight) $1 $3 }


expr22:
    expr21                          { $1 }
  | expr22 infixator expr21         { mkExpr $1 $3 $ EBinaryOpF $2 $1 $3 }

infixator:
    '`' expr21 '`'                  { Located ($1 `composeSpan` $3) $ Infixated $ mkExpr $1 $3 $ leExprF $ unFix $2 }


expr21:
    expr2                           { $1 }
  | expr21 '+' expr21               { mkExpr $1 $3 $ EBinaryOpF (Located $2 Plus) $1 $3 }
  | expr21 '-' expr21               { mkExpr $1 $3 $ EBinaryOpF (Located $2 Minus) $1 $3 }
  | expr21 '><' expr21              { mkExpr $1 $3 $ EBinaryOpF (Located $2 Times) $1 $3 }
  | expr21 '/' expr21               { mkExpr $1 $3 $ EBinaryOpF (Located $2 Div) $1 $3 }

expr2:
    expr1                           { $1 }
  | expr1 params                    { mkExpr $1 $2 $ EApp $1 $2 }

param:
    expr1                           { Located (toSpan $1) $ Param $1 }
  | '{' expr '}'                    { Located (composeSpan $1 $3) $ ParamImplicit $2 }

params:
    param                           { Located (toSpan $1) [$1] }
  | param params                    { Located (composeSpan $1 $2) $ $1 : unLocated $2 }


expr1 :: {LocatedExpr} :
    name                          { mkExpr $1 $1 $ EVar $ unLocated $1 }
  | literal                       { mkExpr $1 $1 $ ELiteralF $ fst $1 }
  | '~' expr1                     { mkExpr $1 $2 $ EUnaryOpF (Located $1 Inverse) $2 }
  | '(' expr_with_typing ')'      { mkExpr $1 $3 $ leExprF $ unFix $2 }
  | list                          { mkExpr $1 $1 $ EListF $1 }
  | tuple                         { mkExpr $1 $1 $ ETupleF $1 }
  | '?'                           { mkExpr $1 $1 $ EHole }
  | '_'                           { mkExpr $1 $1 $ EPlaceholder }

literal:  
    nat                     { let (L.TNat b n , l) = $1 in (LNat b n , l) }
  | fp                      { let (L.TFP b fp , l) = $1 in (LFP b fp , l) }
  | str                     { let (L.TStr s , l) = $1 in (LStr s , l) }
  | char                    { let (L.TChar c , l) = $1 in (LChar c , l) }

name:
    qn                      { let (L.TQN qn , l) = $1 in Located l qn }


list:
    '[' ']'               { Located (composeSpan $1 $2) [] }
  | '[' elems ']'         { Located (composeSpan $1 $3) $ unLocated $2 }

tuple:
    '(' ')'                       { Located (composeSpan $1 $2) [] }
  | '(' expr ',' ')'              { Located (composeSpan $1 $4) [$2] }
  | '(' expr ',' elems ')'        { Located (composeSpan $1 $5) $ $2 : unLocated $4 }

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

mkArrow :: Located (Param LocatedExpr) -> LocatedExpr -> ExprF LocatedExpr
mkArrow a (Fix (LocatedExprF (EArrowF as b) _)) = EArrowF (a:as) b
mkArrow a b = EArrowF [a] b

parseError :: L.LocatedToken -> L.Alex a
parseError t = L.alexError $ "parseError: " ++ show t

lexwrap :: (L.LocatedToken -> L.Alex a) -> L.Alex a
lexwrap = (L.alexMonadScan >>=)

}