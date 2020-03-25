{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Vein.Core.Lambda.Expr where

import qualified Vein.Core.Module as M
import qualified Vein.Syntax.AST as AST

import Numeric.Natural (Natural)
import Data.Fix (Fix(..))

type Const = M.QN

data ExprF r =
    Var Natural
  | Lam (Abs r)
  | App r r
  | Const Const
  | Univ
  | Typing r r
  | Pi (Abs r)
  | Subst (Subst r) r
  | Primitive (Primitive r)
  deriving (Eq,Show,Functor,Foldable,Traversable)

data ExprFWith a r = ExprFWith a (ExprF r)
  deriving (Eq,Show,Functor,Foldable,Traversable)

data TypedExprF a r = TypedExprF { tefType :: ExprFWith a r , tefTerm :: ExprFWith a r }
  deriving (Eq,Show,Functor,Foldable,Traversable)

type Expr a = Fix (ExprFWith a)
type TypedExpr a = Fix (TypedExprF a)

data Abs e = Abs e e
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Subst e =
    Shift Natural
  | Dot e (Subst e)
  deriving (Eq,Show,Functor,Foldable,Traversable)

nilSubst = Shift 0
dotNil e = Dot e nilSubst

data Primitive e =
    Literal AST.Literal
  | Tuple [e]
  deriving (Eq,Show,Functor,Foldable,Traversable)