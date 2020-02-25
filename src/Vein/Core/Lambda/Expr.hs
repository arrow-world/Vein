{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Vein.Core.Lambda.Expr where

import Numeric.Natural (Natural)
import Data.Fix (Fix(..))

data ExprF c r =
    Var Natural
  | Lam (Maybe r) r
  | App r [r]
  | Const c
  | Univ
  | Typing r r
  | Hole
  deriving (Eq,Show,Functor,Foldable,Traversable)

data TypedExprF c r = TypedExprF { tefTerm :: ExprF c r , tefType :: ExprF c r }
  deriving (Eq,Show,Functor,Foldable,Traversable)

type Expr c = Fix (ExprF c)
type TypedExpr c = Fix (TypedExprF c)