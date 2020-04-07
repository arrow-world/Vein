{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Vein.Core.Lambda.Expr where

import qualified Vein.Core.Module as M
import qualified Vein.Syntax.AST as AST

import Numeric.Natural (Natural)
import Data.Fix (Fix(..))

type Const = Natural

data ExprF pat r =
    Var Natural
  | Lam (Abs r)
  | App r r
  | Const Const
  | Univ
  | Typing r r
  | Pi (Abs r)
  | Subst (Subst r) r
  | Case [Clause pat r]
  | Primitive (Primitive r)
  deriving (Eq,Show,Functor,Foldable,Traversable)

data ExprFWith a r = ExprFWith a (ExprF (TypedPat a) r)
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

data PatF r =
    PatCon { datatype :: Const , conId :: Natural , args :: [r] }
  | PatList [r]
  | PatTuple [r]
  | PatLit AST.Literal
  | PatWildcard
  deriving (Eq,Show,Functor,Foldable,Traversable)

data PatFWith a r = PatFWith a (PatF r)
  deriving (Eq,Show,Functor,Foldable,Traversable)

data TypedPatF a r = TypedPatF { tpfType :: ExprFWith a r , tpfPat :: PatFWith a r }
  deriving (Eq,Show,Functor,Foldable,Traversable)

type TypedPat a = Fix (TypedPatF a)


data Clause pat e = Clause pat e
  deriving (Eq,Show,Functor,Foldable,Traversable)