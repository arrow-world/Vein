module Vein.Core.Lambda.Types where

import Vein.Core.Lambda.Expr (Expr)
import Vein.Core.Module as M

import Numeric.Natural (Natural)

data Type a =
    Unit
  | Univ Natural
  | Tuple [Type a]
  | TFun (Type a) (Type a)
  | TApp (Type a) [Type a]
  | Type a
    deriving (Eq, Show)

type TypedExpr c e a =
  Expr c (Either e (Expr c e, Type a))

data Const =
    NewType { paramTypes :: [Type M.QN] }
    deriving (Eq, Show)