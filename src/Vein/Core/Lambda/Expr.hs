module Vein.Core.Lambda.Expr where

import Numeric.Natural (Natural)

data Expr c e =
    Var Natural
  | Const c
  | Lam Natural [Expr c e]
  | App (Expr c e) [Expr c e]
  | Expr e
    deriving Eq
