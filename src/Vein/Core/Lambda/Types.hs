module Vein.Core.Lambda.Types where

import Vein.Core.Lambda.Expr (Expr)
import Vein.Core.Module as M

import Numeric.Natural (Natural)

data Const =
    NewType { paramTypes :: [Expr M.QN] }
    deriving (Eq, Show)