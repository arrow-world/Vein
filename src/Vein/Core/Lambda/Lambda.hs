module Vein.Core.Lambda.Lambda where

import Data.Fix

data ExprUnfix c e =
    Var Integer
  | Const c
  | Lam Integer [e]
  | App e [e]
    deriving Show

data Expr c = Expr (Fix (ExprUnfix c))
  deriving Show

var :: Integer -> Expr c
var i = Expr $ Fix $ Var i

const :: c -> Expr c
const x = Expr $ Fix $ Const x

lam :: Integer -> [Expr c] -> Expr c
lam n xs = Expr $ Fix $
  Lam n $ map (\(Expr x) -> x) xs

app :: Expr c -> [Expr c] -> Expr c
app (Expr f) xs = Expr $ Fix $
  App f $ map (\(Expr x) -> x) xs
