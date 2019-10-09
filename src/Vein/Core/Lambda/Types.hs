module Vein.Core.Lambda.Types where

import Vein.Core.Lambda.Identifier

data TFun a = Fun a Integer
  deriving Eq

data Mono a =
    Var Integer
  | TApp (TFun a) [Mono a]
    deriving Eq

var :: Integer -> Maybe (Mono a)
var n
  | n >= 0    = Just $ Var n
  | otherwise = Nothing

tApp :: TFun a -> [Mono a] -> Maybe (Mono a)
tApp f xs =
    if n == fromIntegral (length xs) then
      Just $ TApp f xs
    else
      Nothing
  where
    (Fun _ n) = f

data Poly a = Poly Integer (Mono a)
  deriving Eq

poly :: Integer -> Mono a -> Maybe (Poly a)
poly n x
  | n >= 0    = Just $ Poly n x
  | otherwise = Nothing

type MonoIdent = Mono Identifier
type PolyIdent = Poly Identifier
