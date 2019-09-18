{-# LANGUAGE MultiParamTypeClasses #-}

module Vein.Core.Behavior where

import Vein.Core.SimpleSF

data Behavior a = Behavior

instance Signal (Behavior a) where

data PureB a b = PureB (a -> b)
instance PrimitiveSF (PureB a b) (Behavior a) (Behavior b)

pureB :: (a -> b) -> SF (Behavior a) (Behavior b)
pureB = Primitive . PureB
