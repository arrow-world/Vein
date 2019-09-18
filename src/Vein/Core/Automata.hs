{-# LANGUAGE MultiParamTypeClasses #-}

module Vein.Core.Automata where

import Vein.Core.SimpleSF
import Vein.Core.Event
import Vein.Core.Monoidal.Monoidal

data Mealy s σ λ = Mealy { s0 :: s
			 , t :: (s, σ) -> s
			 , g :: (s, σ) -> λ
			 }

instance PrimitiveSF (Mealy s σ λ) (Event σ) (Event λ) where
