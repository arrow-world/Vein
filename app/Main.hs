{-# LANGUAGE DataKinds #-}

module Main where

import Vein.Core.Monoidal.Monoidal

{-
sinT :: Double -> SF (Behavior Double) (DTCV 44100)
sinT f = (pureB (\t -> sin (2*pi*f*t))) >>>
  (Primitive (ToDTCV_FromBehaviorFloat :: ToDTCV_FromBehaviorFloat 44100))
-}

main :: IO ()
main = do
  -- let llvm = compile (sinT 440) TimeSource AudioSink
  -- toIO llvm
  return ()
