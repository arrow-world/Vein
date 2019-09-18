{-# LANGUAGE DataKinds #-}

module Main where

import Vein.Core.Monoidal.Monoidal
import Vein.Core.SimpleSF
import Vein.Core.DTCV
import Vein.Core.Behavior
import Vein.Core.SS.SS
import Vein.Core.SS.Audio
import Vein.Core.SS.Time

sinT :: Double -> SF (Behavior Double) (DTCV 44100)
sinT f = (pureB (\t -> sin (2*pi*f*t))) >>>
  (Primitive (ToDTCV_FromBehaviorFloat :: ToDTCV_FromBehaviorFloat 44100))

main :: IO ()
main = do
  -- let llvm = compile (sinT 440) TimeSource AudioSink
  -- toIO llvm
  return ()
