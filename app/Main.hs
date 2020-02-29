module Main where

import Vein.Core.Monoidal.Monoidal
import qualified Vein.Syntax.Lexer as Lexer
import qualified Vein.Syntax.Parser as Parser

import qualified Option as Option

{-
sinT : Double -> SF (Behavior Double) (DTCV 44100)
sinT f = (pureB (\t -> sin (2*pi*f*t))) >>>
  (Primitive (ToDTCV_FromBehaviorFloat :: ToDTCV_FromBehaviorFloat 44100))
-}

parse :: String -> Either String (Parser.Top Parser.LocatedExpr)
parse s = Lexer.runAlex s Parser.parse

main :: IO ()
main = do
  -- let llvm = compile (sinT 440) TimeSource AudioSink
  -- toIO llvm

  opt <- Option.parse
  let inputFiles = Option.inputFiles opt
  let outputFile = Option.outputFile opt
  let verbose = Option.verbose opt

  srcs <- traverse readFile inputFiles
  print $ map parse srcs

  return ()
