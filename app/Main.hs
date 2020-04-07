module Main where

import Vein.Core.Monoidal.Monoidal
import qualified Vein.Syntax.Lexer as Lexer
import qualified Vein.Syntax.Parser as Parser
import qualified Vein.Syntax.AST as AST
import qualified Vein.Syntax.PrettyPrinter as PrettyPrinter
import qualified Vein.Core.Module as Module

import qualified Option as Option

import Data.Text.Prettyprint.Doc (pretty)
import Data.Text.Prettyprint.Doc.Util (putDocW)
import           Data.Foldable                  ( for_ )
import System.Console.Terminal.Size as TerminalSize

{-
sinT : Double -> SF (Behavior Double) (DTCV 44100)
sinT f = (pureB (\t -> sin (2*pi*f*t))) >>>
  (Primitive (ToDTCV_FromBehaviorFloat :: ToDTCV_FromBehaviorFloat 44100))
-}

parse :: String -> Either String (AST.ParsedEnv Module.QN AST.LocatedExpr)
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

  width <- TerminalSize.size >>= return . maybe 80 TerminalSize.width

  for_ (zip inputFiles srcs) $ \(file,src) -> either print id $ do
    src' <- parse src
    return $ do
      putStr $ "File: " ++ file ++ "\n\n"
      putDocW width $ PrettyPrinter.prettyParsedEnv src'
      putStr "\n\n\n"

  return ()
