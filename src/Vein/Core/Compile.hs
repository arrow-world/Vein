module Vein.Core.Compile where

import Vein.Core.Component (Component, Env)
import qualified Vein.Core.Module as M

import qualified LLVM.AST as LLVM_AST

data Compiler = Compiler
    { compiler :: Env -> Component -> Either PassError LLVM_AST.Module
    , require :: [M.QN]
    }

compile :: [Compiler] -> Env -> Component -> Either CompileError LLVM_AST.Module
compile [] _ _ = Left NoPass
compile (p:ps) env f = case (compiler p) env f of
  Left e -> case e of
    Unsupported _ -> compile ps env f
    otherwise -> Left $ PassError e
  Right m -> Right m

data CompileError =
    NoPass
  | PassError PassError
    deriving (Eq, Show)

data PassError =
    Unsupported UnsupportedError
    deriving (Eq, Show)

data UnsupportedError =
    SignalType
  | SystemIOConnector
    deriving (Eq, Show)