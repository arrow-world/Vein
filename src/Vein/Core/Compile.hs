module Vein.Core.Compile where

import Vein.Core.Component (Component)
import qualified Vein.Core.Module as M

import qualified LLVM.AST as LLVM_AST

data Pass = Pass
    { compiler :: Component -> Either PassError LLVM_AST.Module
    , require :: [M.QN]
    }

compile :: [Pass] -> Component -> Either CompileError LLVM_AST.Module
compile [] _ = Left NoPass
compile (p:ps) f = case (compiler p) f of
  Left e -> case e of
    Unsupported _ -> compile ps f
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