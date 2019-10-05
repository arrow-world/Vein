{-# LANGUAGE RecursiveDo #-}

module Vein.Core.Compile where

import qualified Vein.Core.SS.SS

import LLVM.Prelude (Int)
import LLVM.AST (Module)
import qualified LLVM.AST.IntegerPredicate as IntegerPredicate
import LLVM.AST.Name (Name(Name))
import LLVM.AST.Type (i32)
import LLVM.AST.Operand (Operand(ConstantOperand))
import LLVM.AST.Constant (Constant(Int))
import LLVM.IRBuilder.Module (buildModule, function, ParameterName(ParameterName))
-- import LLVM.IRBuilder.Monad
import LLVM.IRBuilder (block, named, icmp, condBr, br, add, phi, ret)
import Data.ByteString.Internal (c2w)
import Data.ByteString.Short (pack)
import Data.ByteString.Short.Internal (ShortByteString)

toSBS :: [Char] -> ShortByteString
toSBS = pack . (map c2w)

-- compile :: GroundedSF -> LLVM

{-
simple :: Module
simple = buildModule (toSBS "exampleModule") $ mdo
  function (Name $ toSBS "f") [(i32, ParameterName $ toSBS "a")] i32 $ \[a] -> mdo
    entry <- block `named` (toSBS "entry")
    cond <- icmp IntegerPredicate.EQ a (ConstantOperand(Int 32 0))
    ifThen <- block
    ifElse <- block `named` (toSBS "if.else")
    condBr cond ifThen ifElse
    trVal <- add a (ConstantOperand (Int 32 0))
    br ifExit
    flVal <- add a (ConstantOperand (Int 32 0))
    br ifExit
    ifExit <- block `named` (toSBS "if.exit")
    r <- phi [(trVal, ifThen), (flVal, ifElse)]
    ret r
-}
