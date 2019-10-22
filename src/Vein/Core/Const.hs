{-# LANGUAGE MultiParamTypeClasses #-}

module Vein.Core.Const where

import qualified Vein.Core.Compile as Compile
import qualified Vein.Core.Module as M
import qualified Vein.Core.Component as Component
import Vein.Core.Monoidal.Monoidal (Arrow (doco), (><), Object (Object))

import qualified LLVM.AST as LA
import qualified LLVM.AST.Global as LAG
import qualified LLVM.AST.Name as LAN
import qualified Data.Text as T
import Control.Monad.State (State, get, modify)
import Data.Char (isAscii)
import Data.String (fromString)

data PrimitiveType =
    Nat
  | Int
  | Bool
  | Maybe (Object PrimitiveType)
    deriving (Eq, Show)

data PrimitiveFunction =
    NSucc
    deriving (Eq, Show)

instance Arrow PrimitiveFunction PrimitiveType where
  doco f = case f of
    nSucc -> (Object Nat, Object Nat)

type UnNameId = Word

compile :: Component.Env -> Component.NamedComponent
  -> State UnNameId (Either Error LA.Definition)
compile env c = do
  fname <- toLLVMName $ M.name c
  return $ Right $ LA.GlobalDefinition LA.functionDefaults {
    LAG.name = fname
  }

toLLVMName :: M.Name -> State UnNameId LAN.Name
toLLVMName (M.Name name) =
  if all isAscii $ T.unpack name then
    pure $ LAN.Name $ fromString $ T.unpack name
  else do
    n <- get
    modify (+1)
    return $ LAN.UnName n

data Error

{-
compiler = Compile.Compiler
  { Compile.compiler = compile
  , Compile.require = fmap (M.readQN . T.pack)
      [ "Data.Natural.Nat"
      , "Data.Natural.succ"
      , "Data.Natural.pred"
      , "Data.Natural.tryPred"
      , "Data.Natural.plus"
      , "Data.Natural.tryMinus"
      , "Data.Natural.monus"
      , "Data.Natural.times"
      , "Data.Natural.div"
      , "Data.Natural.mod"
      , "Data.Integral.Int"
      , "Data.Integral.succ"
      , "Data.Integral.pred"
      , "Data.Integral.plus"
      , "Data.Integral.minus"
      , "Data.Integral.times"
      , "Data.Integral.div"
      , "Data.Integral.mod"
      , "Data.Bool.Bool"
      , "Data.Bool.true"
      , "Data.Bool.false"
      , "Data.Bool.and"
      , "Data.Bool.or"
      , "Data.Bool.not"
      , "Data.Bool.if"
      ]
  }
-}