{-# LANGUAGE MultiParamTypeClasses #-}

module Vein.Core.Const where

import qualified Vein.Core.Compile as Compile
import qualified Vein.Core.Module as M
import qualified Vein.Core.Component as Component
import Vein.Core.Monoidal.Monoidal ( Arrow (doco, domain, codomain)
                                   , (><)
                                   , Object (Object, Unit, ProductO)
                                   , Morphism
                                   , Traced
                                   )

import qualified LLVM.AST as LA
import qualified LLVM.AST.Global as LAG
import qualified LLVM.AST.Name as LAN
import qualified Data.Text as T
import qualified Data.Map.Lazy as Map
import Control.Monad.State (State, get, modify)
import Data.Char (isAscii)
import Data.String (fromString)
import Data.Word (Word32)


type Function = Morphism (Traced M.QN) M.QN
type Type = Object (M.Named M.QN)

data Definition =
    DefFunc Function
  | DefType Type
  deriving (Eq, Show)

type Env = M.ModuleMap Definition

compileFunc :: Env -> M.Named Function -> State UnNameId (Either Error LA.Definition)
compileFunc env c = do
  fname <- toLLVMName $ M.name c
  return $ Right $ LA.GlobalDefinition LA.functionDefaults
    { LAG.name = fname
    , LAG.parameters =
      ( [
        ]
      , False
      )
    }


data Error


compileType :: Env -> Type -> Either TypeCompilationError LA.Type
compileType env t =
  case t of
    Object (M.Named qn _) ->
      case Map.lookup qn env of
        Just def -> case def of
          DefType x -> compileType env x
          DefFunc _ -> Left UnexpectedFunction

        Nothing -> maybe (Left UndefinedType) Right (lookupPrimitiveTypes qn)

    Unit -> Right LA.VoidType

    ProductO x y -> do
      elementTypes <- mapM (compileType env) [x,y]
      return $ LA.StructureType
        { LA.isPacked = False
        , LA.elementTypes = elementTypes
        }

lookupPrimitiveTypes :: M.QN -> Maybe LA.Type
lookupPrimitiveTypes qn = case M.showQN qn of
  _ -> Nothing

data TypeCompilationError =
    UndefinedType
  | UnexpectedFunction
  deriving (Eq, Show)

type UnNameId = Word

toLLVMName :: M.Name -> State UnNameId LAN.Name
toLLVMName (M.Name name) =
  if all isAscii $ T.unpack name then
    pure $ LAN.Name $ fromString $ T.unpack name
  else
    genUnName

toLLVMNameM :: Maybe M.Name -> State UnNameId LAN.Name
toLLVMNameM (Just name) = toLLVMName name
toLLVMNameM Nothing = genUnName

genUnName :: State UnNameId LAN.Name
genUnName = do 
  n <- get
  modify (+1)
  return $ LAN.UnName n

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