{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Vein.Core.Const where

import qualified Vein.Core.Compile as Compile
import qualified Vein.Core.Module as M
import qualified Vein.Core.Component as Component
import qualified Vein.Core.Monoidal.Monoidal as Monoidal
import Vein.Core.Monoidal.Monoidal ( (><)
                                   , Object (Object, Unit, ProductO)
                                   , Morphism
                                   , Traced
                                   , codA
                                   , docoA
                                   , docoTraced
                                   , Morphism (Id
                                              , Compose
                                              , ProductM
                                              , UnitorL
                                              , UnitorR
                                              , UnunitorL
                                              , UnunitorR
                                              , Assoc
                                              , Unassoc
                                              , Morphism
                                              )
                                   )
import Vein.Core.Cherry (Cherry (Fruit, Branch), CherryM)

import qualified LLVM.AST as LA
import qualified LLVM.AST.Global as LAG
import qualified LLVM.AST.Name as LAN
import qualified Data.Text as T
import qualified Data.Map.Lazy as Map
import Control.Monad.State (State, get, modify, StateT, lift, state, runState)
import Data.Char (isAscii)
import Data.String (fromString)
import Data.Word (Word32)
import Numeric.Natural (Natural)
import Control.Arrow (left)

data Value =
    Val { valCtor :: M.QN , valParams :: [Value] }
  | ValBinInt { valBinInt :: Integer , valBits :: Word32 }
  | ValFP32 Float
  | ValFP64 Double
  | ValNat Natural
  deriving (Eq, Show)

type FunctionBase = M.Named Value

data TypeValue =
    TypeVal { typeCtor :: M.QN , typeParams :: [Value] }
  deriving (Eq, Show)

type TypeBase = M.Named TypeValue

type Function = Morphism (Traced FunctionBase) TypeBase
type Type = Object TypeBase


docoFn :: Env -> Function -> Maybe (Type, Type)
docoFn env f = docoA (docoTraced doco') f
  where doco' (M.Named v _) = docoVal env v


docoVal :: Env -> Value -> Maybe (Type, Type)
docoVal env v =
  case v of
    Val { valCtor , valParams } ->
      case Map.lookup valCtor env of
        Just def -> case def of
          DefFunc f -> docoFn env f
          _ -> Nothing
        
        Nothing -> docoValPrim valCtor valParams

    ValBinInt { valBinInt , valBits } ->
      Just $ docoConst TypeVal { typeCtor = M.readQN (T.pack "Data.Binary.Int")
                               , typeParams = [ValNat $ fromIntegral valBits]
                               }
    
    ValFP32 f ->
      Just $ docoConst
        TypeVal { typeCtor = M.readQN (T.pack "Data.Binary.FP32") , typeParams = [] }
    
    ValFP64 f ->
      Just $ docoConst
        TypeVal { typeCtor = M.readQN (T.pack "Data.Binary.FP64") , typeParams = [] }
    
    ValNat n ->
      Just $ docoConst
        TypeVal { typeCtor = M.readQN (T.pack "Data.Natural.Nat") , typeParams = [] }

  where
    docoConst x = (Unit , Object (M.Named x $ M.Name $ T.pack ""))

docoValPrim :: M.QN -> [Value] -> Maybe (Type, Type)
docoValPrim ctor params = case T.unpack $ M.showQN ctor of
  _ -> Nothing


data Definition =
    DefFunc Function
  | DefType Type
  deriving (Eq, Show)

type Env = M.ModuleMap Definition

compileFunc :: Env -> M.Named Function -> StateT UnNameId (Either Error) LA.Definition
compileFunc env (M.Named f name) = do
  (dom, cod) <- lift $ maybe (Left DoCoFnError) Right $ docoFn env f
  paramType <- lift $ left TypeCompilationError $ compileType env dom
  retType <- lift $ left TypeCompilationError $ compileType env cod

  fname <- hoist $ toLLVMName name
  argname <- hoist $ genUnName
  
  lift $ Right $ LA.GlobalDefinition LA.functionDefaults
      { LAG.name = fname
      , LAG.parameters = ( [ LAG.Parameter paramType argname [] ] , False )
      , LAG.returnType = retType
      , LAG.basicBlocks =
          [
          ]
      }

  where
    hoist = state . runState


data Error =
    TypeCompilationError TypeCompilationError
  | DoCoFnError
  deriving (Eq, Show)


assign :: (m -> CherryM a -> Maybe (CherryM a)) -> Morphism m o -> CherryM a -> Maybe (CherryM a)
assign f m x =
  case m of
    Id _ -> Just x

    Compose m1 m2 -> (assign f m1 x) >>= (assign f m2)

    ProductM m1 m2 -> Branch <$> (assign f m1 x) <*> (assign f m2 x)

    UnitorL _ -> case x of
      Branch x y -> Just y
      _ -> Nothing

    UnitorR _ -> case x of
      Branch x y -> Just x
      _ -> Nothing
    
    UnunitorL _ -> Just $ Branch x (Fruit Nothing)

    UnunitorR _ -> Just $ Branch x (Fruit Nothing)

    Assoc _ _ _ -> case x of
      Branch (Branch x y) z -> Just $ Branch x (Branch y z)
      _ -> Nothing
    
    Unassoc _ _ _ -> case x of
      Branch x (Branch y z) -> Just $ Branch (Branch x y) z
      _ -> Nothing
    
    Morphism m1 -> f m1 x



compileType :: Env -> Type -> Either TypeCompilationError LA.Type
compileType env t =
  case t of
    Object (M.Named tv _) ->
      case Map.lookup (typeCtor tv) env of
        Just def -> case def of
          DefType x -> compileType env x
          DefFunc fn -> lookupPrimTy

        Nothing -> lookupPrimTy
      where
        lookupPrimTy = maybe (Left UndefinedType) Right (compilePrimitiveTypes tv)

    Unit -> Right LA.VoidType

    ProductO x y -> do
      elementTypes <- mapM (compileType env) [x,y]
      return $ LA.StructureType
        { LA.isPacked = False
        , LA.elementTypes = elementTypes
        }

compilePrimitiveTypes :: TypeValue -> Maybe LA.Type
compilePrimitiveTypes tv = case T.unpack $ M.showQN (typeCtor tv) of
  _ -> Nothing

data TypeCompilationError =
    UndefinedType
  deriving (Eq, Show)


type UnNameId = Word

toLLVMName :: M.Name -> State UnNameId LAN.Name
toLLVMName (M.Name name) =
  if (all isAscii $ T.unpack name) && (not $ name == T.empty) then
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