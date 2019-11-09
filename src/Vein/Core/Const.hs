{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}

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
import qualified LLVM.AST.Instruction as LAI
import qualified LLVM.AST.IntegerPredicate as IntegerPredicate
import qualified Data.Text as T
import qualified Data.Map.Lazy as Map
import qualified Data.Default as Default
import Control.Monad.State (State, get, modify, StateT, lift, state, runState)
import Data.Char (isAscii)
import Data.String (fromString)
import Data.Word (Word32)
import Numeric.Natural (Natural)
import Control.Arrow (left)
import Control.Monad.State (MonadFix)
import LLVM.AST.Instruction ( Named ((:=)) )
import LLVM.AST.Operand ( Operand (ConstantOperand) )
import LLVM.IRBuilder.Monad (MonadIRBuilder, emitInstr, block, freshUnName)
import LLVM.IRBuilder.Instruction (add, sub, mul, sdiv, br, condBr, icmp, phi)
import LLVM.AST.Constant ( Constant (Int), integerBits, integerValue
                         , sizeof, unsignedIntegerValue
                         )
import LLVM.AST.Typed ( Typed (typeOf) )

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

compileFuncPrim :: (MonadFix m, MonadIRBuilder m) => M.QN -> [Value] -> [Operand] -> m Operand
compileFuncPrim f params ops = case T.unpack $ M.showQN f of
  "Data.Binary.Int.add" -> add a b where [a,b] = ops
  "Data.Binary.Int.sub" -> sub a b where [a,b] = ops
  "Data.Binary.Int.mul" -> mul a b where [a,b] = ops
  "Data.Binary.Int.div" ->
    mdo
      isZero <- icmp IntegerPredicate.EQ b $ ConstantOperand (Int nBits 0)
      condBr isZero ifThen ifElse

      ifThen <- block
      tVal <- sdiv a b
      br ifExit

      ifElse <- block
      fVal <- sdiv a b

      ifExit <- block
      phi [(tVal, ifThen), (fVal, ifElse)]
    where
      [a,b] = ops
      LA.IntegerType nBits = typeOf a

data Error =
    TypeCompilationError TypeCompilationError
  | DoCoFnError
  deriving (Eq, Show)


assign :: (m -> [a] -> [a]) -> Morphism m o -> [a] -> [a]
assign f m xs =
  case m of
    Compose m1 m2 -> assign f m2 $ assign f m1 xs
    ProductM m1 m2 -> assign f m1 xs ++ assign f m2 xs
    Morphism m' -> f m' xs
    Id _ -> xs
    UnitorL x -> xs
    UnitorR x -> xs
    UnunitorL _ -> xs
    UnunitorR _ -> xs
    Assoc _ _ _ -> xs
    Unassoc _ _ _ -> xs

lenOfOb :: Object a -> Int
lenOfOb (ProductO x y) = lenOfOb x + lenOfOb y
lenOfOb _ = 1

flattenOb :: Object a -> [a]
flattenOb (Object x) = [x]
flattenOb Unit = []
flattenOb (ProductO x y) = flattenOb x ++ flattenOb y

stdenv :: Env
stdenv = Map.empty

maybeType :: Value -> Either TypeCompilationError LA.Type
maybeType t = compileTypeValue stdenv TypeVal { typeCtor = M.readQN $ T.pack "Maybe"
                                              , typeParams = [t]
                                              }


compileType :: Env -> Type -> Either TypeCompilationError LA.Type
compileType env t =
  case t of
    Object (M.Named tv _) -> compileTypeValue env tv

    Unit -> Right LA.VoidType

    ProductO x y -> do
      elementTypes <- mapM (compileType env) [x,y]
      return $ LA.StructureType
        { LA.isPacked = False
        , LA.elementTypes = elementTypes
        }

compileTypeValue :: Env -> TypeValue -> Either TypeCompilationError LA.Type
compileTypeValue env tv =
  case Map.lookup (typeCtor tv) env of
    Just def -> case def of
      DefType x -> compileType env x
      DefFunc fn -> lookupPrimTy

    Nothing -> lookupPrimTy
  where
    lookupPrimTy = case compilePrimTypes env tv of
      Left UnsupportedPrimType -> Left UndefinedType
      x -> x

compilePrimTypes :: Env -> TypeValue -> Either TypeCompilationError LA.Type
compilePrimTypes env tv = case T.unpack $ M.showQN (typeCtor tv) of
  "Data.Either" -> do
    let [Val a xs, Val b ys] = typeParams tv

    ts <- mapM (compileTypeValue env) [TypeVal a xs, TypeVal b ys]

    let sz = maximum $ fmap (unsignedIntegerValue . sizeof) ts
        byteBits = 8
        
    return $ LA.IntegerType $ fromIntegral $ sz * byteBits
  
  "Data.Pair" -> do
    let [Val a xs, Val b ys] = typeParams tv
    ts <- mapM (compileTypeValue env) [TypeVal a xs, TypeVal b ys]
    return $  LA.StructureType
                { LA.isPacked = False
                , LA.elementTypes = ts
                }
        
  _ -> Left UnsupportedPrimType

data TypeCompilationError =
    UndefinedType
  | UnsupportedPrimType
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