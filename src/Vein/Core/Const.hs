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
                                   , Traced (Trace, Traced)
                                   , TracedMorphism
                                   , codA
                                   , docoA
                                   , docoTracedMorphism
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
import Control.Monad.Reader (Reader)
import LLVM.AST.Instruction ( Named ((:=)) )
import LLVM.AST.Operand ( Operand (ConstantOperand) )
import LLVM.IRBuilder.Monad (MonadIRBuilder, emitInstr, block, freshUnName)
import LLVM.IRBuilder.Module ( MonadModuleBuilder, ParameterName (NoParameterName), function )
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

type FunctionBase = Value

data TypeValue =
    TypeVal { typeCtor :: M.QN , typeParams :: [Value] }
  deriving (Eq, Show)

type TypeBase = TypeValue

type Function = TracedMorphism FunctionBase TypeBase
type Type = Object TypeBase


docoFn :: Env -> Function -> Maybe (Type, Type)
docoFn env f = docoTracedMorphism doco' f
  where doco' v = docoVal env v


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
    docoConst x = (Unit , Object x)

docoValPrim :: M.QN -> [Value] -> Maybe (Type, Type)
docoValPrim ctor params = case T.unpack $ M.showQN ctor of
  _ -> Nothing


data Definition =
    DefFunc Function
  | DefType Type
  deriving (Eq, Show)

type Env = M.ModuleMap Definition


compileFunc :: MonadModuleBuilder m => Env -> M.Named Function -> Either Error (m Operand)
compileFunc env (M.Named f name) = do
  (dom, cod) <- maybe (Left DoCoFnError) Right $ docoFn env f
  paramType <- left TypeCompilationError $ compileType env dom
  retType <- left TypeCompilationError $ compileType env cod

  let Just name' = toLLVMName name

  return $  function name' (fmap (,NoParameterName) [paramType]) retType
              undefined

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


assignTraced :: Env -> (m -> [a] -> [a]) -> Traced m o -> [a] -> [a]
assignTraced env f m xs =
  case m of
    Traced m' -> assign (assignTraced env f) m' xs
    Trace m' -> undefined

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


compileType :: Env -> Type -> Either TypeCompilationError LA.Type
compileType env t =
  case t of
    Object tv -> compileTypeValue env tv

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

    let szBody = maximum $ fmap (unsignedIntegerValue . sizeof) ts
        byteBits = 8
        
    return $  LA.StructureType
                { LA.isPacked = False
                , LA.elementTypes =
                    [ LA.IntegerType 1
                    , LA.IntegerType $ fromIntegral $ szBody * byteBits
                    ]
                }
  
  "Data.Pair" -> do
    let [Val a xs, Val b ys] = typeParams tv
    ts <- mapM (compileTypeValue env) [TypeVal a xs, TypeVal b ys]
    return $  LA.StructureType
                { LA.isPacked = False
                , LA.elementTypes = ts
                }
        
  "Data.Binary.Int" ->
    let [ValNat nBits] = typeParams tv in
      Right $ LA.IntegerType $ fromIntegral (nBits+1)
  
  "Data.Binary.Nat" ->
    let [ValNat nBits] = typeParams tv in
      Right $ LA.IntegerType $ fromIntegral nBits
  
  "Data.FloatingPoint.Double" ->
    let [] = typeParams tv in
      Right $ LA.FloatingPointType $ LA.DoubleFP

  _ -> Left UnsupportedPrimType

data TypeCompilationError =
    UndefinedType
  | UnsupportedPrimType
  deriving (Eq, Show)


toLLVMName :: M.Name -> Maybe LAN.Name
toLLVMName (M.Name name) =
  if (all isAscii $ T.unpack name) && (not $ name == T.empty) then
    Just $ LAN.Name $ fromString $ T.unpack name
  else
    Nothing