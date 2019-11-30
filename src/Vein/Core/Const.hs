{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Vein.Core.Const where

import qualified Vein.Core.Compile as Compile
import qualified Vein.Core.Module as M
import qualified Vein.Core.Component as Component
import qualified Vein.Core.Monoidal.Monoidal as Monoidal
import Vein.Core.Monoidal.Monoidal ( (><)
                                   , Object (Object, Unit, ProductO)
                                   , WithInternalHom (..)
                                   , Morphism
                                   , Traced (Trace, Traced)
                                   , TracedMorphism
                                   , Braided (..)
                                   , CartesianClosed (..)
                                   , Cartesian (..)
                                   , CartesianClosedMorphism
                                   , domA
                                   , codA
                                   , docoA
                                   , doco
                                   , docoTraced
                                   , docoTracedMorphism
                                   , docoLift
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
import Control.Arrow (left, (***))
import Data.Functor.Identity (Identity (Identity), runIdentity)
import Control.Monad ( (>=>) )
import Control.Monad.State (MonadFix)
import Control.Monad.Reader (Reader, ask, runReader, ReaderT)
import Control.Monad.Except (ExceptT)
import Data.Foldable ( foldrM )
import LLVM.AST.Instruction ( Named ((:=)) )
import LLVM.AST.Operand ( Operand (ConstantOperand) )
import LLVM.IRBuilder.Monad (MonadIRBuilder, emitInstr, block, freshUnName, IRBuilderT)
import LLVM.IRBuilder.Module ( MonadModuleBuilder, ParameterName (NoParameterName), function )
import LLVM.IRBuilder.Instruction ( add, sub, mul, sdiv, br, condBr, icmp, phi
                                  , insertValue, ret
                                  )
import LLVM.AST.Constant ( Constant (Int, Float, Undef, InsertValue), integerBits, integerValue
                         , sizeof, unsignedIntegerValue
                         )
import LLVM.AST.Typed ( Typed (typeOf) )
import LLVM.AST.Float ( SomeFloat (Single, Double) )

data Value =
    Val { valCtor :: M.QN , valParams :: [Value] }
  | ValBinInt { valBinInt :: Integer , valBits :: Word32 }
  | ValFP32 Float
  | ValFP64 Double
  | ValNat Natural
  deriving (Eq, Show)

data TypeValue =
    TypeVal { typeCtor :: M.QN , typeParams :: [Value] }
  deriving (Eq, Show)

type TypeBase = WithInternalHom TypeValue

type Function = CartesianClosedMorphism (Cartesian Value TypeBase) TypeValue
type Type = Object TypeBase


docoFn :: Env -> Function -> Maybe (Type, Type)
docoFn env f = doco (docoLift doco') f
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
    docoConst x = (Unit , Object $ WithInternalHom x)

docoValPrim :: M.QN -> [Value] -> Maybe (Type, Type)
docoValPrim ctor params = case T.unpack $ M.showQN ctor of
  _ -> Nothing


data Definition =
    DefFunc Function
  | DefTypeAlias Type
  deriving (Eq, Show)

type Env = M.ModuleMap Definition


compileFuncToDef :: (MonadFix m, MonadModuleBuilder m) =>
                      Env -> M.Named Function -> Either Error (m Operand)
compileFuncToDef env (M.Named f name) = do
  (dom, cod) <- maybe (Left DoCoFnError) Right $ docoFn env f
  paramType <- left TypeCompilationError $ compileType env dom
  retType <- left TypeCompilationError $ compileType env cod

  let Just name' = toLLVMName name

  function name' (fmap (,NoParameterName) [paramType]) retType <$>
    do
      f' <- runReader (compileFunc f) env
      return $ f' >=> ret

compileFunc ::  (MonadFix m, MonadIRBuilder m) =>
                  Function -> Reader Env (Either Error ([Operand] -> m Operand))
compileFunc f =
  do
    env <- ask
    let run r = runReader r env
    let docoVal' = (maybe (Left DoCoFnError) Right) . (docoVal env)

    let f' = assign (singleton . run . compileValue) docoVal' f

    return $ (.) <$> pure (aggregateOps []) <*> f'

  where
    singleton ::  (MonadFix m, MonadIRBuilder m) =>
                    Either Error ([Operand] -> m Operand) ->
                      Either Error ([Operand] -> m [Operand])
    singleton = fmap $ \g -> (fmap pure) . g

    aggregateOps :: MonadIRBuilder m => [LA.Type] -> m [Operand] -> m Operand
    aggregateOps types ops = do
      ops' <- ops

      foldrM
        ( \(op,i) agg -> insertValue agg op [i] )
        ( ConstantOperand $ Undef structType )
        ( zip ops' $ fmap fromIntegral [0..(length ops')] )
      where structType = LA.StructureType False types

compileValue :: (MonadFix m, MonadIRBuilder m) =>
                  Value -> Reader Env (Either Error ([Operand] -> m Operand))
compileValue v =
  case v of
    Val ctor params -> do
      env <- ask
      case Map.lookup ctor env of
        Just def -> case def of
          DefTypeAlias x -> return $ Left UnexpectedType
          DefFunc fn -> compileFunc fn
        
        Nothing ->  return $
                      (maybe (Left UndefinedValue) Right $ compileFuncPrim ctor)
                        <*> (pure params)
    
    ValBinInt n nBits -> constant $ return $ ConstantOperand $ Int nBits n
    ValFP32 x -> constant $ return $ ConstantOperand $ Float (Single x)
    ValFP64 x -> constant $ return $ ConstantOperand $ Float (Double x)
  where
    constant x = return $ Right $ \[] -> x

compileFuncPrim ::  (MonadFix m, MonadIRBuilder m) =>
                      M.QN -> Maybe ([Value] -> [Operand] -> m Operand)
compileFuncPrim f = case T.unpack $ M.showQN f of
  "Data.Binary.Int.add" -> Just $ \ [] [a,b] -> add a b
  "Data.Binary.Int.sub" -> Just $ \ [] [a,b] -> sub a b
  "Data.Binary.Int.mul" -> Just $ \ [] [a,b] -> mul a b
  "Data.Binary.Int.div" -> Just $ \ [] [a,b] ->
    mdo
      let LA.IntegerType nBits = typeOf a

      isZero <- icmp IntegerPredicate.EQ b $ ConstantOperand (Int nBits 0)
      condBr isZero ifThen ifElse

      ifThen <- block
      tVal <- sdiv a b >>= just
      br ifExit

      ifElse <- block
      let fVal = ConstantOperand $ nothing undefined

      ifExit <- block
      phi [(tVal, ifThen), (fVal, ifElse)]
  _ -> Nothing

just x = undefined
nothing a = ctor 0 (maybeTy a) 1
maybeTy a = undefined
isJust x = undefined
unwrap x = undefined

ctor n ty nBitsOfTag = InsertValue (Undef ty) (Int nBitsOfTag n) [0]

data Error =
    TypeCompilationError TypeCompilationError
  | DoCoFnError
  | UnexpectedType
  | UndefinedValue
  deriving (Eq, Show)

assign :: (Monad f, MonadIRBuilder g) =>
                (Value -> f ([a] -> g [a]))
            ->  (Value -> f (Type, Type))
            ->  Function
            ->  f ([a] -> g [a])
assign f doco' =
  assignMorphism
    (assignCartesianClosed (assignCartesian f doco') $ doco doco')
      (doco $ doco doco')

assignCartesian ::  (Monad f, MonadIRBuilder g) =>
                          (m -> f ([a] -> g [a]))
                      ->  (m -> f (Object o, Object o))
                      ->  Cartesian m o
                      ->  f ([a] -> g [a])
assignCartesian f doco m =
  case m of
    Cartesian m' -> f m'
    Diag _ -> return $ \[x] -> return [x,x]
    Aug _ -> return $ \[x] -> return []

assignCartesianClosed ::  (Monad f, MonadIRBuilder g) =>
                              (m -> f ([a] -> g [a]))
                          ->  (m -> f (Object (WithInternalHom o), Object (WithInternalHom o)))
                          ->  CartesianClosed m o
                          ->  f ([a] -> g [a])
assignCartesianClosed f doco m =
  case m of
    CartesianClosed m' -> f m'

assignMorphism :: (Monad f, Monad g) =>
                        (m -> f ([a] -> g [a]))
                    ->  (m -> f (Object o, Object o))
                    ->  Morphism m o
                    ->  f ([a] -> g [a])
assignMorphism f doco m =
  case m of
    Compose m1 m2 ->
      do
        m1' <- assign' m1
        m2' <- assign' m2
        return $ m1' >=> m2'

    ProductM m1 m2 ->
      do
        m1' <- assign' m1
        m2' <- assign' m2
        nofInputs_m1 <- nofInputs m1
        let splitInputs = splitAt nofInputs_m1

        return $ \xs -> 
          let (ys,zs) = splitInputs xs in
            (++) <$> m1' ys <*> m2' zs
      where
        nofInputs m' = lenOfOb <$> domA doco m'

    Morphism m' -> f m'
    Id _ -> id'
    UnitorL x -> id'
    UnitorR x -> id'
    UnunitorL _ -> id'
    UnunitorR _ -> id'
    Assoc _ _ _ -> id'
    Unassoc _ _ _ -> id'
  where
    id' = return $ return
    assign' = assignMorphism f doco


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
    Object t' -> case t' of
      WithInternalHom tv -> compileTypeValue env tv
      Hom x y ->  LA.FunctionType
                    <$> compileType' y
                    <*> (pure <$> compileType' x)
                    <*> (pure False)

    Unit -> Right LA.VoidType

    ProductO x y -> do
      elementTypes <- mapM compileType' [x,y]
      return $ LA.StructureType
        { LA.isPacked = False
        , LA.elementTypes = elementTypes
        }
  where
    compileType' = compileType env

compileTypeValue :: Env -> TypeValue -> Either TypeCompilationError LA.Type
compileTypeValue env tv =
  case Map.lookup (typeCtor tv) env of
    Just def -> case def of
      DefTypeAlias x -> compileType env x
      DefFunc fn -> Left UnexpectedFunc

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
  | UnexpectedFunc
  deriving (Eq, Show)


toLLVMName :: M.Name -> Maybe LAN.Name
toLLVMName (M.Name name) =
  if (all isAscii $ T.unpack name) && (not $ name == T.empty) then
    Just $ LAN.Name $ fromString $ T.unpack name
  else
    Nothing