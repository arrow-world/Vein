{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Vein.Temporal.Event where

import Vein.Core.Monoidal.Monoidal ( Object (..) , lenOfOb , flattenOb , (><) )
import Vein.Core.Monoidal.CompactClosed ( DualityM (DualityM, Ev, Cv)
                                        , CompactClosedCartesianMorphismF
                                        , CompactClosedCartesianMorphism
                                        )

import qualified Vein.Core.Module as M
import qualified Vein.Core.Const as C
import qualified Vein.Core.Monoidal.Monoidal as Mo
import qualified Vein.Core.Monoidal.CompactClosed as CC

import qualified LLVM.AST as LA
import qualified Data.Map.Lazy as Map
import LLVM.AST.Operand ( Operand (ConstantOperand) )
import LLVM.IRBuilder.Monad ( MonadIRBuilder , block , emitBlockStart , fresh , IRBuilder )
import LLVM.IRBuilder.Monad as LIM
import LLVM.IRBuilder.Instruction ( br, condBr )
import Control.Monad ((>>))
import Control.Monad.State ( State , modify , get , runState , MonadFix )
import Data.Foldable ( foldrM )
import Control.Monad.Reader (Reader, ask, asks, runReader, ReaderT, mapReaderT)
import Control.Monad.Except (throwError, ExceptT (ExceptT))
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import Data.Fix ( Fix (..), cataM )
import Data.Either ( partitionEithers )
import Numeric.Natural ( Natural )
import Control.Arrow (arr, Kleisli, (>>>), (&&&), (+++))
import qualified Control.Category as Category

{-
 - type Event : Type -> Type
 - mealy : (a -> b) -> s -> Event a -> Event b
 - merge : Event a -> Event b -> Event (Either a b)
 - route : Event (Either a b) -> Event a * Event b
 -
 - type Behavior : Type -> Type
 - map : (a -> b) -> Behavior a -> Behavior b
 - 連続時間より一旦離散時間やったほうが、逆に連続時間もわかりそう 
 -
 - 離散時間のbehaviorは興味深くて、直前の値を取得できるので(遅延素子があるので)
 - おそらくeventと等価になる(全てのeventに関する作用素をbehaviorの作用素だけで構成できる)。
 - つまり、扱いやすい。
 - 一方、連続時間の場合、直前の値が取れないので、eventの方が強くなるっぽい。
 - なんかhaskellのデータ型がF代数にも余F代数にもなるって話を思いだした
 -
 - まとめると、離散連続それぞれの場合にeventとbehaviorという概念が考えられて、
 - 離散の場合二つは等価だが連続になるとeventのほうが強いらしい。
 - 逆に、連続時間のbehaviorは使用可能な作用素が少ない(弱い)ってことは制約が少ないってことだから、
 - つまりはbehaviorの方が真に一般的で、つまりeventよりも真に信号の表現力が高い？(まあそれはそう...か？)
 - これは直観的に納得できるし、なぜ連続なbehaviorを求めていたかの答えになるだろう。
 -
 - あと現実的には作用素の計算時間に由来する遅延を考える必要があるけど、それはまた別の話のようだ
 -
 - 作用素の遅延は二つに分類できて、必ず同じ時間だけ遅延する定遅延と、
 - 入力に関係なくバラバラな時間で遅延する不定遅延に分類できる。
 - 定遅延はそれを作用素の定義に組み込めばよさそうだけど、不定遅延はよくわからない
 -
 - 不定遅延は十分長い遅延作用素によって吸収することができるはずで、これによって良い感じになりそう
 - 
 - 型の意味論について
 - 型は高々可算個である項に対して型付けされる以上、必然的に高々可算な集合を表現するものだと思っていたが、
 - よく考えれば解釈写像の値域と終域の違いで、確かに値域は高々可算だが終域は任意の濃度でいいわけであって、
 - つまりは表現的意味論的に考えて型は任意濃度の集合を表現すると考えてもいい
 - (ただしその言語上で構成できるのは高々可算個の値だけだけど)という解釈が妥当では
 -
 - なんでこんな事を考えるかというと、もしかしたら外から入ってくる値はその言語上で構成できないような
 - 値であるという可能性を考えられる余地が生まれて、信号型がこの状況なので
 -
 - じゃあ、解釈集合がその型が付く項集合と同型な(パターンマッチができる)型のことをデータ型、
 - そうじゃなくてパターンマッチができないようなのを集合論的型って言う。
 -}

{-
 - とりあえずEventオンリーのSFからLLVMにコンパイルするやつ作るん
 - Time = ℝ 
 - [| Event a |] = ℕ → (Time × [| a |])
 - [| Event a -> Event b |] =
 -   { F ∈ [| Event a |] → [| Event b |] | T ∈ Time . F(D_T(E)) = D_T(F(E)) }
 -
 - route (merge x y) = (x,y)
 - let (y,z) = route x in merge y z = x
 -
 - set Event : Type -> Type
 - set Behavior : Type -> Type
 -}

{- コンパイル方法: いくつか特定のコネクタ(SystemIOConnector, 外部環境との通信用)を特別視して、それのみを持つコンポネントをコンパイル可能とする。
 - 例えば、TimeメスコネクタとAudio.Sinkオスコネクタを持つコンポネントはコンパイルできる。
 - 例
 -
 - CosPlay : Component (-Temporal.Time >< Temporal.System.Audio.Sink)
 - CosPlay t s = s << toAudioSink $ map cos (2*Math.pi*440*t)
 -}

require = fmap (M.readQN . T.pack)
  [ "Temporal.Event"
  , "Temporal.Event.mealy"
  , "Temporal.Event.merge"
  , "Temporal.Event.route"
  ]

type ComponentF = CompactClosedCartesianMorphismF Value TypeValue
type Component = Fix ComponentF

newtype ComponentNumberedF r = ComponentNumberedF (ComponentF r, Maybe Natural)
  deriving (Eq, Show, Functor, Foldable, Traversable)
type ComponentNumbered = Fix ComponentNumberedF

data Definition =
    DefComponent ([C.Value] -> Component)
  | DefTypeAlias ([C.Value] -> Type)

type Env = M.ModuleMap Definition


-- compileCom :: MonadIRBuilder m => Component -> Reader Env (Either ScanError (ComProc m LA.Name [Operand]))
-- compileCom = undefined

{-
data JumpableCodeBuilder mBuilder mCode a = JumpableCodeBuilder
  { jump :: (a -> mCode a) -> mCode a -> mBuilder (mCode a , mCode ())
  }

llvmJumpCodeBuilder :: (MonadIRBuilder m, MonadIRBuilder m') => JumpableCodeBuilder m m' [Operand]
llvmJumpCodeBuilder = JumpbleCodeBuilder
  { jump = \cont ctx -> do
      xs <- ctx
      label <- block
      ys <- cont xs
      return $ (  , br label )
  }
-}

data InputPort =
    LeftInputPort Natural
  | RightInputPort Natural
    deriving (Eq, Show)

data OutputPort =
    RightOutputPort Natural
  | LeftOutputPort Natural
    deriving (Eq, Show)

{-
data MonadCodeBuilder m a => ComProc m a =
  ComProc { outputs :: [(OutputPort , a -> m a)] , terminals :: [(a -> m (), OutputPort , m a)] }
type Visitor m a = Value -> ReaderT Env (ExceptT ScanError m) (ComProc m a)
type Output m a = (OutputPort , a -> m a)
type Terminal m a = (a -> m () , OutputPort , m a)
type OutputNoDirection m a = (Natural , a -> m a)

buildCode :: MonadCodeBuilder m a => Visitor m a -> Component -> InputPort
                                    -> ReaderT Env (ExceptT ScanError m) (ComProc m a)
buildCode visitor (Fix c) port =
  let (CC.CompactClosedCartesianMorphismF c') = c in
    case c' of

      Mo.Cartesian (CC.DualityM (Mo.Braided f)) -> case f of

        Mo.Id x -> id' x
        
        Mo.Compose g h -> case port of
          LeftInputPort _ -> do
            (lOsG , rOsG , tsG) <- buildCode' g port
            (lOsH , rOsH , tsH) <- buildCodes h LeftInputPort rOsG
            (lOsI , rOsI , tsI) <- buildInnerInteraction lOsH
            return $ ComProc ( (map leftOutput (lOsG ++ lOsH ++ lOsI)) ++ (map rightOutput (rOsG ++ rOsH ++ rOsI)) ) (tsG ++ tsH ++ tsI)

          RightInputPort _ -> do
            (lOsH , rOsH , tsH) <- buildCode' h port
            (lOsG , rOsG , tsG) <- buildCodes g RightInputPort lOsH
            (lOsI , rOsI , tsI) <- buildInnerInteraction rOsG
            return $ ComProc ( (map leftOutput (lOsH ++ lOsG ++ lOsI)) ++ (map rightOutput (rOsH ++ rOsG ++ rOsI)) ) (tsH ++ tsG ++ tsI)

          where
            -- buildCode' :: CodeBuilder m l a => Component -> InputPort -> ReaderT Env (ExceptT ScanError m) ([OutputNoDirection m a] , [OutputNoDirection m a] , [Terminal m a])
            buildCode' component port' = do
              ComProc os ts <- buildCode visitor component port'
              let (lOs , rOs) = partitionOutputs os
              return (lOs , rOs , ts)

            -- buildCodes :: CodeBuilder m l a => Component -> (Natural -> InputPort) -> [OutputNoDirection m a] -> ReaderT Env (ExceptT ScanError m) ([OutputNoDirection m a] , [OutputNoDirection m a] , [Terminal m a])
            buildCodes component director inputs = do
              codes <- sequence $ map ((buildCode visitor component) . director . fst) inputs
              let ts = concat $ map terminals codes
              let (lOs , rOs) = partitionOutputs $ concat $ map outputs codes
              return (lOs , rOs , ts)

            -- buildInnerInteraction :: CodeBuilder m l a => [OutputNoDirection m a] -> ReaderT Env (ExceptT ScanError m) ([OutputNoDirection m a] , [OutputNoDirection m a] , [Terminal m a])
            buildInnerInteraction [] = pure $ ([],[],[])
            buildInnerInteraction lOsH = do
              (lOsG , rOsG , tsG) <- buildCodes g RightInputPort lOsH
              (lOsH , rOsH , tsH) <- buildCodes h LeftInputPort rOsG
              (lOsI , rOsI , tsI) <- buildInnerInteraction lOsH
              return ( (lOsG ++ lOsI) , (rOsH ++ rOsI) , (tsG ++ tsH ++ tsI) )

            -- leftOutput :: CodeBuilder m l a => OutputNoDirection m a -> Output m a
            leftOutput (op,code) = (LeftOutputPort op , code)

            -- rightOutput :: CodeBuilder m l a => OutputNoDirection m a -> Output m a
            rightOutput (op,code) = (RightOutputPort op , code)

            plusCodes (ComProc os ts) (ComProc os' ts') = ComProc (os++os') (ts++ts')

            -- partitionOutputs :: CodeBuilder m l a => [Output m a] -> ([OutputNoDirection m a] , [OutputNoDirection m a])
            partitionOutputs =
              partitionEithers .
                map
                  ( \(op,code) ->
                      case op of
                        LeftOutputPort n -> Left (n,code)
                        RightOutputPort n -> Right (n,code)
                  )

        Mo.ProductM g h -> case port of
          LeftInputPort n -> do
            (outboundG,_) <- dom' g >>= splitDuality'
            (outboundH,_) <- dom' h >>= splitDuality'

            case (fromIntegral $ length outboundG , fromIntegral $ length outboundH) of
              (m,m') | n < m -> buildCode' g $ LeftInputPort n
              (m,m') | m <= n && n < m+m' -> buildCode' h $ LeftInputPort $ n - m
              _ -> throwError $ InvalidInputPort port

          RightInputPort n -> do
            (_,inboundG) <- cod' g >>= splitDuality'
            (_,inboundH) <- cod' h >>= splitDuality'

            case (fromIntegral $ length inboundG , fromIntegral $ length inboundH) of
              (m,m') | n < m -> buildCode' g $ RightInputPort n
              (m,m') | m <= n && n < m+m' -> buildCode' h $ RightInputPort $ n - m
              _ -> throwError $ InvalidInputPort port
          
        Mo.UnitorL x -> id' x
        Mo.UnitorR x -> id' x
        Mo.UnunitorL x -> id' x
        Mo.UnunitorR x -> id' x
        Mo.Assoc x y z -> id' ((x >< y) >< z)
        Mo.Unassoc x y z -> id' ((x >< y) >< z)
        Mo.Morphism val -> visitor val
      
      Mo.Cartesian (DualityM (Mo.Braid x y)) -> do
        (outboundX,inboundX) <- splitDuality' x
        (outboundY,inboundY) <- splitDuality' y

        case port of
          LeftInputPort n -> case (fromIntegral $ length outboundX , fromIntegral $ length outboundY) of
            (m,m') | n < m'             -> return $ ComProc [(RightOutputPort $ m + n , return)] []
            (m,m') | m <= n && n < m+m' -> return $ ComProc [(RightOutputPort $ n - m , return)] []
            _                           -> throwError $ InvalidInputPort port

          RightInputPort n -> case (fromIntegral $ length inboundX , fromIntegral $ length inboundY) of
            (m,m') | n < m'             -> return $ ComProc [(LeftOutputPort $ m + n , return)] []
            (m,m') | m <= n && n < m+m' -> return $ ComProc [(LeftOutputPort $ n - m , return)] []
            _                           -> throwError $ InvalidInputPort port
      
      Mo.Cartesian (Ev x) -> do
        (outbound,inbound) <- splitDuality' x

        let lo = fromIntegral $ length outbound
        let li = fromIntegral $ length inbound

        case port of
          LeftInputPort n | n < lo                -> return $ ComProc [(LeftOutputPort $ li + n, return)] []
          LeftInputPort n | lo <= n && n < lo+li  -> return $ ComProc [(LeftOutputPort $ n - lo, return)] []
          _                                       -> throwError $ InvalidInputPort port
      
      Mo.Cartesian (Cv x) -> do
        (outbound,inbound) <- splitDuality' x

        let li = fromIntegral $ length inbound
        let lo = fromIntegral $ length outbound

        case port of
          RightInputPort n | n < li               -> return $ ComProc [(RightOutputPort $ lo + n, return)] []
          RightInputPort n | li <= n && n < li+lo -> return $ ComProc [(RightOutputPort $ n - li, return)] []
          _                                       -> throwError $ InvalidInputPort port
        
        {-
          +-  cv  -+
          |        |
          |  +-----+--> X_outbound
          |  |     |
          |  | +---+--> X_inbound*
          |  | |   |
          |  | |   |
          |  | +---+<-- X_inbound
          |  |     |
          |  +-----+<-- X_outbound*
          |        |
          +--------+
        -}
      
      Mo.Diag x -> do
        (outbound,inbound) <- splitDuality' x

        let lo = fromIntegral $ length outbound
        let li = fromIntegral $ length inbound

        case port of
          LeftInputPort n | n < lo ->
            return $ ComProc [(RightOutputPort n , return) , (RightOutputPort $ lo + n , return)] []

          RightInputPort n | fromIntegral n < li*2 -> do
            l <- lift $ lift $ uniqueLabel
            v <- lift $ lift $ uniqueLabel
            return $ ComProc [] [(\x -> save v x >> goto l , LeftOutputPort n , label l >> load v)]
          
          _ -> throwError $ InvalidInputPort port
      
      Mo.Aug x -> do
        (outbound,_) <- splitDuality' x
        case port of
          LeftInputPort n | n < (fromIntegral $ length outbound)  -> return $ ComProc [] []
          _                                                       -> throwError $ InvalidInputPort port

  where
    id' x = do
      (outbound,inbound) <- splitDuality' x
      case port of
        LeftInputPort n | fromIntegral n < length outbound -> pure $ ComProc [(RightOutputPort n , return)] []
        RightInputPort n | fromIntegral n < length inbound -> pure $ ComProc [(LeftOutputPort n , return)] []
        _ -> throwError $ InvalidInputPort port

    buildCode' = buildCode visitor

    splitDuality' = liftExpandTypeErr . splitDuality
    flattenType' = liftExpandTypeErr . flattenType

    -- liftExpandTypeErr :: Monad m => ReaderT Env (Either ExpandTypeError) a -> ReaderT Env (ExceptT ScanError m) a
    liftExpandTypeErr = mapReaderT $ ExceptT . return . either (Left . ExpandTypeError) Right

    doco' = liftDoCoErr . doco
    dom' = liftDoCoErr . dom
    cod' = liftDoCoErr . cod

    -- liftDoCoErr :: Monad m => Maybe a -> ReaderT Env (ExceptT ScanError m) a
    liftDoCoErr = maybe (throwError DoCoError) return 
-}


newtype Counter a = Counter (State Natural a)
  deriving (Functor, Applicative, Monad)

count :: Counter Natural
count = Counter $ get <* modify (+1)

runCounter :: Counter a -> (a , Natural)
runCounter (Counter m) = runState m 0

numbering :: Component -> Counter ComponentNumbered
numbering (Fix c) =
  let CC.CompactClosedCartesianMorphismF m = c in
    case m of
      Mo.Diag x -> do
        n <- count
        c' <- traverse numbering c
        return $ Fix $ ComponentNumberedF (c' , Just n)
      
      _ -> do
        c' <- traverse numbering c
        return $ Fix $ ComponentNumberedF (c' , Nothing)

data TypeValue =
    TypeValue { typeCtor :: M.QN , typeParams :: [C.Value] }
  deriving (Eq, Show)

data Value =
    Val { valCtor :: M.QN , valParams :: [C.Value] }
  deriving (Eq, Show)

type Type = CC.CompactClosedCartesianObject TypeValue

doco = CC.docoCompactClosedCartesianMorphism docoVal
dom = (fmap fst) . doco
cod = (fmap snd) . doco

flattenType :: Type -> ReaderT Env (Either ExpandTypeError) [CC.D TypeValue]
flattenType x = Mo.flattenOb <$> expandType x

splitDuality :: Type -> ReaderT Env (Either ExpandTypeError) ([Type],[Type])
splitDuality x = (partitionEithers . fmap toEither) <$> flattenType x
  where
    toEither x = case x of
      CC.D x -> Left $ Object $ CC.D x
      CC.Dual x -> Right x


expandType :: Type -> ReaderT Env (Either ExpandTypeError) Type
expandType x = case x of
  Mo.Unit -> pure $ Mo.Unit

  Mo.ProductO x y -> Mo.ProductO <$> expandType x <*> expandType y

  Mo.Object (CC.Dual x) -> do
    x' <- expandType x
    case x' of
      Mo.Object (CC.Dual y) -> expandType y
      y -> pure $ Mo.Object $ CC.Dual y

  Mo.Object (CC.D (TypeValue ctor params)) -> do
    env <- ask
    case Map.lookup ctor env of
      Just (DefTypeAlias t) -> pure $ t params
      Just (DefComponent _) -> throwError UnexpectedComponentDefinition
      Nothing -> throwError Undefined

data ExpandTypeError =
    UnexpectedComponentDefinition
  | Undefined


docoVal :: Value -> Maybe (Type, Type)
docoVal = undefined


data DelayedMealy = DM { initState :: C.Value , trans :: C.Function }

compileDM ::  (MonadFix m, MonadIRBuilder m) =>
                DelayedMealy -> Reader Env (Either CompileError ([Operand] -> m Operand))
compileDM DM{initState, trans} = asks $ \env -> undefined


data Source m = Source { poll :: m Operand }
data Sink m = Sink { push :: [Operand] -> m () }

type Sources m = M.ModuleMap (Source m)
type Sinks m = M.ModuleMap (Sink m)
type SSs m = (Sources m, Sinks m)

eventLoop :: (MonadIRBuilder m, MonadFix m) => [Source m] -> [Sink m] -> Component -> m ()
eventLoop ins outs com = do
  loop <- block

  sequence $
    fmap
      ( \Source{poll} ->
        (
          mdo
            e <- poll
            condBr (C.isJust e) ifThen ifElse

            ifThen <- block
            e' <- C.unwrap e

            ifElse <- block

            return ()
        )
      )
      ins

  br loop

data CompileError

data ScanError =
    DoCoError
  | ExpandTypeError ExpandTypeError
  | InvalidInputPort InputPort