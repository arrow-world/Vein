{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Vein.Temporal.Event where

import Vein.Core.Monoidal.Monoidal ( Object (..) , lenOfOb , flattenOb )
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
import LLVM.IRBuilder.Monad ( MonadIRBuilder , block , emitBlockStart , fresh )
import LLVM.IRBuilder.Instruction ( br, condBr )
import Control.Monad.State ( MonadFix )
import Data.Foldable ( foldrM )
import Control.Monad.Reader (Reader, ask, asks, runReader, ReaderT, mapReaderT)
import Control.Monad.Except (throwError)
import qualified Data.Text as T
import Data.Fix ( Fix (..) )
import Data.Either ( partitionEithers )
import Numeric.Natural ( Natural )

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

type ComponentF = CompactClosedCartesianMorphismF M.QN TypeValue
type Component = Fix ComponentF


data Definition =
    DefComponent ([C.Value] -> Component)
  | DefTypeAlias ([C.Value] -> Type)

type Env = M.ModuleMap Definition


compileCom :: MonadIRBuilder m => Component -> Reader Env (Either ScanError (ComProc m LA.Name [Operand]))
compileCom = undefined


class Monad m => CodeBuilder m label where
  goto :: label -> m ()
  label :: label -> m ()

instance MonadIRBuilder m => CodeBuilder m LA.Name where
  goto = br
  label = emitBlockStart

data InputPort =
    LeftInputPort Natural
  | RightInputPort Natural
    deriving (Eq, Show)

data OutputPort =
    RightOutputPort Natural
  | LeftOutputPort Natural
    deriving (Eq, Show)

data CodeBuilder m l => ComProc m l a = ComProc { outputs :: [(OutputPort , a -> m a)] , terminals :: [a -> m ()] }
type Visitor = ()
type Output m a = (OutputPort , a -> m a)
type OutputNoDirection m a = (Natural , a -> m a)

buildCode :: CodeBuilder m l => Visitor -> Component -> InputPort
                                -> ReaderT Env (Either ScanError) (ComProc m l a)
buildCode visitor (Fix c) port =
  let (CC.CompactClosedCartesianMorphismF c') = c in
    case c' of

      Mo.Cartesian (CC.DualityM (Mo.Braided f)) -> case f of

        Mo.Id x -> do
          (outbound,inbound) <- splitDuality' x
          case port of
            LeftInputPort n | fromIntegral n < length outbound -> pure $ ComProc [(RightOutputPort n , return)] []
            RightInputPort n | fromIntegral n < length inbound -> pure $ ComProc [(LeftOutputPort n , return)] []
            _ -> throwError $ InvalidInputPort port
        
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
            buildCode' component port' = do
              ComProc os ts <- buildCode visitor component port'
              let (lOs , rOs) = partitionOutputs os
              return (lOs , rOs , ts)

            buildCodes :: CodeBuilder m l => Component -> (Natural -> OutputPort) -> [OutputNoDirection m a] -> ReaderT Env (Either ScanError) ([OutputNoDirection m a] , [OutputNoDirection m a] , [a -> m ()])
            buildCodes component director inputs = do
              codes <- sequence $ map ((buildCode visitor component) . director . fst) inputs
              let ts = concat $ map terminals codes
              let (lOs , rOs) = partitionOutputs $ concat $ map outputs codes
              return (lOs , rOs , ts)

            buildInnerInteraction :: CodeBuilder m l => [OutputNoDirection m a] -> ReaderT Env (Either ScanError) ([OutputNoDirection m a] , [OutputNoDirection m a] , [a -> m ()])
            buildInnerInteraction lOsH = do
              (lOsG , rOsG , tsG) <- buildCodes g RightInputPort lOsH
              (lOsH , rOsH , tsH) <- buildCodes h LeftInputPort rOsG
              (lOsI , rOsI , tsI) <- buildInnerInteraction lOsH
              return ( (lOsG ++ lOsI) , (rOsH ++ rOsI) , (tsG ++ tsH ++ tsI) )
            buildInnerInteraction [] = pure $ ([],[],[])

            leftOutput :: CodeBuilder m l => OutputNoDirection m a -> Output m a
            leftOutput (op,code) = (LeftOutputPort op , code)

            rightOutput :: CodeBuilder m l => OutputNoDirection m a -> Output m a
            rightOutput (op,code) = (RightOutputPort op , code)

            plusCodes (ComProc os ts) (ComProc os' ts') = ComProc (os++os') (ts++ts')

            partitionOutputs :: CodeBuilder m l => [Output m a] -> ([OutputNoDirection m a] , [OutputNoDirection m a])
            partitionOutputs =
              partitionEithers .
                map
                  ( \(op,code) ->
                      case op of
                        LeftOutputPort n -> Left (n,code)
                        RightOutputPort n -> Right (n,code)
                  )

  where
    splitDuality' = (mapReaderT liftExpandTypeErr) . splitDuality
    flattenType' = (mapReaderT liftExpandTypeErr) . flattenType

    liftExpandTypeErr :: Either ExpandTypeError a -> Either ScanError a
    liftExpandTypeErr = either (Left . ExpandTypeError) Right

data TypeValue =
    TypeValue { typeCtor :: M.QN , typeParams :: [C.Value] }
  deriving (Eq, Show)

type Type = CC.CompactClosedCartesianObject TypeValue


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


docoVal :: C.Value -> Maybe (Type, Type)
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
    DoCoFnError
  | ExpandTypeError ExpandTypeError
  | InvalidInputPort InputPort