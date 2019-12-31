{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ApplicativeDo #-}
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
import Control.Monad.Reader (Reader, ask, asks, runReader, ReaderT)
import qualified Data.Text as T
import Data.Fix ( Fix (..) )
import Data.Either ( partitionEithers )

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


type Cont m = [Operand] -> m ()
data MonadIRBuilder m => OnSends m = OnSends { contOnSends :: [Cont m] }
data MonadIRBuilder m => OnRecvs m = OnRecvs { contOnRecvs :: [Cont m] }
data Interface = Interface { blockName :: LA.Name , varNames :: [LA.Name] }
data Flow m = Flow { proc :: OnSends m -> OnRecvs m , ifs :: Maybe [Interface] }
type ComProc m = m (Flow m, Flow m)

compileCom :: (MonadIRBuilder m) => Component -> Reader Env (Either CompileError (ComProc m))
compileCom (Fix (CC.CompactClosedCartesianMorphismF c)) =
  case c of
    Mo.Cartesian (CC.DualityM (Mo.Braided f)) -> case f of

      Mo.Id x -> pure $ pure $
        return  ( Flow ( \(OnSends conts) -> OnRecvs conts ) Nothing
                , Flow ( \(OnSends conts) -> OnRecvs conts ) Nothing
                )
      
      Mo.Compose g h -> do
        g' <- compileCom g
        h' <- compileCom h
        pure $ do
          g'' <- g'
          h'' <- h'
          pure $ do
            (Flow procOG ifsOG , Flow procIG ifsIG) <- g''
            (Flow procOH ifsOH , Flow procIH ifsIH) <- h''
            -- return (Flow )
            return undefined
    
    Mo.Cartesian (CC.Ev x) -> splitDuality' x $ \outbound inbound ->
      do
        outboundNames <- blockAndVarNames outbound
        inboundNames <- blockAndVarNames inbound

        let namesInOrder = outboundNames ++ inboundNames

        return  ( Flow  ( \(OnSends []) -> OnRecvs $ fmap
                            ( \(Interface dst vars) -> \ops ->
                                do
                                  sequence $ fmap (\(var,op) -> return $ var LA.:= op) $ zip vars ops
                                  br dst
                                  return ()
                            )
                            namesInOrder
                        )
                        $ Just namesInOrder
                , Flow  (const $ OnRecvs []) $ Nothing
                )
    
    Mo.Cartesian (CC.Cv x) -> splitDuality' x $ \outbound inbound ->
      do
        outboundNames <- blockAndVarNames outbound
        inboundNames <- blockAndVarNames inbound

        let namesInOrder = inboundNames ++ outboundNames

        return  ( Flow  (const $ OnRecvs []) $ Nothing
                , Flow  ( \(OnSends []) -> OnRecvs $ fmap
                            ( \(Interface dst vars) -> \ops ->
                                do
                                  sequence $ fmap (\(var,op) -> return $ var LA.:= op) $ zip vars ops
                                  br dst
                                  return ()
                            )
                            namesInOrder
                        )
                        $ Just namesInOrder
                )
    
    {-
    Mo.Aug x -> do
      x' <- splitDuality x
      pure $ either (Left . ExpandTypeError) Right $ do
        (forward,backward) <- x'

        -- (length up) should be = (length backward)
        pure $ ComProc $ \(OnSends up []) -> OnRecvs (replicate (length forward) nop) []
    -}
  where
    -- nop = const $ label $ const $ return []

    names :: MonadIRBuilder m => Int -> m [LA.Name]
    names n = sequence $ replicate n fresh

    blockAndVarNames :: MonadIRBuilder m => [Type] -> m [Interface]
    blockAndVarNames flattenEventType =
      map ( \(blockName,varNames) -> Interface blockName varNames ) <$>
        ( zip <$> names (length flattenEventType) <*> (sequence $ fmap (names . lengthConstTypeAsLlvmType) flattenEventType) )

    splitDuality' x f = do
      x' <- splitDuality x

      pure $ either (Left . ExpandTypeError) Right $ do
        (outbound,inbound) <- x'
        pure $ f outbound inbound
    
    -- provisional for supporting multiple operands
    lengthConstTypeAsLlvmType t = 1

compilePrimCom :: (MonadIRBuilder m, MonadFix m) => M.QN -> [C.Value] -> Maybe (ComProc m)
compilePrimCom name args = Nothing


data TypeValue =
    TypeValue { typeCtor :: M.QN , typeParams :: [C.Value] }
  deriving (Eq, Show)

type Type = CC.CompactClosedCartesianObject TypeValue


liftExpandTypeErr :: Reader Env (Either ExpandTypeError a) -> Reader Env (Either CompileError a)
liftExpandTypeErr = fmap $ either (Left . ExpandTypeError) Right


splitDuality :: Type -> Reader Env (Either ExpandTypeError ([Type], [Type]))
splitDuality x =
  do
    x' <- expandType x
    flatten <- pure $ Mo.flattenOb <$> x'
    pure $ (partitionEithers . fmap toEither) <$> flatten
  where
    toEither x = case x of
      CC.D x -> Left $ Object $ CC.D x
      CC.Dual x -> Right x

expandType :: Type -> Reader Env (Either ExpandTypeError Type)
expandType x = case x of
  Mo.Unit -> pure $ Right Mo.Unit

  Mo.ProductO x y -> do
    x' <- expandType x
    y' <- expandType y
    return $ Mo.ProductO <$> x' <*> y'

  Mo.Object (CC.Dual x) -> expandType x

  Mo.Object (CC.D (TypeValue ctor params)) -> do
    env <- ask
    pure $ case Map.lookup ctor env of
      Just (DefTypeAlias t) -> Right $ t params
      Just (DefComponent _) -> Left UnexpectedComponentDefinition
      Nothing -> Left Undefined

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

data CompileError =
    DoCoFnError
  | ExpandTypeError ExpandTypeError
  | CompileCortexError CompileCortexError

data CompileCortexError