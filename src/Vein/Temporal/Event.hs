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
import Data.Fix ( Fix (..), cata )
import Data.Either ( partitionEithers )
import Numeric.Natural ( Natural )
import Control.Arrow (arr, Kleisli, (>>>), (&&&), (+++), (***))
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

data ComponentNumberedF r = ComponentNumberedF { cnComponent :: ComponentF r , cnId :: Maybe Natural }
  deriving (Eq, Show, Functor, Foldable, Traversable)
type ComponentNumbered = Fix ComponentNumberedF

data Definition =
    DefComponent ([C.Value] -> Component)
  | DefTypeAlias ([C.Value] -> Type)

type Env = M.ModuleMap Definition


-- compileCom :: MonadIRBuilder m => Component -> Reader Env (Either ScanError (PreCode m LA.Name [Operand]))
-- compileCom = undefined

type EndoKleisli m a = a -> m a
type EKTerminator m a = a -> m ()

{-
composeEKSeq :: EKSeq m a -> EndoKleisli m a
composeEKSeq (x:xs) = x >>= composeEKSeq xs

data JumpableCodeBuilder mBuilder mCode a = JumpableCodeBuilder
  { jump :: EndoKleisli mCode a -> mCode a -> mBuilder (EndoKleisli mCode a , mCode ()) }

llvmJumpableCodeBuilder :: (MonadIRBuilder m, MonadIRBuilder m') => JumpableCodeBuilder m m' [Operand]
llvmJumpableCodeBuilder = JumpbleCodeBuilder
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
    deriving (Eq, Show, Ord)

inputPortNo :: InputPort -> Natural
inputPortNo (LeftInputPort n) = n
inputPortNo (RightInputPort n) = n

data OutputPort =
    RightOutputPort Natural
  | LeftOutputPort Natural
    deriving (Eq, Show, Ord)

outputPortNo :: OutputPort -> Natural
outputPortNo (LeftOutputPort n) = n
outputPortNo (RightOutputPort n) = n


data JunctionPoint = JunctionPoint { jpMorphismId :: Natural , jpLeftPort :: Natural }
  deriving (Eq, Show)

data EKSeq m a =
    EksSnoc (EKSeq m a) (EndoKleisli m a)
  | EksNil

data PreCodeBranch m a =
    PcbSnocMerge { pcbSmInit :: PreCodeBranch m a , pcbSmJp :: JunctionPoint , pcbSmEks :: EKSeq m a }
  | PcbForkStart { pcbStem :: PreCodeBranch m a , pcbJp :: JunctionPoint , pcbEks :: EKSeq m a }
  | PcbStart { pcbStartEks :: EKSeq m a }

appendPreCodeBranch :: EndoKleisli m a -> PreCodeBranch m a -> PreCodeBranch m a
appendPreCodeBranch ek pcb = case pcb of
  PcbSnocMerge init jp eks -> PcbSnocMerge init jp (EksSnoc eks ek)
  PcbForkStart stem jp eks -> PcbForkStart stem jp (EksSnoc eks ek)
  PcbStart eks -> PcbStart (EksSnoc eks ek)

hasMerge :: JunctionPoint -> PreCodeBranch m a -> Bool
hasMerge jp pcb = case pcb of
  PcbSnocMerge pcb' jp' _ -> jp == jp' || hasMerge jp pcb'
  PcbForkStart stem _ _ -> hasMerge jp stem
  PcbStart _ -> False

data PreCodeTerminalBranch m a =
    PctbEnd (PreCodeBranch m a)
  | PctbFork { pctbStem :: PreCodeBranch m a , pctbJp :: JunctionPoint }
  | PctbMerge (PreCodeBranch m a) JunctionPoint

data PreCode m a =
  PreCode { pcOutput :: PreCodeBranch m a }

data PreCodes m a =
  PreCodes  { pcsOutputs :: [(OutputPort , PreCodeBranch m a)]
            , pcsTerminals :: [PreCodeTerminalBranch m a]
            }

instance Semigroup (PreCodes m a) where
  x <> y = PreCodes (pcsOutputs x ++ pcsOutputs y) (pcsTerminals x ++ pcsTerminals y)

instance Monoid (PreCodes m a) where
  mempty = PreCodes { pcsOutputs = [] , pcsTerminals = [] }


type Visitor m a = Value -> InputPort -> ReaderT Env (Either ScanError) [(OutputPort , EndoKleisli m a)]
type Output m a = (OutputPort , a -> m a)
type Terminal m a = (a -> m () , OutputPort , m a)
type OutputNoDirection m a = (Natural , a -> m a)

buildPreCode :: Visitor m a -> PreCode m a -> ComponentNumbered -> InputPort -> ReaderT Env (Either ScanError) (PreCodes m a)
buildPreCode visitor initCode (Fix c) port =
  let ComponentNumberedF (CC.CompactClosedCartesianMorphismF c') morphismId = c in
    case c' of

      Mo.Cartesian (CC.DualityM (Mo.Braided f)) -> case f of

        Mo.Id x -> id' x
        
        Mo.Compose g h -> case port of
          LeftInputPort _ -> do
            (lOsG , rOsG , tsG) <- buildPreCode'' g port
            (lOsH , rOsH , tsH) <- buildPreCodes h LeftInputPort rOsG
            (lOsI , rOsI , tsI) <- buildInnerInteraction lOsH
            return $ PreCodes ( (map leftOutput (lOsG ++ lOsH ++ lOsI)) ++ (map rightOutput (rOsG ++ rOsH ++ rOsI)) ) (tsG ++ tsH ++ tsI)

          RightInputPort _ -> do
            (lOsH , rOsH , tsH) <- buildPreCode'' h port
            (lOsG , rOsG , tsG) <- buildPreCodes g RightInputPort lOsH
            (lOsI , rOsI , tsI) <- buildInnerInteraction rOsG
            return $ PreCodes ( (map leftOutput (lOsH ++ lOsG ++ lOsI)) ++ (map rightOutput (rOsH ++ rOsG ++ rOsI)) ) (tsH ++ tsG ++ tsI)

          where
            buildPreCode'' component port' = do
              PreCodes os ts <- buildPreCode' initCode component port'
              let (lOs , rOs) = partitionOutputs os
              return (lOs , rOs , ts)

            buildPreCodes component director inputs = do
              codes <- sequence $ map (\(n,pcb) -> buildPreCode' (PreCode pcb) component $ director n) inputs
              let ts = concat $ map pcsTerminals codes
              let (lOs , rOs) = partitionOutputs $ concat $ map pcsOutputs codes
              return (lOs , rOs , ts)

            buildInnerInteraction [] = pure $ ([],[],[])
            buildInnerInteraction lOsH = do
              (lOsG , rOsG , tsG) <- buildPreCodes g RightInputPort lOsH
              (lOsH , rOsH , tsH) <- buildPreCodes h LeftInputPort rOsG
              (lOsI , rOsI , tsI) <- buildInnerInteraction lOsH
              return ( (lOsG ++ lOsI) , (rOsH ++ rOsI) , (tsG ++ tsH ++ tsI) )

            leftOutput (op,code) = (LeftOutputPort op , code)

            rightOutput (op,code) = (RightOutputPort op , code)

            partitionOutputs =
              partitionEithers .
                map
                  ( \(op,pcb) ->  case op of
                                    LeftOutputPort n -> Left (n , pcb)
                                    RightOutputPort n -> Right (n , pcb)
                  )

        Mo.ProductM g h -> case port of
          LeftInputPort n -> do
            (outboundG,_) <- dom' g >>= splitDuality'
            (outboundH,_) <- dom' h >>= splitDuality'

            case (fromIntegral $ length outboundG , fromIntegral $ length outboundH) of
              (m,m') | n < m -> buildPreCode' initCode g $ LeftInputPort n
              (m,m') | m <= n && n < m+m' -> buildPreCode' initCode h $ LeftInputPort $ n - m
              _ -> throwError $ InvalidInputPort port

          RightInputPort n -> do
            (_,inboundG) <- cod' g >>= splitDuality'
            (_,inboundH) <- cod' h >>= splitDuality'

            case (fromIntegral $ length inboundG , fromIntegral $ length inboundH) of
              (m,m') | n < m -> buildPreCode' initCode g $ RightInputPort n
              (m,m') | m <= n && n < m+m' -> buildPreCode' initCode h $ RightInputPort $ n - m
              _ -> throwError $ InvalidInputPort port
          
        Mo.UnitorL x -> id' x
        Mo.UnitorR x -> id' x
        Mo.UnunitorL x -> id' x
        Mo.UnunitorR x -> id' x
        Mo.Assoc x y z -> id' ((x >< y) >< z)
        Mo.Unassoc x y z -> id' ((x >< y) >< z)
        Mo.Morphism val -> do
          os <- visitor val port
          return $ PreCodes (map (fmap $ flip appendPreCodeBranch $ pcOutput initCode) os) []
      
      Mo.Cartesian (DualityM (Mo.Braid x y)) -> do
        (outboundX,inboundX) <- splitDuality' x
        (outboundY,inboundY) <- splitDuality' y

        case port of
          LeftInputPort n -> case (fromIntegral $ length outboundX , fromIntegral $ length outboundY) of
            (m,m') | n < m'             -> return $ outputFrom $ RightOutputPort $ m + n
            (m,m') | m <= n && n < m+m' -> return $ outputFrom $ RightOutputPort $ n - m
            _                           -> throwError $ InvalidInputPort port

          RightInputPort n -> case (fromIntegral $ length inboundX , fromIntegral $ length inboundY) of
            (m,m') | n < m'             -> return $ outputFrom $ LeftOutputPort $ m + n
            (m,m') | m <= n && n < m+m' -> return $ outputFrom $ LeftOutputPort $ n - m
            _                           -> throwError $ InvalidInputPort port
      
      Mo.Cartesian (Ev x) -> do
        (outbound,inbound) <- splitDuality' x

        let lo = fromIntegral $ length outbound
        let li = fromIntegral $ length inbound

        case port of
          LeftInputPort n | n < lo                -> return $ outputFrom $ LeftOutputPort $ li + n
          LeftInputPort n | lo <= n && n < lo+li  -> return $ outputFrom $ LeftOutputPort $ n - lo
          _                                       -> throwError $ InvalidInputPort port
      
      Mo.Cartesian (Cv x) -> do
        (outbound,inbound) <- splitDuality' x

        let li = fromIntegral $ length inbound
        let lo = fromIntegral $ length outbound

        case port of
          RightInputPort n | n < li               -> return $ outputFrom $ RightOutputPort $ lo + n
          RightInputPort n | li <= n && n < li+lo -> return $ outputFrom $ RightOutputPort $ n - li
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
            return $ PreCodes [ ( RightOutputPort n        , PcbForkStart (pcOutput initCode) jp EksNil )
                              , ( RightOutputPort $ lo + n , PcbForkStart (pcOutput initCode) jp EksNil )
                              ]
                              [PctbFork (pcOutput initCode) jp]
            where
              jp = jp' n

          RightInputPort n | fromIntegral n < li*2 ->
              if hasMerge jp (pcOutput initCode) then
                return $ PreCodes [(op , PcbSnocMerge (pcOutput initCode) jp EksNil)] []
              else
                return $ PreCodes [] [PctbMerge (pcOutput initCode) jp]
            where
              op = LeftOutputPort n'
              jp = jp' n'
              n' = n `mod` li
          
          _ -> throwError $ InvalidInputPort port
        where
          jp' = JunctionPoint morphismId'
            where Just morphismId' = morphismId
      
      Mo.Aug x -> do
        (outbound,_) <- splitDuality' x
        case port of
          LeftInputPort n | n < (fromIntegral $ length outbound)  ->
            return $ PreCodes [] [PctbEnd $ pcOutput initCode]
          _                                                       -> throwError $ InvalidInputPort port

  where
    id' x = do
      (outbound,inbound) <- splitDuality' x
      case port of
        LeftInputPort n | fromIntegral n < length outbound -> pure $ outputFrom $ RightOutputPort n
        RightInputPort n | fromIntegral n < length inbound -> pure $ outputFrom $ LeftOutputPort n
        _ -> throwError $ InvalidInputPort port
    
    outputFrom op = PreCodes [(op , pcOutput initCode)] []

    buildPreCode' = buildPreCode visitor

    splitDuality' = liftExpandTypeErr . splitDuality
    flattenType' = liftExpandTypeErr . flattenType

    liftExpandTypeErr = mapReaderT $ either (Left . ExpandTypeError) Right

    doco' = liftDoCoErr . doco
    dom' = liftDoCoErr . dom
    cod' = liftDoCoErr . cod

    liftDoCoErr = maybe (throwError DoCoError) return 


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
        return $ Fix $ ComponentNumberedF c' (Just n)
      
      _ -> do
        c' <- traverse numbering c
        return $ Fix $ ComponentNumberedF c' Nothing




data TypeValue =
    TypeValue { typeCtor :: M.QN , typeParams :: [C.Value] }
  deriving (Eq, Show)

data Value =
    Val { valCtor :: M.QN , valParams :: [C.Value] }
  deriving (Eq, Show)

type Type = CC.CompactClosedCartesianObject TypeValue

doco = cata $ CC.docoCompactClosedCartesianMorphismF docoVal id . cnComponent
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