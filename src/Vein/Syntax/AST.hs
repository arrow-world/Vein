{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}

module Vein.Syntax.AST where

import qualified Vein.Syntax.Lexer as L
import Vein.Syntax.Lexer (Span(..))
import Vein.Core.Module as Module

import Numeric.Natural (Natural)
import Data.Fix (Fix(..))
import qualified Data.HashMap.Lazy as HashMap
import           Data.Either                    ( partitionEithers )
import           Control.Monad                  ( (>=>) )
import Data.Hashable

data Top e v = Top
  { definitions :: [(Definition e v , v)]
  , annotations :: [Annotation e v]
  }
  deriving (Eq,Show,Functor,Foldable,Traversable)


newtype ItemMap e v = ItemMap (HashMap.HashMap v (Item e v))
  deriving (Eq,Show)

instance Functor (ItemMap e) where
  fmap :: (Eq b , Hashable b) => (a -> b) -> ItemMap e a -> ItemMap e b


data Env e v = Env
  { envModule :: ItemMap e v
  , envAnnotations :: [Annotation e v]
  }
  deriving (Eq,Show,Functor,Foldable,Traversable)

emptyEnv = Env (ItemMap HashMap.empty) []

data Item e v =
    ItemDef (Definition e v)
  | ItemEnv (Env e v)
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Statement e v =
    Def v (Definition e v)
  | Ann (Annotation e v)
  deriving (Eq,Show,Functor,Foldable,Traversable)

statementsToEnv :: [Statement e Module.QN] -> Either (ParseError e Module.QN) (Env e Module.QN)
statementsToEnv [] = Right emptyEnv
statementsToEnv (s:ss) = do
  Env mod anns <- statementsToEnv ss
  case s of
    Def name def -> Env <$> HashMap.alterF f name mod <*> pure anns
      where
        f v = case (v,def) of
          (Nothing,_) -> Right $ Just $ ItemDef def
          (Just (ItemDef (DefConst cs)) , DefConst cs') -> Right $ Just $ ItemDef $ DefConst $ cs' ++ cs
          (Just i,_) -> Left $ MultipleDeclS name def i
    
    Ann ann -> return $ Env mod $ ann : anns

parsedStatementsToEnv :: [Either (ParseError e Module.QN) (Statement e Module.QN)] -> ParsedEnv e Module.QN
parsedStatementsToEnv ss = ParsedEnv $
    if null errs then
      either (Left . pure) Right $ statementsToEnv ss'
    else
      Left errs
  where (errs,ss') = partitionEithers ss

data Definition e v =
    DefData (Datatype e v)
  | DefTypeclass (Typeclass e v)
  | DefConst [(Located [Located (Param e)] , e)]
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Annotation e v =
    TypeAnnotation e e
  | DeclInstance (Instance e v)
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Prop e v =
    PropEq (Located v) (Located [Located (Param e)]) e
  | PropTypeAnnotation e e
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Clause e = Clause e e
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Datatype e v =
    GADT (Located [Located (Param e)]) (Located (ParsedEnv e v))
  | ADT (Located [Located (Param e)]) [Constructor e v]
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Typeclass e v = Typeclass (Located [Located (Param e)]) (Located (ParsedEnv e v))
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Instance e v = Instance v (Located [Located (Param e)]) (Located (ParsedEnv e v))
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Constructor e v = Constructor (Located v) (Located [Located (Param e)])
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Param e =
    Param e
  | ParamImplicit e
  deriving (Eq,Show,Functor,Foldable,Traversable)

data ExprF' r v =
    EApp r (Located [Located (Param r)])
  | EUnaryOpF UnaryOp r
  | EBinaryOpF (BinaryOp r) r r
  | ELiteralF Literal
  | ELetInF (Located (ParsedEnv r v)) r
  | EWhereF r (Located (ParsedEnv r v))
  | ECaseOfF r (Located [Located (Clause r)])
  | EMatchF (Located [Located (Clause r)])
  | EListF (Located [r])
  | ETupleF (Located [r])
  | ELamF r r
  | EArrowF [Located (Param r)] r
  | EDo (Located [Located (Stmt r)])
  | EHole
  | EPlaceholder
  | EVar v
  deriving (Eq,Show,Functor,Foldable,Traversable)

type ExprF r = ExprF' r Module.QN

data LocatedExprF r = LocatedExprF { leExprF :: ExprF r , leSpan :: Maybe Span }
  deriving (Eq,Show)

type LocatedExpr = Fix LocatedExprF

data Located a = Located { lSpan :: Maybe Span , unLocated :: a }
  deriving (Eq,Show,Functor,Foldable,Traversable)

data LocatedExprF' r v = LocatedExprF' { leExprF' :: ExprF' r v , leSpan' :: Maybe Span }
  deriving (Eq,Show,Functor,Foldable,Traversable)

type LocatedExpr' v = Fix (LocatedExprF' v)

data UnaryOp =
    Inverse
  deriving (Eq,Show)

data BinaryOp e =
    Arrow
  | App
  | AppImplicit
  | Plus
  | Minus
  | Times
  | Div
  | Typing
  | AppRight
  | Compose
  | ComposeRight
  | Infixated e
  deriving (Eq,Show,Functor)

data Literal =
    LNat L.Base Natural
  | LFP L.Base L.FloatingPoint
  | LStr String
  | LChar Char
  deriving (Eq,Show)

data Stmt e =
    Stmt e
  | StmtAssign e e
  deriving (Eq,Show,Functor)


data ParseError e v =
    MultipleDecl v (Prop e v) (Item e v)
  | MultipleDeclS v (Definition e v) (Item e v)
  | NotAllowedExpr e
  deriving (Eq,Show,Functor,Foldable,Traversable)


newtype ParsedEnv e v = ParsedEnv (Either [ParseError e v] (Env e v))
  deriving (Eq,Show)

instance Functor (ParsedEnv e) where
  fmap f (ParsedEnv x) = ParsedEnv $ case x of
    Left errs -> Left $ map (fmap f) errs
    Right env -> Right $ fmap f env

instance Foldable (ParsedEnv e) where
  foldMap f (ParsedEnv x) = either (foldMap (foldMap f)) (foldMap f) x

instance Traversable (ParsedEnv e) where
  traverse f (ParsedEnv x) = ParsedEnv <$> either (fmap Left . traverse (traverse f)) (fmap Right . traverse f) x

propsToEnv :: [Prop e Module.QN] -> Either (ParseError e Module.QN) (Env e Module.QN)
propsToEnv [] = Right emptyEnv
propsToEnv (p:ps) = do
  Env mod anns <- propsToEnv ps
  case p of
    PropEq (Located _ name) params e -> Env <$> HashMap.alterF f name mod <*> pure anns
      where
        f = \case
          Nothing -> Right $ Just $ ItemDef $ DefConst [(params,e)]
          Just (ItemDef (DefConst cs)) -> Right $ Just $ ItemDef $ DefConst $ (params,e) : cs
          Just i -> Left $ MultipleDecl name p i

    PropTypeAnnotation l r -> return $ Env mod $ TypeAnnotation l r : anns

parsedPropsToEnv :: [Located (Either (ParseError e Module.QN) (Prop e Module.QN))] -> ParsedEnv e Module.QN
parsedPropsToEnv ps = ParsedEnv $
    if null errs then
      either (Left . pure) Right $ propsToEnv ps'
    else
      Left errs
  where (errs,ps') = partitionEithers $ unLocated <$> ps