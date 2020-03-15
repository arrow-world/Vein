{-# LANGUAGE DeriveFunctor #-}

module Vein.Syntax.AST where

import qualified Vein.Syntax.Lexer as L
import Vein.Syntax.Lexer (Span(..))
import Vein.Core.Module as Module

import Numeric.Natural (Natural)
import Data.Fix (Fix(..))
import qualified Data.HashMap.Lazy as HashMap
import           Data.Either                    ( partitionEithers )

data Top e = Top
  { definitions :: [Module.QNamed (Definition e)]
  , annotations :: [Annotation e]
  }
  deriving (Eq,Show,Functor)

data Env e = Env
  { envModule :: Module.ModuleMap (Item e)
  , envAnnotations :: [Annotation e]
  }
  deriving (Eq,Show,Functor)

emptyEnv = Env HashMap.empty []

data Item e =
    ItemDef (Definition e)
  | ItemEnv (Env e)
  deriving (Eq,Show,Functor)

data Statement e =
    Def (Module.QNamed (Definition e))
  | Ann (Annotation e)
  deriving (Eq,Show,Functor)

statementsToEnv :: [Statement e] -> Either (ParseError e) (Env e)
statementsToEnv [] = Right emptyEnv
statementsToEnv (s:ss) = do
  Env mod anns <- statementsToEnv ss
  case s of
    Def (Module.QNamed name def) -> Env <$> HashMap.alterF f name mod <*> pure anns
      where
        f v = case (v,def) of
          (Nothing,_) -> Right $ Just $ ItemDef def
          (Just (ItemDef (DefConst cs)) , DefConst cs') -> Right $ Just $ ItemDef $ DefConst $ cs' ++ cs
          (Just i,_) -> Left $ MultipleDeclS name def i
    
    Ann ann -> return $ Env mod $ ann : anns

parsedStatementsToEnv :: [Either (ParseError e) (Statement e)] -> ParsedEnv e
parsedStatementsToEnv ss = ParsedEnv $
    if null errs then
      either (Left . pure) Right $ statementsToEnv ss'
    else
      Left errs
  where (errs,ss') = partitionEithers ss

data Definition e =
    DefData (Datatype e)
  | DefTypeclass (Typeclass e)
  | DefConst [(Located [Located (Param e)] , e)]
  deriving (Eq,Show,Functor)

data Annotation e =
    TypeAnnotation e e
  | DeclInstance (Instance e)
  deriving (Eq,Show,Functor)

data Prop e =
    PropEq (Located Module.QN) (Located [Located (Param e)]) e
  | PropTypeAnnotation e e
  deriving (Eq,Show,Functor)

data Clause e = Clause e e
  deriving (Eq,Show,Functor)

data Datatype e =
    GADT (Located [Located (Param e)]) (Located (ParsedEnv e))
  | ADT (Located [Located (Param e)]) [Constructor e]
  deriving (Eq,Show,Functor)

data Typeclass e = Typeclass (Located [Located (Param e)]) (Located (ParsedEnv e))
  deriving (Eq,Show,Functor)

data Instance e = Instance Module.QN (Located [Located (Param e)]) (Located (ParsedEnv e))
  deriving (Eq,Show,Functor)

data Constructor e = Constructor (Located Module.QN) (Located [Located (Param e)])
  deriving (Eq,Show,Functor)

data Param e =
    Param e
  | ParamImplicit e
  deriving (Eq,Show,Functor)

data ExprF' r v =
    EApp r (Located [Located (Param r)])
  | EUnaryOpF UnaryOp r
  | EBinaryOpF (BinaryOp r) r r
  | ELiteralF Literal
  | ELetInF (Located (ParsedEnv r)) r
  | EWhereF r (Located (ParsedEnv r))
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
  deriving (Eq,Show,Functor)

type ExprF r = ExprF' r Module.QN

data LocatedExprF r = LocatedExprF { leExprF :: ExprF r , leSpan :: Maybe Span }
  deriving (Eq,Show)

type LocatedExpr = Fix LocatedExprF

data Located a = Located { lSpan :: Maybe Span , unLocated :: a }
  deriving (Eq,Show,Functor)

data UnaryOp =
    Inverse
  deriving (Eq,Show)

data BinaryOp e =
    Arrow
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


data ParseError e =
    MultipleDecl Module.QN (Prop e) (Item e)
  | MultipleDeclS Module.QN (Definition e) (Item e)
  | NotAllowedExpr e
  deriving (Eq,Show,Functor)


newtype ParsedEnv e = ParsedEnv (Either [ParseError e] (Env e))
  deriving (Eq,Show)

instance Functor ParsedEnv where
  fmap f (ParsedEnv x) = ParsedEnv $ case x of
    Left errs -> Left $ map (fmap f) errs
    Right env -> Right $ fmap f env

propsToEnv :: [Prop e] -> Either (ParseError e) (Env e)
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

parsedPropsToEnv :: [Located (Either (ParseError e) (Prop e))] -> ParsedEnv e
parsedPropsToEnv ps = ParsedEnv $
    if null errs then
      either (Left . pure) Right $ propsToEnv ps'
    else
      Left errs
  where (errs,ps') = partitionEithers $ unLocated <$> ps