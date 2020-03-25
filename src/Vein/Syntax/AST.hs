{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Vein.Syntax.AST where

import qualified Vein.Syntax.Lexer as L
import Vein.Syntax.Lexer (Span(..))
import qualified Vein.Core.Module as Module

import Numeric.Natural (Natural)
import Data.Fix (Fix(..))
import qualified Data.HashMap.Lazy as HashMap
import           Data.Either                    ( partitionEithers )
import Data.Bifunctor (Bifunctor , bimap)
import Data.Bifoldable (Bifoldable , bifoldMap)
import Data.Bitraversable (Bitraversable , bitraverse)
import Data.Bifunctor.TH
import Data.Hashable (Hashable)
import           Data.Maybe                     ( mapMaybe
                                                , catMaybes
                                                )
import           Data.Bool                      ( bool )
import           Control.Applicative            ( liftA2 )

data Top e = Top
  { definitions :: [Module.QNamed (Definition Module.QN e)]
  , annotations :: [Annotation Module.QN e]
  }
  deriving (Eq,Show,Functor)


data ModuleMap v e =
    ModuleMap (HashMap.HashMap v (Item v e))
  | ModuleMapL [(v , Item v e)]
  deriving (Eq,Show,Functor,Foldable,Traversable)

instance Bifunctor ModuleMap where
  bimap f g = mapModuleMap $ ModuleMapL . ( map $ bimap f (bimap f g) )

instance Bifoldable ModuleMap where
  bifoldMap f g = mapModuleMap $ foldMap $ bifoldMap f (bifoldMap f g)

instance Bitraversable ModuleMap where
  bitraverse f g = fmap ModuleMapL . ( mapModuleMap $ traverse $ bitraverse f (bitraverse f g) )

mapModuleMap :: ( [(a , Item a b)] -> c ) -> ModuleMap a b -> c
mapModuleMap f = \case
    ModuleMap m -> f $ HashMap.toList m
    ModuleMapL items -> f items


alterF :: (Applicative m , Eq v , Hashable v) => (Maybe (Item v e) -> m (Maybe (Item v e))) -> v -> ModuleMap v e -> m (ModuleMap v e)
alterF f name (ModuleMap m) = ModuleMap <$> HashMap.alterF f name m
alterF f name (ModuleMapL items) =
    ModuleMapL . catMaybes <$>
      if any (\(k,v) -> k == name) items then items' else liftA2 (:) (fmap (name,) <$> f Nothing) items'
  where items' = traverse (\(k,v) -> fmap (k,) <$> f' k v) items
        f' k = (bool pure f $ k == name) . Just


data Env v e = Env
  { envModule :: ModuleMap v e
  , envAnnotations :: [Annotation v e]
  }
  deriving (Eq,Show,Functor,Foldable,Traversable)

emptyEnv = Env (ModuleMap HashMap.empty) []

data Item v e =
    ItemDef (Definition v e)
  | ItemEnv (Env v e)
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Statement v e =
    Def v (Definition v e)
  | Ann (Annotation v e)
  deriving (Eq,Show,Functor)

statementsToEnv :: (Eq v , Hashable v) => [Statement v e] -> Either (ParseError v e) (Env v e)
statementsToEnv [] = Right emptyEnv
statementsToEnv (s:ss) = do
  Env mod anns <- statementsToEnv ss
  case s of
    Def name def -> Env <$> alterF f name mod <*> pure anns
      where
        f v = case (v,def) of
          (Nothing,_) -> Right $ Just $ ItemDef def
          (Just (ItemDef (DefConst cs)) , DefConst cs') -> Right $ Just $ ItemDef $ DefConst $ cs' ++ cs
          (Just i,_) -> Left $ MultipleDeclS name def i
    
    Ann ann -> return $ Env mod $ ann : anns

parsedStatementsToEnv :: (Eq v , Hashable v) => [Either (ParseError v e) (Statement v e)] -> ParsedEnv v e
parsedStatementsToEnv ss = ParsedEnv $
    if null errs then
      either (Left . pure) Right $ statementsToEnv ss'
    else
      Left errs
  where (errs,ss') = partitionEithers ss

data Definition v e =
    DefData (Datatype v e)
  | DefTypeclass (Typeclass v e)
  | DefConst [(Located [Located (Param e)] , e)]
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Annotation v e =
    TypeAnnotation e e
  | DeclInstance (Instance v e)
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Prop v e =
    PropEq (Located v) (Located [Located (Param e)]) e
  | PropTypeAnnotation e e
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Clause e = Clause e e
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Datatype v e =
    GADT (Located [Located (Param e)]) (Located (ParsedEnv v e))
  | ADT (Located [Located (Param e)]) [Constructor v e]
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Typeclass v e = Typeclass (Located [Located (Param e)]) (Located (ParsedEnv v e))
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Instance v e = Instance v (Located [Located (Param e)]) (Located (ParsedEnv v e))
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Constructor v e = Constructor (Located v) (Located [Located (Param e)])
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Param e =
    Param e
  | ParamImplicit e
  deriving (Eq,Show,Functor,Foldable,Traversable)

data ExprF' v r =
    EApp r (Located [Located (Param r)])
  | EUnaryOpF (Located UnaryOp) r
  | EBinaryOpF (Located (BinaryOp r)) r r
  | ELiteralF Literal
  | ELetInF (Located (ParsedEnv Module.QN r)) r
  | EWhereF r (Located (ParsedEnv Module.QN r))
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

type ExprF r = ExprF' Module.QN r

data LocatedExprF r = LocatedExprF { leExprF :: ExprF r , leSpan :: Maybe Span }
  deriving (Eq,Show)

type LocatedExpr = Fix LocatedExprF

data Located a = Located { lSpan :: Maybe Span , unLocated :: a }
  deriving (Eq,Show,Functor,Foldable,Traversable)

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
  deriving (Eq,Show,Functor,Foldable,Traversable)

data Literal =
    LNat L.Base Natural
  | LFP L.Base L.FloatingPoint
  | LStr String
  | LChar Char
  deriving (Eq,Show)

data Stmt e =
    Stmt e
  | StmtAssign e e
  deriving (Eq,Show,Functor,Foldable,Traversable)


data ParseError v e =
    MultipleDecl v (Prop v e) (Item v e)
  | MultipleDeclS v (Definition v e) (Item v e)
  | NotAllowedExpr e
  deriving (Eq,Show,Functor,Foldable,Traversable)


newtype ParsedEnv v e = ParsedEnv (Either [ParseError v e] (Env v e))
  deriving (Eq,Show)

instance Functor (ParsedEnv v) where
  fmap f (ParsedEnv x) = ParsedEnv $ case x of
    Left errs -> Left $ map (fmap f) errs
    Right env -> Right $ fmap f env

instance Foldable (ParsedEnv v) where
  foldMap f (ParsedEnv x) = either
    (foldMap (foldMap f)) 
    (foldMap f) 
    x

instance Traversable (ParsedEnv v) where
  traverse f (ParsedEnv x) = ParsedEnv <$> either
    (fmap Left . traverse (traverse f))
    (fmap Right . traverse f)
    x
    
propsToEnv :: (Eq v , Hashable v) => [Prop v e] -> Either (ParseError v e) (Env v e)
propsToEnv [] = Right emptyEnv
propsToEnv (p:ps) = do
  Env mod anns <- propsToEnv ps
  case p of
    PropEq (Located _ name) params e -> Env <$> alterF f name mod <*> pure anns
      where
        f = \case
          Nothing -> Right $ Just $ ItemDef $ DefConst [(params,e)]
          Just (ItemDef (DefConst cs)) -> Right $ Just $ ItemDef $ DefConst $ (params,e) : cs
          Just i -> Left $ MultipleDecl name p i

    PropTypeAnnotation l r -> return $ Env mod $ TypeAnnotation l r : anns

parsedPropsToEnv :: (Eq v , Hashable v) => [Located (Either (ParseError v e) (Prop v e))] -> ParsedEnv v e
parsedPropsToEnv ps = ParsedEnv $
    if null errs then
      either (Left . pure) Right $ propsToEnv ps'
    else
      Left errs
  where (errs,ps') = partitionEithers $ unLocated <$> ps

uopToName :: UnaryOp -> Module.QN
uopToName = \case
  Inverse -> toQN [modop,"inv"]

bopToName :: BinaryOp LocatedExpr -> Maybe Module.QN
bopToName = unwrapQN' . bopToExpr

bopToExpr' :: BinaryOp e -> Either e Module.QN
bopToExpr' = \case
    Plus -> Right $ toQN [modop,"plus"]
    Minus -> Right $ toQN [modop,"minus"]
    Infixated e -> Left e

bopToExpr :: BinaryOp LocatedExpr -> ExprF LocatedExpr
bopToExpr = either (leExprF . unFix) EVar . bopToExpr'

modop = "operator"
toQN = Module.QN . map Module.Name

unwrapQN' :: ExprF LocatedExpr -> Maybe Module.QN
unwrapQN' = \case
  EVar name -> Just $ name
  _ -> Nothing

unwrapQN :: LocatedExpr -> Maybe (Located Module.QN)
unwrapQN (Fix (LocatedExprF e l)) = Located l <$> unwrapQN' e

$(deriveBifunctor ''ExprF')
$(deriveBifunctor ''Env)
$(deriveBifunctor ''Prop)
$(deriveBifunctor ''ParsedEnv)
$(deriveBifunctor ''ParseError)
$(deriveBifunctor ''Item)
$(deriveBifunctor ''Definition)
$(deriveBifunctor ''Datatype)
$(deriveBifunctor ''Constructor)
$(deriveBifunctor ''Typeclass)
$(deriveBifunctor ''Annotation)
$(deriveBifunctor ''Instance)

$(deriveBifoldable ''ExprF')
$(deriveBifoldable ''Env)
$(deriveBifoldable ''Prop)
$(deriveBifoldable ''ParsedEnv)
$(deriveBifoldable ''ParseError)
$(deriveBifoldable ''Item)
$(deriveBifoldable ''Definition)
$(deriveBifoldable ''Datatype)
$(deriveBifoldable ''Constructor)
$(deriveBifoldable ''Typeclass)
$(deriveBifoldable ''Annotation)
$(deriveBifoldable ''Instance)

$(deriveBitraversable ''ExprF')
$(deriveBitraversable ''Env)
$(deriveBitraversable ''Prop)
$(deriveBitraversable ''ParsedEnv)
$(deriveBitraversable ''ParseError)
$(deriveBitraversable ''Item)
$(deriveBitraversable ''Definition)
$(deriveBitraversable ''Datatype)
$(deriveBitraversable ''Constructor)
$(deriveBitraversable ''Typeclass)
$(deriveBitraversable ''Annotation)
$(deriveBitraversable ''Instance)