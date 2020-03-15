{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vein.Core.Lambda.TypeCheck where

import qualified Vein.Core.Lambda.Expr as E
import qualified Vein.Core.Module as M
import Vein.Util.Counter (Counter(..), count, runCounter)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State ( StateT , put , get , gets , modify , runStateT )
import           Control.Monad.Trans.Reader     ( ReaderT
                                                , ask
                                                , asks
                                                , runReaderT
                                                , local
                                                )
import           Control.Monad.Except           ( MonadError , throwError )
import           Control.Monad                  ( (>=>) )
import           Control.Applicative            ( liftA2 )
import Numeric.Natural ( Natural )
import Data.Fix (Fix(..))
import qualified Data.Map.Lazy as Map
import qualified Data.Default as Default
import Data.Default (Default)

import Prelude hiding (pi)

newtype MetaVar = MetaVar Natural
  deriving (Eq,Show,Ord)

succMetaVar (MetaVar n) = MetaVar $ n + 1

type Env a r = [ExprFWith a r]

data Ctx a r = Ctx
  { nextMetaVar :: MetaVar
  , assignments :: Map.Map MetaVar (ExprFWith a r)
  }
  deriving (Eq,Show)

newtype TypeCheckMonad a' r a = TypeCheckMonad (ReaderT (Env a' r) (StateT (Ctx a' r) (Either (Error r))) a)
  deriving (Functor , Applicative , Monad , MonadError (Error r))


genMetaVar :: TypeCheckMonad a r MetaVar
genMetaVar = TypeCheckMonad $ do
  ctx <- lift get
  let metaVar = nextMetaVar ctx
  lift $ put $ ctx { nextMetaVar = succMetaVar metaVar }
  return metaVar

metaVar :: Default a => TypeCheckMonad a r (ExprFWith a r)
metaVar = expr . EMetaVar <$> genMetaVar


assign :: MetaVar -> ExprFWith a r -> TypeCheckMonad a r (ExprFWith a r)
assign v e = TypeCheckMonad $ do
  ctx <- lift $ get
  lift $ put $ ctx { assignments = Map.insert v e $ assignments ctx }
  return e


getEnv :: TypeCheckMonad a r (Env a r)
getEnv = TypeCheckMonad $ ask

lookupEnv :: Natural -> TypeCheckMonad a r (ExprFWith a r)
lookupEnv n = TypeCheckMonad $ asks (!! fromIntegral n)

localEnv :: (Env a' r -> Env a' r) -> TypeCheckMonad a' r a -> TypeCheckMonad a' r a
localEnv f (TypeCheckMonad r) = TypeCheckMonad $ local f r

appendEnv e = localEnv (e:)


runTypeCheckMonad :: TypeCheckMonad a' r a -> Ctx a' r -> Either (Error r) (a , Ctx a' r)
runTypeCheckMonad (TypeCheckMonad r) = runStateT (runReaderT r [])


data ExprF r =
    EMetaVar MetaVar
  | EExpr (E.ExprF r)
  deriving (Eq,Show,Functor,Foldable,Traversable)

data ExprFWith a r = ExprFWith { eAnn :: a , unExpr :: ExprF r }
  deriving (Eq,Show,Functor,Foldable,Traversable)

expr :: Default a => ExprF r -> ExprFWith a r
expr = ExprFWith Default.def

mapExpr f (ExprFWith a e) = ExprFWith a $ f e


type Expr a = Fix (ExprFWith a)


data TypedExprF a r = TypedExprF { tefType :: ExprFWith a r , tefTerm :: ExprFWith a r }
  deriving (Functor,Foldable,Traversable)

type TypedExpr a = Fix (TypedExprF a)

t `typed` ty = TypedExprF ty t
typeof = tefType
termof = tefTerm
mapTerm f (TypedExprF ty e) = TypedExprF ty $ f e

check :: Default a => TypedExpr a -> TypeCheckMonad a (TypedExpr a) ()
check e = do
  ty <- infer $ termof $ unFix e
  unify ty (typeof $ unFix e)
  return ()

unifyTyped :: Default a => TypedExpr a -> TypedExpr a -> TypeCheckMonad a (TypedExpr a) (TypedExpr a)
unifyTyped e1 e2 = do
  e <- unify (termof $ unFix e1) (termof $ unFix e2)
  t <- unify (typeof $ unFix e1) (typeof $ unFix e2)
  return $ Fix $ e `typed` t


reconstruct :: Default a => ExprFWith a (TypedExpr a) -> TypeCheckMonad a (TypedExpr a) (TypedExpr a)
reconstruct e = Fix . TypedExprF e <$> infer e

infer :: Default a => ExprFWith a (TypedExpr a) -> TypeCheckMonad a (TypedExpr a) (ExprFWith a (TypedExpr a))
infer (ExprFWith ann e) = case e of
  EExpr e -> case e of
    E.Var n -> lookupEnv n

    E.Lam a@(E.Abs t'@(Fix t) (Fix e)) -> do
      let typeofArg = t
      typeofE <- appendEnv (termof typeofArg) $ infer $ termof e

      unify (typeof typeofArg) univ

      return $ pi t' $ Fix $ typeofE `typed` univ

    E.App (Fix e1) e2'@(Fix e2) -> do
      let typeofArg = typeof $ e2
      typeofVal <- metaVar
      let fnTy = pi (Fix $ typeofArg `typed` univ) (Fix $ (typeofVal `typed` univ))

      unify (typeof $ e1) fnTy

      return $ subst (E.dotNil e2') (Fix $ typeofVal `typed` univ)
    
    E.Const c -> resolveType c

    E.Univ -> return univ

    E.Typing (Fix e) (Fix t) -> do
      unify (typeof t) univ
      unify (typeof e) $ termof t
    
    E.Pi (E.Abs (Fix t) (Fix e)) -> do
      unify (typeof t) univ
      unify (typeof e) univ

      return univ
    
    E.Subst s e -> typeof . unFix <$> execSubst s e

  EMetaVar v -> metaVar


unify :: Default a  => ExprFWith a (TypedExpr a) -> ExprFWith a (TypedExpr a)
                    -> TypeCheckMonad a (TypedExpr a) (ExprFWith a (TypedExpr a))
unify e1 e2 = case (unExpr e1 , unExpr e2) of
  (EMetaVar v1 , EMetaVar v2) -> assign (max v1 v2) $ expr $ EMetaVar (min v1 v2)
  
  (EMetaVar v , EExpr e) -> assign v e2
  (EExpr e , EMetaVar v) -> assign v e1

  (EExpr e1 , EExpr e2) -> case (e1,e2) of
    (E.Univ , E.Univ) -> return univ
    -- _ -> throwError $ UnifyError e1 e2


{-

toExprF :: Maybe (E.ExprF r) -> TypeCheckMonad r (ExprF r)
toExprF = maybe (EMetaVar <$> genMetaVar) $ return . EExpr

unwrapExprF :: Maybe (ExprF r) -> TypeCheckMonad r (ExprF r)
unwrapExprF = maybe metaVar return

unwrapTypedExprF :: Maybe (TypedExprF f r) -> TypeCheckMonad r (TypedExprF f r)
unwrapTypedExprF = maybe (TypedExprF <$> metaVar <*> metaVar) return

-}

var :: Default a => Natural -> ExprFWith a r
var k = expr $ EExpr $ E.Var k

univ :: Default a => ExprFWith a r
univ = expr $ EExpr E.Univ

pi :: Default a => r -> r -> ExprFWith a r
pi t e = expr $ EExpr $ E.Pi $ E.Abs t e

subst :: Default a => E.Subst r -> r -> ExprFWith a r
subst s e = expr $ EExpr $ E.Subst s e

resolveType :: E.Const -> TypeCheckMonad a r (ExprFWith a r)
resolveType c = undefined

execSubstTyped :: Default a => E.Subst (TypedExpr a) -> TypedExpr a
                            -> TypeCheckMonad a (TypedExpr a) (TypedExpr a)
execSubstTyped s (Fix (TypedExprF t e)) = Fix $ TypedExprF (execSubst s t) (execSubst s e)

execSubst :: Default a  => E.Subst (TypedExpr a) -> ExprFWith a (TypedExpr a)
                        -> TypeCheckMonad a (TypedExpr a) (TypedExpr a)
execSubst s e = case (s , unExpr e) of
    (E.Shift m , EExpr (E.Var k)) -> mapVar $ k + m
    (E.Dot e' _ , EExpr (E.Var 0)) -> return e'
    (E.Dot e' s' , EExpr (E.Var k)) -> mapVar $ k - 1
    (s , EExpr (E.Subst s' e)) -> execSubstTyped s <$> execSubstTyped s' e
    (_ , EExpr E.Univ) -> return e
    (s , EExpr (E.Pi a)) -> pi $ execSubstAbs a
  where
    mapVar k = Fix <$> (mapExpr (const $ EExpr $ E.Var k) e `typed`) <$> metaVar

execSubstAbs :: E.Subst (ExprFWith a (TypedExpr a)) -> E.Abs (TypedExpr a) -> E.Abs (TypedExpr a)
execSubstAbs s (E.Abs t e) =
  E.Abs (execSubst s t) $ execSubst (var 0 `E.Dot` (E.Shift 1 `composeSubst` s)) e

data Error r =
  UnifyError (E.ExprF r) (E.ExprF r)