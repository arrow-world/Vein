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
import           Data.Fix                       ( Fix(..)
                                                , cata
                                                )
import qualified Data.Map.Lazy as Map
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Default as Default
import Data.Default (Default)

import Prelude hiding (pi)

newtype MetaVar = MetaVar Natural
  deriving (Eq,Show,Ord)

succMetaVar (MetaVar n) = MetaVar $ n + 1

type GlobalEnv a = HashMap.HashMap E.Const (TypedExpr a)
type LocalCtx a = [ExprFWith a (TypedExpr a)]

data Env a = Env
  { globalEnv :: GlobalEnv a
  , localCtx :: LocalCtx a
  }
  deriving (Eq,Show)

mapLocalCtx :: (LocalCtx a -> LocalCtx a) -> Env a -> Env a
mapLocalCtx f (Env e g) = Env e (f g)

data Ctx a = Ctx
  { nextMetaVar :: MetaVar
  , assignments :: Map.Map MetaVar (ExprFWith a (TypedExpr a))
  }
  deriving (Eq,Show)

newtype TypeCheckMonad ann a = TypeCheckMonad (ReaderT (Env ann) (StateT (Ctx ann) (Either (Error (TypedExpr ann)))) a)
  deriving (Functor , Applicative , Monad , MonadError (Error (TypedExpr ann)))


genMetaVar :: TypeCheckMonad a MetaVar
genMetaVar = TypeCheckMonad $ do
  ctx <- lift get
  let metaVar = nextMetaVar ctx
  lift $ put $ ctx { nextMetaVar = succMetaVar metaVar }
  return metaVar

metaVar :: Default a => TypeCheckMonad a (ExprFWith a r)
metaVar = expr . EMetaVar <$> genMetaVar


assign :: MetaVar -> ExprFWith a (TypedExpr a) -> TypeCheckMonad a (ExprFWith a (TypedExpr a))
assign v e = TypeCheckMonad $ do
  ctx <- lift $ get
  lift $ put $ ctx { assignments = Map.insert v e $ assignments ctx }
  return e


resolveConst :: E.Const -> TypeCheckMonad a (TypedExpr a)
resolveConst c = do
  c' <- TypeCheckMonad $ asks $ HashMap.lookup c . globalEnv
  maybe (throwError $ NotInScope c) return c'

lookupLocalCtx :: Natural -> TypeCheckMonad a (ExprFWith a (TypedExpr a))
lookupLocalCtx n = TypeCheckMonad $ asks $ (!! fromIntegral n) . localCtx

localEnv :: (Env ann -> Env ann) -> TypeCheckMonad ann a -> TypeCheckMonad ann a
localEnv f (TypeCheckMonad r) = TypeCheckMonad $ local f r

appendLocalCtx e = localEnv $ mapLocalCtx (e:)


runTypeCheckMonad :: TypeCheckMonad ann a -> GlobalEnv ann -> Ctx ann -> Either (Error (TypedExpr ann)) (a , Ctx ann)
runTypeCheckMonad (TypeCheckMonad r) e = runStateT (runReaderT r $ Env e [])


data ExprF r =
    EMetaVar MetaVar
  | EExpr (E.ExprF r)
  deriving (Eq,Show,Functor,Foldable,Traversable)

data ExprFWith a r = ExprFWith { eAnn :: a , unExpr :: ExprF r }
  deriving (Eq,Show,Functor,Foldable,Traversable)

expr :: Default a => ExprF r -> ExprFWith a r
expr = ExprFWith Default.def

mapExpr f (ExprFWith a e) = ExprFWith a $ f e
setExpr e' (ExprFWith a e) = ExprFWith a e'

traverseExprFWith :: Functor f => (ExprF r -> f (ExprF b)) -> ExprFWith a r -> f (ExprFWith a b)
traverseExprFWith f (ExprFWith a e) = ExprFWith a <$> f e


type Expr a = Fix (ExprFWith a)


data TypedExprF a r = TypedExprF { tefType :: ExprFWith a r , tefTerm :: ExprFWith a r }
  deriving (Eq,Show,Functor,Foldable,Traversable)

type TypedExpr a = Fix (TypedExprF a)

t `typed` ty = TypedExprF ty t
typeof = tefType
termof = tefTerm
mapTerm f (TypedExprF ty e) = TypedExprF ty $ f e

eraseType :: TypedExpr a -> Expr a
eraseType = cata $ Fix . termof


check :: (Default a , Monoid a) => Expr a -> TypeCheckMonad a (E.TypedExpr a)
check = reconstruct >=> execAssign

execAssign :: Default a => TypedExpr a -> TypeCheckMonad a (E.TypedExpr a)
execAssign = undefined

checkTyped :: (Default a , Monoid a) => TypedExpr a -> TypeCheckMonad a ()
checkTyped e = do
  ty <- infer (\e -> checkTyped e *> (return . termof . unFix) e) $ unExpr $ termof $ unFix e
  unify ty $ typeof $ unFix e
  return ()


reconstruct :: (Default a , Monoid a) => Expr a -> TypeCheckMonad a (TypedExpr a)
reconstruct e = Fix <$> liftA2 typed e' (infer (return . termof . unFix) =<< unExpr <$> e')
  where
    e' = reconstruct' $ unFix e

    reconstruct' :: (Default a , Monoid a) => ExprFWith a (Expr a) -> TypeCheckMonad a (ExprFWith a (TypedExpr a))
    reconstruct' = traverseExpr' (reconstruct' . unFix) reconstruct


infer :: (Default a , Monoid a) => (r -> TypeCheckMonad a (ExprFWith a (TypedExpr a)))
                                -> ExprF r -> TypeCheckMonad a (ExprFWith a (TypedExpr a))
infer unfix = \case
  EExpr e -> case e of
    E.Var n -> lookupLocalCtx n

    E.Lam a@(E.Abs t e) -> do
      typeofArg <- unfix t
      typeofE <- appendLocalCtx typeofArg $ infer' e

      return $ pi (Fix $ typeofArg `typed` univ) (Fix $ typeofE `typed` univ)

    E.App e1 e2 -> do
      typeofArg <- infer' e2

      typeofVal <- metaVar
      let fnTy = pi (Fix $ typeofArg `typed` univ) (Fix $ (typeofVal `typed` univ))
      unify fnTy =<< infer' e1

      subst <$> (E.dotNil . Fix <$> ((`typed` typeofArg) <$> unfix e2)) <*> pure (Fix $ typeofVal `typed` univ)
    
    E.Const c -> typeof . unFix <$> resolveConst c

    E.Univ -> return univ

    E.Typing e t -> unfix t
    
    E.Pi (E.Abs t e) -> return univ
    
    E.Subst s e -> do
      s' <- traverse (fmap (Fix . fmap eraseType) . unfix) s
      e' <- Fix . fmap eraseType <$> unfix e
      infer (return . termof . unFix) =<< ((unExpr . termof . unFix) <$> (reconstruct =<< execSubst Default.def s' e'))

  EMetaVar v -> metaVar
  where
    infer' e = infer (return . termof . unFix) =<< unExpr <$> unfix e


unify :: (Default a , Monoid a) => ExprFWith a (TypedExpr a) -> ExprFWith a (TypedExpr a)
                                -> TypeCheckMonad a (ExprFWith a (TypedExpr a))
unify e1 e2 = case (unExpr e1 , unExpr e2) of
  (EMetaVar v1 , EMetaVar v2) -> assign (max v1 v2) $ withMergedAnn $ EMetaVar (min v1 v2)
  
  (EMetaVar v , EExpr e) -> assign v e2
  (EExpr e , EMetaVar v) -> assign v e1

  (EExpr e1 , EExpr e2) -> maybe
    (throwError $ UnifyError e1 e2)
    (fmap (withMergedAnn . EExpr) . traverse id)
    $ zipMatchExprF unifyTyped e1 e2
  
  where withMergedAnn = ExprFWith $ eAnn e1 <> eAnn e2

unifyTyped :: (Default a , Monoid a) => TypedExpr a -> TypedExpr a -> TypeCheckMonad a (TypedExpr a)
unifyTyped e1 e2 = do
  e <- unify (termof $ unFix e1) (termof $ unFix e2)
  t <- unify (typeof $ unFix e1) (typeof $ unFix e2)
  return $ Fix $ e `typed` t


zipMatchExprF :: (a -> b -> c) -> E.ExprF a -> E.ExprF b -> Maybe (E.ExprF c)
zipMatchExprF f = curry $ \case
  (E.Var n , E.Var m) | n == m -> Just $ E.Var n
  (E.Lam a , E.Lam a') -> Just $ E.Lam $ zipMatchAbs a a'
  (E.App e1 e2, E.App e1' e2') -> Just $ E.App (f e1 e1') (f e2 e2')
  (E.Const c , E.Const c') | c == c' -> Just $ E.Const c
  (E.Univ , E.Univ) -> Just E.Univ
  (E.Typing e t , E.Typing e' t') -> Just $ E.Typing (f e e') (f t t')
  (E.Pi a , E.Pi a') -> Just $ E.Pi $ zipMatchAbs a a'
  (E.Subst s e , E.Subst s' e') -> E.Subst <$> zipMatchSubst s s' <*> pure (f e e')
  _ -> Nothing
  where
    zipMatchAbs (E.Abs t e) (E.Abs t' e') = E.Abs (f t t') (f e e')
    zipMatchSubst = curry $ \case
      (E.Shift n , E.Shift m) | n == m -> Just $ E.Shift n
      (E.Dot e s , E.Dot e' s') -> E.Dot (f e e') <$> zipMatchSubst s s'
      _ -> Nothing

traverseExpr' ::  (r -> TypeCheckMonad a (ExprFWith a (TypedExpr a)))
                    -> (r -> TypeCheckMonad a b) -> ExprFWith a r -> TypeCheckMonad a (ExprFWith a b)
traverseExpr' unfix f = traverseExprFWith (traverseExpr unfix f)

traverseExpr ::   (r -> TypeCheckMonad a (ExprFWith a (TypedExpr a)))
                    -> (r -> TypeCheckMonad a b) -> ExprF r -> TypeCheckMonad a (ExprF b)
traverseExpr unfix f = \case
    EMetaVar v -> return $ EMetaVar v
    EExpr (E.Lam a) -> EExpr . E.Lam <$> traverseAbs a
    EExpr (E.Pi a) -> EExpr . E.Lam <$> traverseAbs a
    EExpr e -> EExpr <$> traverse f e
  where
    traverseAbs (E.Abs t e) = E.Abs <$> f t <*> (flip appendLocalCtx (f e) =<< unfix t)


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

{-
execSubstTyped :: Default a => E.Subst (Expr a) -> Expr a -> Expr a
execSubstTyped s (Fix (TypedExprF t e)) = Fix $ TypedExprF (execSubst s t) (execSubst s e)
-}

execSubst :: (Default a , Monoid a) => a -> E.Subst (Expr a) -> Expr a -> TypeCheckMonad a (Expr a)
execSubst ann s e = case (s , unExpr $ unFix e) of
    (E.Shift m , EExpr (E.Var k)) -> setVar $ k + m
    (E.Dot e' _ , EExpr (E.Var 0)) -> return e'
    (E.Dot e' s' , EExpr (E.Var k)) -> setVar $ k - 1
    (_ , EExpr (E.Subst s' e')) -> execSubst ann s =<< execSubst ann s' e'
    (_ , EExpr E.Univ) -> return e
    (_ , EExpr (E.Pi a)) -> setEExpr . E.Pi <$> execSubstAbs ann s a
    (_ , EExpr (E.Lam a)) -> setEExpr . E.Lam <$> execSubstAbs ann s a
    (_ , EExpr (E.App e1 e2)) -> setEExpr <$> (E.App <$> execSubst ann s e1 <*> execSubst ann s e2)
    (_ , EExpr (E.Typing e t)) -> setEExpr <$> (E.Typing <$> execSubst ann s e <*> execSubst ann s t)
    (_ , EExpr (E.Const c)) -> do
      c' <- termof . unFix <$> resolveConst c
      return $ Fix $ ExprFWith (eAnn (unFix e) <> eAnn c') $ fmap eraseType (unExpr c')
    (_ , EMetaVar _) -> return $ Fix $ ExprFWith ann $ EExpr $ E.Subst s e
  where
    setVar k = return $ Fix $ setExpr (EExpr $ E.Var k) $ unFix e
    setEExpr e' = Fix $ setExpr (EExpr e') $ unFix e

execSubstAbs :: (Default a , Monoid a) => a -> E.Subst (Expr a) -> E.Abs (Expr a) -> TypeCheckMonad a (E.Abs (Expr a))
execSubstAbs ann s (E.Abs t e) =
  E.Abs <$> execSubst ann s t <*> execSubst ann (Fix (var 0) `E.Dot` (E.Shift 1 `composeSubst` s)) e

composeSubst :: Default a => E.Subst (Expr a) -> E.Subst (Expr a) -> E.Subst (Expr a)
composeSubst = curry $ \case
  (s , E.Shift 0) -> s
  (E.Dot e s , E.Shift m) -> composeSubst s $ E.Shift $ m - 1
  (E.Shift m , E.Shift n) -> E.Shift $ m + n
  (s , E.Dot e s') -> Fix (subst s e) `E.Dot` (s `composeSubst` s')


data Error r =
    UnifyError (E.ExprF r) (E.ExprF r)
  | NotInScope E.Const