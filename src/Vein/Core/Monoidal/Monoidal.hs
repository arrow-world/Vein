{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Vein.Core.Monoidal.Monoidal where

import Data.Text (Text)
import Data.Fix


data Object a =
    Unit
  | ProductO (Object a) (Object a)
  | Object a
    deriving (Eq, Functor, Show)

(><) = ProductO


data WithInternalHom a =
    WithInternalHom a
  | Hom (Object (WithInternalHom a)) (Object (WithInternalHom a))
    deriving (Eq, Functor, Show)

type ObjectWithInternalHom a = Object (WithInternalHom a)


class IMorphism m m' o | m -> m' o where
  doco :: Applicative f =>
            (m' -> f (Object o, Object o)) -> m -> f (Object o, Object o)

docoLift :: (Applicative f, IMorphism m m' o, IMorphism m' m'' o) =>
              (m'' -> f (Object o, Object o)) -> m -> f (Object o, Object o)
docoLift doco' = doco (doco doco')


data Morphism m a =
    Id (Object a)
  | Compose (Morphism m a) (Morphism m a)
  | ProductM (Morphism m a) (Morphism m a)
  | UnitorL (Object a)
  | UnitorR (Object a)
  | UnunitorL (Object a)
  | UnunitorR (Object a)
  | Assoc (Object a) (Object a) (Object a)
  | Unassoc (Object a) (Object a) (Object a)
  | Morphism m
    deriving (Eq, Functor, Show)

instance IMorphism (Morphism m o) m o where
  doco = docoA

docoA :: Applicative f =>
  (m -> f (Object a, Object a)) -> Morphism m a -> f (Object a, Object a)
docoA doco' f =
  case f of
    Id x          -> pure (x, x)
    Compose g h   -> (,) <$> dom g <*> dom h
    ProductM g h  -> (,) <$> d <*> c
      where d = (><) <$> dom g <*> dom h
            c = (><) <$> cod g <*> cod h
    UnitorL x     -> pure (Unit >< x, x)
    UnitorR x     -> pure (x >< Unit, x)
    UnunitorL x   -> pure (x, Unit >< x)
    UnunitorR x   -> pure (x, x >< Unit)
    Assoc x y z   -> pure ((x >< y) >< z, x >< (y >< z))
    Unassoc x y z -> pure (x >< (y >< z), (x >< y) >< z)
    Morphism g    -> doco' g
  where
    dom = domA doco'
    cod = codA doco'

domA :: Applicative f => (m -> f (Object a, Object a)) -> Morphism m a -> f (Object a)
domA doco' f = fst <$> docoA doco' f

codA :: Applicative f => (m -> f (Object a, Object a)) -> Morphism m a -> f (Object a)
codA doco' f = snd <$> docoA doco' f


data Braided m a =
    Braided m
  | Braid (Object a) (Object a)
    deriving (Eq, Functor, Show)

type BraidedMorphism m a = Morphism (Braided m a) a

instance IMorphism (Braided m o) m o where
  doco doco' m =
    case m of
      Braided g -> doco' g
      Braid x y -> pure (x >< y, y >< x)


data Cartesian m a =
    Cartesian m
  | Diag (Object a)
  | Aug (Object a)
    deriving (Eq, Functor, Show)

type CartesianMorphism m a = Morphism (Cartesian m a) a

instance IMorphism (Cartesian m o) m o where
  doco doco' m =
    case m of
      Cartesian g -> doco' g
      Diag x -> pure (x, x >< x)
      Aug x -> pure (x, Unit)


data CartesianClosed m a =
    CartesianClosed m
  | Eval (Object (WithInternalHom a)) (Object (WithInternalHom a))
    deriving (Eq, Show)

type CartesianClosedMorphism m a = Morphism (CartesianClosed m a) (WithInternalHom a)

instance IMorphism (CartesianClosed m o) m (WithInternalHom o) where
  doco doco' m =
    case m of
      CartesianClosed g -> doco' g
      Eval x y          -> pure ((Object $ Hom x y) >< x, y)


data Symmetric m a =
    Symmetric m
    deriving (Eq, Functor)

type SymmetricMorphism m a = Morphism (Symmetric m a) a

instance IMorphism (Symmetric m o) m o where
  doco doco' (Symmetric m) = doco' m


data Traced m a =
    Traced (Morphism (Traced m a) a)
  | Trace (Morphism (Traced m a) a)
    deriving (Eq, Show)

trace :: Eq o => Morphism (Traced m o) o -> Maybe (Traced m o)
trace m = do
  doco' <- doco (const Nothing) m 
  case doco' of
    (ProductO _ dom, ProductO _ cod)
      | dom == cod -> Just $ Trace m
      | otherwise  -> Nothing
    otherwise -> Nothing

type TracedMorphism m a = Morphism (Traced m a) a

instance IMorphism (Traced m o) m o where
  doco = docoTraced

docoTraced :: Applicative f =>
  (m -> f (Object a, Object a)) -> Traced m a -> f (Object a, Object a)
docoTraced doco' f = case f of
  Traced g -> docoTracedMorphism doco' g
  Trace g  -> doco'' <$> docoTracedMorphism doco' g
    where doco'' (ProductO dom _, ProductO cod _) = (dom, cod)

docoTracedMorphism :: Applicative f =>
  (m -> f (Object a, Object a)) -> TracedMorphism m a -> f (Object a, Object a)
docoTracedMorphism doco' = docoA (docoTraced doco')