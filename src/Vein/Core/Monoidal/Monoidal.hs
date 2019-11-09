{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
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


class Arrow a b | a -> b where
  domain :: a -> Object b
  domain = fst . doco

  codomain :: a -> Object b
  codomain = snd . doco

  doco :: a -> (Object b, Object b)
  doco f = (domain f, codomain f)


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


instance Arrow m a => Arrow (Morphism m a) a where
  domain f = case f of
    Id x          -> x
    Compose g h   -> domain g
    ProductM g h  -> domain g >< domain h
    UnitorL x     -> Unit >< x
    UnitorR x     -> x >< Unit
    UnunitorL x   -> x
    UnunitorR x   -> x
    Assoc x y z   -> (x >< y) >< z
    Unassoc x y z -> x >< (y >< z)
    Morphism g    -> domain g

  codomain f = case f of
    Id x          -> x
    Compose g h   -> codomain h
    ProductM g h  -> codomain g >< codomain h
    UnitorL x     -> x
    UnitorR x     -> x
    UnunitorL x   -> Unit >< x
    UnunitorR x   -> x >< Unit
    Assoc x y z   -> x >< (y >< z)
    Unassoc x y z -> (x >< y) >< z
    Morphism g    -> codomain g


data Braided m a =
    Braided m
  | Braid (Object a) (Object a)
    deriving (Eq, Functor, Show)

type BraidedMorphism m a = Morphism (Braided m a) a

instance Arrow m a => Arrow (Braided m a) a where
  domain f = case f of
    Braided g -> domain g
    Braid x y -> x >< y

  codomain f = case f of
    Braided g -> domain g
    Braid x y -> y >< x


data Cartesian m a =
    Cartesian m
  | Diag (Object a)
  | Aug (Object a)
    deriving (Eq, Functor, Show)

type CartesianMorphism m a = Morphism (Cartesian m a) a

instance Arrow m a => Arrow (Cartesian m a) a where
  doco f = case f of
    Cartesian g -> doco g
    Diag x      -> (x, x >< x)
    Aug x       -> (x, Unit)


data CartesianClosed m a =
    CartesianClosed m
  | Eval (Object (WithInternalHom a)) (Object (WithInternalHom a))
    deriving (Eq, Functor, Show)

type CartesianClosedMorphism m a = Morphism (CartesianClosed m a) (WithInternalHom a)

instance Arrow m (WithInternalHom a) => Arrow (CartesianClosed m a) (WithInternalHom a) where
  doco f = case f of
    CartesianClosed g -> doco g
    Eval x y          -> ((Object $ Hom x y) >< x, y)


data Symmetric m a =
    Symmetric m
    deriving (Eq, Functor)

type SymmetricMorphism m a = Morphism (Symmetric m a) a

instance Arrow m a => Arrow (Symmetric m a) a where
  doco (Symmetric f) = doco f


data Traced m a =
    Traced (Morphism (Traced m a) a)
  | Trace (Morphism (Traced m a) a)
    deriving (Eq, Show)

type TracedMorphism m a = Morphism (Traced m a) a

docoTraced :: Applicative f =>
  (m -> f (Object a, Object a)) -> Traced m a -> f (Object a, Object a)
docoTraced doco' f = case f of
  Traced g -> docoTracedMorphism doco' g
  Trace g  -> doco'' <$> docoTracedMorphism doco' g
    where doco'' (ProductO dom _, ProductO cod _) = (dom, cod)

docoTracedMorphism :: Applicative f =>
  (m -> f (Object a, Object a)) -> TracedMorphism m a -> f (Object a, Object a)
docoTracedMorphism doco' = docoA (docoTraced doco')

instance Arrow m a => Arrow (Traced m a) a where
  doco f = case f of
    Traced g -> doco g
    Trace g  -> (dom, cod)
      where
        ProductO dom _ = domain g
        ProductO cod _ = codomain g