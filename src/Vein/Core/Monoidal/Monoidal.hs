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

import Data.Fix


data Object a =
    Unit
  | ProductO (Object a) (Object a)
  | Object a
    deriving (Eq, Functor, Show)

(><) = ProductO

joinO :: Object (Object o) -> Object o
joinO x = case x of
  Unit -> Unit
  ProductO x y -> ProductO (joinO x) (joinO y)
  Object x -> x

lenOfOb :: Object a -> Int
lenOfOb (ProductO x y) = lenOfOb x + lenOfOb y
lenOfOb Unit = 0
lenOfOb (Object _) = 1

flattenOb :: Object a -> [a]
flattenOb (Object x) = [x]
flattenOb Unit = []
flattenOb (ProductO x y) = flattenOb x ++ flattenOb y


data WI a =
    WI a
  | Hom (Object (WI a)) (Object (WI a))
  deriving (Eq, Show)

class WithInternalHom a where
  hom :: Object a -> Object a -> a


data MorphismF m a r =
    Id (Object a)
  | Compose r r
  | ProductM r r
  | UnitorL (Object a)
  | UnitorR (Object a)
  | UnunitorL (Object a)
  | UnunitorR (Object a)
  | Assoc (Object a) (Object a) (Object a)
  | Unassoc (Object a) (Object a) (Object a)
  | Morphism m
    deriving (Eq, Functor, Show)

docoMorphismF ::  Monad f =>
                        (m -> f (Object a, Object a))
                    ->  (r -> f (Object a, Object a))
                    ->  MorphismF m a r
                    ->  f (Object a, Object a)
docoMorphismF docoM docoR f =
  case f of
    Id x          -> pure (x, x)
    Compose g h   ->
      do
        (dom , _) <- docoR g
        (_ , cod) <- docoR h
        pure (dom , cod)
    ProductM g h  ->
      do
        (a  , b ) <- docoR g
        (a' , b') <- docoR h
        pure (a >< a' , b >< b')
    UnitorL x     -> pure (Unit >< x, x)
    UnitorR x     -> pure (x >< Unit, x)
    UnunitorL x   -> pure (x, Unit >< x)
    UnunitorR x   -> pure (x, x >< Unit)
    Assoc x y z   -> pure ((x >< y) >< z, x >< (y >< z))
    Unassoc x y z -> pure (x >< (y >< z), (x >< y) >< z)
    Morphism g    -> docoM g


data Braided m o =
    Braided m
  | Braid (Object o) (Object o)
    deriving (Eq, Functor, Show)

docoBraided ::  Applicative f =>
                      (m -> f (Object o, Object o))
                  ->  Braided m o
                  ->  f (Object o, Object o)
docoBraided docoM f =
  case f of
    Braided g -> docoM g
    Braid x y -> pure (x >< y , y >< x)


data Cartesian m o =
    Cartesian m
  | Diag (Object o)
  | Aug (Object o)
    deriving (Eq, Functor, Show)

docoCartesian ::  Applicative f =>
                      (m -> f (Object o, Object o))
                  ->  Cartesian m o
                  ->  f (Object o, Object o)
docoCartesian docoM f =
  case f of
    Cartesian g -> docoM g
    Diag x      -> pure (x, x >< x)
    Aug x       -> pure (x, Unit)


data WithInternalHom o => CartesianClosed m o =
    CartesianClosed m
  | Eval (Object o) (Object o)
    deriving (Eq, Show)

docoCartesianClosed ::  (Applicative f, WithInternalHom o) =>
                            (m -> f (Object o, Object o))
                        ->  CartesianClosed m o
                        ->  f (Object o, Object o)
docoCartesianClosed docoM f =
  case f of
    CartesianClosed g -> docoM g
    Eval x y          -> pure ((Object $ hom x y) >< x, y)


instance WithInternalHom (WI o) where
  hom = Hom


type CartesianClosedBraidedCartesian m o =
  (CartesianClosed (Cartesian (Braided m (WI o)) (WI o)) (WI o))

newtype CartesianClosedBraidedCartesianMorphismF m o r =
  CartesianClosedBraidedCartesianMorphismF
    (CartesianClosedBraidedCartesian (MorphismF m (WI o) r) o)
  deriving (Eq, Show)

type CartesianClosedBraidedCartesianMorphism m o =
  Fix (CartesianClosedBraidedCartesianMorphismF m o)

docoCartesianClosedBraidedCartesianMorphism ::  Monad f =>
                                                      (m -> f (Object (WI o), Object (WI o)))
                                                  ->  CartesianClosedBraidedCartesianMorphism m o
                                                  ->  f (Object (WI o), Object (WI o))
docoCartesianClosedBraidedCartesianMorphism docoM (Fix (CartesianClosedBraidedCartesianMorphismF m)) = 
  (
    docoCartesianClosed $ docoCartesian $ docoBraided $ docoMorphismF docoM
      (docoCartesianClosedBraidedCartesianMorphism docoM)
  ) m

data Traced m o =
    Traced m
  | Trace m
    deriving (Eq, Show, Functor)