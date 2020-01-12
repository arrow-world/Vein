{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DatatypeContexts #-}

module Vein.Core.Monoidal.CompactClosed where

import qualified Vein.Core.Monoidal.Monoidal as Monoidal
import Vein.Core.Monoidal.Monoidal ((><), Object (..))

import Data.Fix (Fix (..))


data D a =
    D a
  | Dual (Monoidal.Object (D a))
  deriving (Eq, Functor, Show)


class DualityObject a where
  dual :: Monoidal.Object a -> a

instance DualityObject (D a) where
  dual = Dual


data DualityObject o => DualityM m o =
    DualityM m
  | Ev (Monoidal.Object o)
  | Cv (Monoidal.Object o)
    deriving (Eq, Show)


data DI o =
    DI o
  | Dual' (Monoidal.Object (DI o))
  | Hom' (Monoidal.Object (DI o)) (Monoidal.Object (DI o))
    deriving (Eq, Show)

instance DualityObject (DI a) where
  dual = Dual'

instance Monoidal.WithInternalHom (DI a) where
  hom = Hom'


docoDualityM :: (Applicative f, DualityObject o) =>
                      (m -> f (Object o, Object o))
                  ->  DualityM m o
                  ->  f (Object o, Object o)
docoDualityM docoM f =
  case f of
    DualityM g -> docoM g
    Ev x       -> pure (x >< (Monoidal.Object $ dual x), Monoidal.Unit)
    Cv x       -> pure (Monoidal.Unit, x >< (Monoidal.Object $ dual x))

type CompactClosedCartesianObject o = Object (D o)

type CompactClosedCartesian m o =
  Monoidal.Cartesian (DualityM (Monoidal.Braided m (D o)) (D o)) (D o)

newtype CompactClosedCartesianMorphismF m o r =
  CompactClosedCartesianMorphismF
    (CompactClosedCartesian (Monoidal.MorphismF m (D o) r) o)
  deriving (Eq, Show)

type CompactClosedCartesianMorphism m o =
  Fix (CompactClosedCartesianMorphismF m o)

docoCompactClosedCartesianMorphism ::  Monad f =>
                                                            (m -> f (Monoidal.Object (D o), Monoidal.Object (D o)))
                                                        ->  CompactClosedCartesianMorphism m o
                                                        ->  f (Monoidal.Object (D o), Monoidal.Object (D o))
docoCompactClosedCartesianMorphism docoM (Fix (CompactClosedCartesianMorphismF m)) = 
  (
    Monoidal.docoCartesian $ docoDualityM $ Monoidal.docoBraided $ Monoidal.docoMorphismF docoM
      (docoCompactClosedCartesianMorphism docoM)
  ) m