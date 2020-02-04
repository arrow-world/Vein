{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DatatypeContexts #-}

module Vein.Core.Monoidal.CompactClosed where

import qualified Vein.Core.Monoidal.Monoidal as Monoidal
import Vein.Core.Monoidal.Monoidal ((><), Object (..))

import Data.Fix (Fix (..) , cata)
import Control.Monad.Fix (fix)


data D a =
    D a
  | Dual (Monoidal.Object (D a))
  deriving (Eq, Functor, Show)


class DualityObject a where
  dual :: Monoidal.Object a -> a

instance DualityObject (D a) where
  dual = Dual


data DualityObject o => DualityM o m =
    DualityM m
  | Ev (Monoidal.Object o)
  | Cv (Monoidal.Object o)
    deriving (Eq, Show, Functor, Foldable, Traversable)


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
                  ->  DualityM o m
                  ->  f (Object o, Object o)
docoDualityM docoM f =
  case f of
    DualityM g -> docoM g
    Ev x       -> pure (x >< (Monoidal.Object $ dual x), Monoidal.Unit)
    Cv x       -> pure (Monoidal.Unit, x >< (Monoidal.Object $ dual x))

type CompactClosedCartesianObject o = Object (D o)

type CompactClosedCartesian o m =
  Monoidal.Cartesian (D o) (DualityM (D o) (Monoidal.Braided (D o) m))

newtype CompactClosedCartesianMorphismF m o r =
  CompactClosedCartesianMorphismF
    (CompactClosedCartesian o (Monoidal.MorphismF m (D o) r))
  deriving (Eq, Show, Functor, Foldable, Traversable)

type CompactClosedCartesianMorphism m o =
  Fix (CompactClosedCartesianMorphismF m o)

docoCompactClosedCartesianMorphismF ::  Monad f =>
                                                            (m -> f (Monoidal.Object (D o), Monoidal.Object (D o)))
                                                        ->  (r -> f (Monoidal.Object (D o) , Monoidal.Object (D o)))
                                                        ->  CompactClosedCartesianMorphismF m o r
                                                        ->  f (Monoidal.Object (D o), Monoidal.Object (D o))
docoCompactClosedCartesianMorphismF docoM docoR (CompactClosedCartesianMorphismF m) = 
  (Monoidal.docoCartesian $ docoDualityM $ Monoidal.docoBraided $ Monoidal.docoMorphismF docoM docoR) m

docoCompactClosedCartesianMorphism ::  Monad f =>
                                                            (m -> f (Monoidal.Object (D o), Monoidal.Object (D o)))
                                                        ->  CompactClosedCartesianMorphism m o
                                                        ->  f (Monoidal.Object (D o), Monoidal.Object (D o))
docoCompactClosedCartesianMorphism docoM =
  cata $ docoCompactClosedCartesianMorphismF docoM id