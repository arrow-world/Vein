{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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


type CompactClosedCartesianClosedCartesian m o =
  Monoidal.CartesianClosed (Monoidal.Cartesian (DualityM (Monoidal.Braided m (DI o)) (DI o)) (DI o)) (DI o)

newtype CompactClosedCartesianClosedCartesianMorphismF m o r =
  CompactClosedCartesianClosedCartesianMorphismF
    (CompactClosedCartesianClosedCartesian (Monoidal.MorphismF m (DI o) r) o)

type CompactClosedCartesianClosedCartesianMorphism m o =
  Fix (CompactClosedCartesianClosedCartesianMorphismF m o)

docoCompactClosedCartesianClosedCartesianMorphism ::  Monad f =>
                                                            (m -> f (Monoidal.Object (DI o), Monoidal.Object (DI o)))
                                                        ->  CompactClosedCartesianClosedCartesianMorphism m o
                                                        ->  f (Monoidal.Object (DI o), Monoidal.Object (DI o))
docoCompactClosedCartesianClosedCartesianMorphism docoM (Fix (CompactClosedCartesianClosedCartesianMorphismF m)) = 
  (
    Monoidal.docoCartesianClosed $ Monoidal.docoCartesian $ docoDualityM $ Monoidal.docoBraided $ Monoidal.docoMorphismF docoM
      (docoCompactClosedCartesianClosedCartesianMorphism docoM)
  ) m