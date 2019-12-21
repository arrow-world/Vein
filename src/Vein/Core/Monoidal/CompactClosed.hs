{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vein.Core.Monoidal.CompactClosed where

import qualified Vein.Core.Monoidal.Monoidal as Monoidal
import Vein.Core.Monoidal.Monoidal ((><))

import Data.Fix (Fix (..))

data DualityO a =
    DualityO a
  | Dual (Monoidal.Object (DualityO a))
    deriving (Eq, Functor, Show)

type Object a = Monoidal.Object (DualityO a)


data DualityM m o =
    DualityM m
  | Ev (Object o)
  | Cv (Object o)
    deriving (Eq, Show)


docoDualityM :: Applicative f =>
                      (m -> f (Object o, Object o))
                  ->  DualityM m o
                  ->  f (Object o, Object o)
docoDualityM docoM f =
  case f of
    DualityM g -> docoM g
    Ev x       -> pure (x >< (Monoidal.Object $ Dual x), Monoidal.Unit)
    Cv x       -> pure (Monoidal.Unit, x >< (Monoidal.Object $ Dual x))


type IH o = Monoidal.WithInternalHom o
type DIh o = DualityO (IH o)
type IhDIh o = IH (DualityO (IH o))

type CompactClosedCartesianClosedCartesian o r =
  Monoidal.CartesianClosed (Monoidal.Cartesian (DualityM (Monoidal.Braided r (DIh o)) (IH o)) (DIh o)) (DIh o)

newtype CompactClosedCartesianClosedCartesianMorphismF m o r =
  CompactClosedCartesianClosedCartesianMorphismF
    (Monoidal.MorphismF m (IhDIh o) (CompactClosedCartesianClosedCartesian o r))

type CompactClosedCartesianClosedCartesianMorphism m o =
  Fix (CompactClosedCartesianClosedCartesianMorphismF m o)

docoCompactClosedCartesianClosedCartesian ::  Monad f =>
                                                    (m -> f (Monoidal.Object (IhDIh o), Monoidal.Object (IhDIh o)))
                                                ->  CompactClosedCartesianClosedCartesian o
                                                      (CompactClosedCartesianClosedCartesianMorphism m o)
                                                ->  f (Monoidal.Object (IhDIh o), Monoidal.Object (IhDIh o))
docoCompactClosedCartesianClosedCartesian docoM = 
  Monoidal.docoCartesianClosed $ docoCartesian'
  where
    docoCartesian' m = do
      (x,y) <- (Monoidal.docoCartesian $ docoDualityM $ Monoidal.docoBraided $ undefined docoM) m
      return (fmap ih x , fmap ih y)
    
    ih = Monoidal.WithInternalHom
 
docoCompactClosedCartesianClosedCartesianMorphism docoM (Fix (CompactClosedCartesianClosedCartesianMorphismF f)) =
  Monoidal.docoMorphismF
    docoM
    (docoCompactClosedCartesianClosedCartesian docoM)
    f