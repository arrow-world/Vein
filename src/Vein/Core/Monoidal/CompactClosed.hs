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
type IhD o = IH (DualityO o)
type IhDIh o = IH (DualityO (IH o))
type DIhD o = DualityO (IH (DualityO o))

type CompactClosedCartesianClosedCartesian m o =
  Monoidal.CartesianClosed (Monoidal.Cartesian (DualityM (Monoidal.Braided m (IhDIh o)) (IhD o)) (IhDIh o)) (DIh o)

newtype CompactClosedCartesianClosedCartesianMorphismF m o r =
  CompactClosedCartesianClosedCartesianMorphismF
    (CompactClosedCartesianClosedCartesian (Monoidal.MorphismF m (IhDIh o) r) o)

type CompactClosedCartesianClosedCartesianMorphism m o =
  Fix (CompactClosedCartesianClosedCartesianMorphismF m o)

dual :: Object o -> Object o
dual x = Monoidal.joinO $ fmap dual' x
  where
    dual' y = case y of
      DualityO y -> Monoidal.Object $ Dual (Monoidal.Object $ DualityO y)
      Dual y -> y

flipDIhD :: Monoidal.Object (DIhD o) -> Monoidal.Object (IhDIh o)
flipDIhD x = Monoidal.joinO $ fmap elimHeadD dIhDIh
  where
    dIhDIh = (fmap $ fmap $ fmap $ fmap Monoidal.WithInternalHom) x

    elimHeadD :: DIhD (IH o) -> Monoidal.Object (IhDIh o)
    elimHeadD y = case y of
      DualityO z -> Monoidal.Object z
      Dual z -> (fmap $ fmap $ Dual . Monoidal.Object) (Monoidal.joinO $ fmap elimHeadD z)

{-
docoCompactClosedCartesianClosedCartesianMorphism ::  Monad f =>
                                                            (m -> f (Monoidal.Object (IhDIh o), Monoidal.Object (IhDIh o)))
                                                        ->  CompactClosedCartesianClosedCartesianMorphism m o
                                                        ->  f (Monoidal.Object (IhDIh o), Monoidal.Object (IhDIh o))
docoCompactClosedCartesianClosedCartesianMorphism docoM (Fix (CompactClosedCartesianClosedCartesianMorphismF m)) = 
    (
      Monoidal.docoCartesianClosed $ Monoidal.docoCartesian $ docoDualityM $ Monoidal.docoBraided $ Monoidal.docoMorphismF docoM
        (docoCompactClosedCartesianClosedCartesianMorphism docoM)
    ) m
  where
    docoCartesian' docoM m = do
      (x,y) <- Monoidal.docoCartesian docoM m
      return (fmap ih x , fmap ih y)
    
    ih = Monoidal.WithInternalHom
-}