{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vein.Core.Monoidal.CompactClosed where

import qualified Vein.Core.Monoidal.Monoidal as Monoidal
import Vein.Core.Monoidal.Monoidal ((><))

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