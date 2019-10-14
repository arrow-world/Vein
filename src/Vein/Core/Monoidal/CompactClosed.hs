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


data DualityM m a =
    DualityM m
  | Ev (Object a)
  | Cv (Object a)
    deriving (Eq, Functor, Show)

type Morphism m a = Monoidal.Morphism (DualityM m a) (DualityO a)

instance Monoidal.Arrow m (DualityO a) => Monoidal.Arrow (DualityM m a) (DualityO a) where
  doco f = case f of
    DualityM g -> Monoidal.doco g
    Ev x       -> (x >< (Monoidal.Object $ Dual x), Monoidal.Unit)
    Cv x       -> (Monoidal.Unit, x >< (Monoidal.Object $ Dual x))