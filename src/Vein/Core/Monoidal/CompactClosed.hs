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
    deriving (Eq, Show)

type Morphism m a = Monoidal.Morphism (DualityM m a) (DualityO a)

instance Monoidal.IMorphism (DualityM m o) m (DualityO o) where
  doco doco' m =
    case m of
      DualityM g -> doco' g
      Ev x       -> pure (x >< (Monoidal.Object $ Dual x), Monoidal.Unit)
      Cv x       -> pure (Monoidal.Unit, x >< (Monoidal.Object $ Dual x))