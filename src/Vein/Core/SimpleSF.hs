{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Vein.Core.SimpleSF where

import Vein.Core.Monoidal.Monoidal

-- a type-class of signal types.
class Signal a where

-- a type-class of primitive signal functions.
class (Signal a, Signal b) => PrimitiveSF f a b where

-- a signal type such as a pair (product) of two signal types.
data (><) a b where
  SPair :: (Signal a, Signal b) => a >< b
instance Signal (a >< b)

-- a signal type of unit.
data SUnit = SUnit
instance Signal SUnit

-- a datatype representing signal functions from a signal type `a` to a signal type `b`.
data SF a b where
  Primitive :: PrimitiveSF f a b => f -> SF a b

  Id :: Signal a => SF a a
  Compose :: (Signal a, Signal b, Signal c) => SF a b -> SF b c -> SF a c

  Product :: (Signal a, Signal b, Signal a', Signal b') =>
    SF a b -> SF a' b' -> SF (a >< a') (b >< b')

  UnitorL :: (Signal a) => SF (SUnit >< a) a
  UnitorR :: (Signal a) => SF (a >< SUnit) a
  UnunitorL :: (Signal a) => SF a (SUnit >< a)
  UnunitorR :: (Signal a) => SF a (a >< SUnit)

  Assoc :: (Signal a, Signal b, Signal c) => SF ((a >< b) >< c) (a >< (b >< c))
  Unassoc :: (Signal a, Signal b, Signal c) => SF (a >< (b >< c)) ((a >< b) >< c)

  Swap :: (Signal a, Signal b) => SF (a >< b) (b >< a)

  Copy :: (Signal a) => SF a (a >< a)
  Cut :: (Signal a) => SF a SUnit

instance Monoidal SF Signal (><) SUnit where
  id = Id
  (>>>) = Compose

  (**) = Product

  unitorL = UnitorL
  unitorR = UnitorR

  ununitorL = UnunitorL
  ununitorR = UnunitorR

  assoc = Assoc
  unassoc = Unassoc

instance Braided SF Signal (><) SUnit where
  braid = Swap

instance Cartesian SF Signal (><) SUnit where
  diag = Copy
  aug = Cut
