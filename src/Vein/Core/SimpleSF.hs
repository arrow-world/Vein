{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Vein.Core.SimpleSF where

import Vein.Core.Monoidal.Monoidal

class Signal a where
class (Signal a, Signal b) => PrimitiveSF f a b where

data (><) a b where
  SPair :: (Signal a, Signal b) => a >< b

data SVoid = SVoid

instance Signal SVoid
instance Signal (a >< b)

data SF a b where
  Primitive :: PrimitiveSF f a b => f -> SF a b

  Id :: Signal a => SF a a
  Compose :: (Signal a, Signal b, Signal c) => SF a b -> SF b c -> SF a c

  Product :: (Signal a, Signal b, Signal a', Signal b') =>
    SF a b -> SF a' b' -> SF (a >< a') (b >< b')

  UnitorL :: (Signal a) => SF (SVoid >< a) a
  UnitorR :: (Signal a) => SF (a >< SVoid) a
  UnunitorL :: (Signal a) => SF a (SVoid >< a)
  UnunitorR :: (Signal a) => SF a (a >< SVoid)

  Assoc :: (Signal a, Signal b, Signal c) => SF ((a >< b) >< c) (a >< (b >< c))
  Unassoc :: (Signal a, Signal b, Signal c) => SF (a >< (b >< c)) ((a >< b) >< c)

  Swap :: (Signal a, Signal b) => SF (a >< b) (b >< a)

  Copy :: (Signal a) => SF a (a >< a)
  Cut :: (Signal a) => SF a SVoid

instance Monoidal SF Signal (><) SVoid where
  id = Id
  (>>>) = Compose

  (**) = Product

  unitorL = UnitorL
  unitorR = UnitorR

  ununitorL = UnunitorL
  ununitorR = UnunitorR

  assoc = Assoc
  unassoc = Unassoc

instance Braided SF Signal (><) SVoid where
  braid = Swap

instance Cartesian SF Signal (><) SVoid where
  diag = Copy
  aug = Cut
