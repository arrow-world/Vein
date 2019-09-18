{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vein.Core.DTCV where

import Vein.Core.SimpleSF
import Vein.Core.Behavior
import GHC.TypeNats
import Data.Proxy

-- |
-- a signal type of discrete time continuous valued signals.
--
-- >>> :set -XDataKinds
-- >>> import Data.Proxy
-- >>> audio = DTCV :: DTCV (Proxy 44100)
data DTCV (fs :: Nat) = DTCV

instance Signal (DTCV fs) where

data ToDTCV_FromBehaviorFloat (fs :: Nat) = ToDTCV_FromBehaviorFloat
instance PrimitiveSF (ToDTCV_FromBehaviorFloat fs) (Behavior Double) (DTCV fs)
