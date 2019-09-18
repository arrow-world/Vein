{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Vein.Core.DTCV where

import Vein.Core.SimpleSF
import GHC.TypeNats
import Data.Proxy

-- |
-- a signal type of discrete time continuous valued signals.
--
-- >>> :set -XDataKinds
-- >>> import Data.Proxy
-- >>> audio = DTCV :: DTCV (Proxy 44100)
data DTCV_Proxy fs = DTCV
type DTCV (fs :: Nat) = DTCV_Proxy (Proxy fs)

instance Signal (DTCV_Proxy fs) where
