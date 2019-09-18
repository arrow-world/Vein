module Vein.Core.DTCV where

import Vein.Core.SimpleSF

-- |
-- a signal type of discrete time continuous valued signals.
--
-- >>> :set -XDataKinds
-- >>> import Data.Proxy
-- >>> audio = DTCV :: DTCV (Proxy 44100)
data DTCV fs = DTCV

instance Signal (DTCV fs) where
