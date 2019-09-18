{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Vein.Core.SS.Time where

import Vein.Core.SS.SS
import Vein.Core.DTCV
import Vein.Core.Behavior
import GHC.TypeNats
import Data.Proxy

data TimeSource = TimeSource

instance Source TimeSource (Behavior Double) where
