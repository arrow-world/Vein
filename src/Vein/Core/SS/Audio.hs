{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Vein.Core.SS.Audio where

import Vein.Core.SS.SS
import Vein.Core.DTCV
import GHC.TypeNats
import Data.Proxy

data AudioSink_Proxy fs = AudioSink
type AudioSink (fs :: Nat) = AudioSink_Proxy (Proxy fs)

instance Sink (AudioSink_Proxy fs) (DTCV_Proxy fs) where
