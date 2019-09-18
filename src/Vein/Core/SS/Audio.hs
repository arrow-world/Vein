{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Vein.Core.SS.Audio where

import Vein.Core.SS.SS
import Vein.Core.DTCV
import GHC.TypeNats
import Data.Proxy

data AudioSink (fs :: Nat) = AudioSink

instance Sink (AudioSink fs) (DTCV fs) where
