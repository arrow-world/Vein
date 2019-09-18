{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Vein.Core.SS.SS where

import Vein.Core.SimpleSF

class Signal a => Source s a where

class Signal a => Sink s a where


data GroundedSF where
  GroundedSF :: (Signal a, Signal b, Source source a, Sink sink b) =>
    SF a b -> source -> sink -> GroundedSF
