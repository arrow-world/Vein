{-# LANGUAGE FlexibleContexts #-}

module Vein.Core.Component where

import qualified Vein.Core.Monoidal.Monoidal as M
import qualified Vein.Core.Monoidal.CompactClosed as CC
import qualified Vein.Core.Module as Module

type Connector = CC.Object Module.QN

data Component = Component (CC.Morphism Module.QN (Module.QN))
  deriving (Eq, Show)