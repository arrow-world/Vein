{-# LANGUAGE FlexibleContexts #-}

module Vein.Core.Component where

import qualified Vein.Core.Monoidal.Monoidal as M
import qualified Vein.Core.Monoidal.CompactClosed as CC
import qualified Vein.Core.Module as Module

type Connector = CC.Object Module.QN

type Component = CC.Morphism Module.QN Module.QN

data Const =
    CConnector Connector
  | CComponent Component
    deriving (Eq, Show)

type Env = Module.ModuleMap Const