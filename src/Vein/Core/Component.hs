{-# LANGUAGE FlexibleContexts #-}

module Vein.Core.Component where

import qualified Vein.Core.Monoidal.Monoidal as M
import qualified Vein.Core.Monoidal.CompactClosed as CC
import qualified Vein.Core.Module as Module

type Connector = CC.Object Module.QN
type NamedConnector = CC.Object (Module.Named Module.QN)

type Component = CC.Morphism Module.QN Module.QN
type NamedComponent = Module.Named (CC.Morphism (Module.Named Module.QN) (Module.Named Module.QN))

data Const =
    CConnector Connector
  | CComponent Component
    deriving (Eq, Show)

type Env = Module.ModuleMap Const