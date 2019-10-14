{-# LANGUAGE FlexibleContexts #-}

module Vein.Signal.Component where

import qualified Vein.Core.Monoidal.Monoidal as M
import qualified Vein.Core.Monoidal.CompactClosed as CC

type Connector a = CC.Object a

data Component m a = Component (CC.Morphism m a)

component :: Eq a => M.Arrow m (CC.DualityO a) => CC.Morphism m a -> Maybe (Component m a)
component f
  | M.codomain f == M.Unit = Just $ Component f
  | otherwise              = Nothing