{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Vein.Core.Monoidal.Monoidal where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Data.Text (Text)
import Data.Fix

data ObjectUnfixed a ob =
    Unit
  | Lift a
  | Ob ob
  | ProductO ob ob
  deriving Eq

newtype Object a = Object (Fix (ObjectUnfixed a))
  deriving Eq

data MorphismUnfixed a m =
    Id a
  | Compose m m 
  | ProductM m m
  | UnitorL a
  | UnitorR a
  | UnunitorL a
  | UnunitorR a
  | Assoc a a a
  | Unassoc a a a
    deriving Eq

newtype Morphism a = Morphism (Fix (MorphismUnfixed a))

data BraidedMorphismUnfixed a m =
    BraidedMor (MorphismUnfixed a m)
  | Braid (Object a) (Object a)
type BraidedMorphism a = Fix (BraidedMorphismUnfixed a)

data CartesianMorphismUnfixed a m =
    CartesianMor (MorphismUnfixed a m)
  | Diag (Object a)
  | Aug (Object a)
type CartesianMorphism a = Fix (CartesianMorphismUnfixed a)

data CartesianClosedMorphismUnfixed a m =
    CartesianClosedMor (CartesianMorphismUnfixed a m)
  | Eval (Object a) (Object a)
type CartesianClosedMorphism a = Fix (CartesianClosedMorphismUnfixed a)

data SymmetricMorphismUnfixed a m =
    SymmetricMor (MorphismUnfixed a m)
type SymmetricMorphism a = Fix (SymmetricMorphismUnfixed a)

data TracedMorphismUnfixed a m =
    TracedMor (SymmetricMorphismUnfixed a m)
  | Trace m
type TracedMorphism a = Fix (TracedMorphismUnfixed a)


data CompactClosedObjectUnfixed a ob =
    CompactClosedOb (ObjectUnfixed a ob)
  | Dual ob
  deriving Eq

newtype CompactClosedObject a =
  CompactClosedObject (Fix (CompactClosedObjectUnfixed a))

data CompactClosedMorphismUnfixed a m =
    CompactClosedMor (SymmetricMorphismUnfixed a m)
  | Ev (CompactClosedObject a)
  | Unev (CompactClosedObject a)
type CompactClosedMorphism a = Fix (CompactClosedMorphismUnfixed a)
