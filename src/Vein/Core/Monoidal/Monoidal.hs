{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Vein.Core.Monoidal.Monoidal
  ( Object, unit, ob, (><)
  )where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Data.Text (Text)
import Data.Fix

data Object a =
    Unit
  | ProductO (Object a) (Object a)
  | Ob a
  deriving Eq

unit :: Object a
unit = Unit

ob :: a -> Object a
ob = Ob

(><) :: Object a -> Object a -> Object a
(><) = ProductO

data MorphismUnfixed a m =
    Id (Object a)
  | Compose m m 
  | ProductM m m
  | UnitorL (Object a)
  | UnitorR (Object a)
  | UnunitorL (Object a)
  | UnunitorR (Object a)
  | Assoc (Object a) (Object a) (Object a)
  | Unassoc (Object a) (Object a) (Object a)
  | Mor m
    deriving Eq

type Morphism a = Fix (MorphismUnfixed a)

id :: Object a -> MorphismUnfixed a m
id = Id

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


data CompactClosedObject a =
    CompactClosedOb (Object a)
  | Dual (Object a)
  deriving Eq

data CompactClosedMorphismUnfixed a m =
    CompactClosedMor (SymmetricMorphismUnfixed a m)
  | Ev (CompactClosedObject a)
  | Unev (CompactClosedObject a)
type CompactClosedMorphism a = Fix (CompactClosedMorphismUnfixed a)
