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
import Data.Types.Injective

class Pairable a where
  (><) :: a -> a -> a

class Pairable a => Destructable a where
  up :: a -> Maybe a
  down :: a -> Maybe a

class Destructable a => PointedDestructable a where
  unit :: a

class PointedDestructable a => Object a


class (Eq a, Object a, Destructable (m a b)) => Morphism m a b where
  id :: a -> m a b
  (>>) :: m a b -> m a b -> Maybe (m a b)
  left :: m a b -> Maybe (m a b)
  right :: m a b -> Maybe (m a b)

  unitorL :: a -> m a b
  unitorR :: a -> m a b
  ununitorL :: a -> m a b
  ununitorR :: a -> m a b
  assoc :: a -> a -> a -> m a b
  unassoc :: a -> a -> a -> m a b

  domain :: m a b -> a
  codomain :: m a b -> a

isComposable :: Object a => Morphism m a b => m a b -> m a b -> Bool
isComposable f g = codomain f == domain g


data ObjectUnfixed a ob =
    Unit
  | Lift a
  | Ob ob
  | ProductO ob ob
  deriving Eq

newtype ObjectData a = ObjectData (Fix (ObjectUnfixed a))
  deriving Eq

instance Pairable (ObjectData a) where
  (ObjectData x) >< (ObjectData y) = ObjectData $ Fix $ ProductO x y

instance Destructable (ObjectData a) where
  up (ObjectData (Fix x)) = case x of
    ProductO y z -> Just (ObjectData y)
    _ -> Nothing

  down (ObjectData (Fix x)) = case x of
    ProductO y z -> Just (ObjectData z)
    _ -> Nothing

instance PointedDestructable (ObjectData a) where
  unit = ObjectData $ Fix Unit

instance Object (ObjectData a)


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

newtype MorphismData a = MorphismData (Fix (MorphismUnfixed a))

instance Pairable (MorphismData a) where
  (MorphismData f) >< (MorphismData g) = MorphismData $ Fix $ ProductM f g

instance Destructable (MorphismData a) where
  up (MorphismData (Fix f)) = case f of
    ProductM g h -> Just $ MorphismData g
    _ -> Nothing
    
  down (MorphismData (Fix f)) = case f of
    ProductM g h -> Just $ MorphismData h
    _ -> Nothing

instance (Eq a, Injective (MorphismUnfixed (ObjectData a) b) b)
  => Morphism MorphismUnfixed (ObjectData a) b where
  id x = Id x

  f >> g =
    if isComposable f g then
      Just $ Compose (to f) (to g)
    else
      Nothing

  left (MorphismData (Fix f)) = case f of
    Compose g h -> Just $ MorphismData g
    _ -> Nothing

  right (MorphismData (Fix f)) = case f of
    Compose g h -> Just $ MorphismData h
    _ -> Nothing

  unitorL x = MorphismData $ Fix $ UnitorL x
  unitorR x = MorphismData $ Fix $ UnitorR x
  ununitorL x = MorphismData $ Fix $ UnunitorL x
  ununitorR x = MorphismData $ Fix $ UnunitorR x
  assoc x y z = MorphismData $ Fix $ Assoc x y z
  unassoc x y z = MorphismData $ Fix $ Unassoc x y z

  domain (MorphismData (Fix m)) = case m of
    Id x -> x
    Compose f g -> domain $ MorphismData $ f
    ProductM f g -> ObjectData $ Fix $ ProductO dom_f dom_g
      where
        (ObjectData dom_f) = domain $ MorphismData f
        (ObjectData dom_g) = domain $ MorphismData g
    UnitorL x -> unit >< x
    UnitorR x -> x >< unit
    UnunitorL x -> x
    UnunitorR x -> x
    Assoc x y z -> (x >< y) >< z
    Unassoc x y z -> x >< (y >< z)

  codomain (MorphismData (Fix m)) = case m of
    Id x -> x
    Compose f g -> codomain $ MorphismData $ g
    ProductM f g -> ObjectData $ Fix $ ProductO cod_f cod_g
      where
        (ObjectData cod_f) = codomain $ MorphismData f
        (ObjectData cod_g) = codomain $ MorphismData g
    UnitorL x -> x
    UnitorR x -> x
    UnunitorL x -> unit >< x
    UnunitorR x -> x >< unit
    Assoc x y z -> x >< (y >< z)
    Unassoc x y z -> (x >< y) >< z

{-
valid (Fix (Compose f g)) = (valid f) && (valid g) && (codomain f == domain g)
valid (Fix (ProductM f g)) = (valid f) && (valid g)
valid _ = True
-}


data BraidedMorphismUnfixed a m =
    BraidedMor (MorphismUnfixed a m)
  | Braid (ObjectData a) (ObjectData a)
type BraidedMorphism a = Fix (BraidedMorphismUnfixed a)

data CartesianMorphismUnfixed a m =
    CartesianMor (MorphismUnfixed a m)
  | Diag (ObjectData a)
  | Aug (ObjectData a)
type CartesianMorphism a = Fix (CartesianMorphismUnfixed a)

data CartesianClosedMorphismUnfixed a m =
    CartesianClosedMor (CartesianMorphismUnfixed a m)
  | Eval (ObjectData a) (ObjectData a)
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

newtype CompactClosedObjectData a =
  CompactClosedObjectData (Fix (CompactClosedObjectUnfixed a))

instance Pairable (CompactClosedObjectData a) where
  (CompactClosedObjectData x) >< (CompactClosedObjectData y) =
    CompactClosedObjectData $ Fix $ CompactClosedOb $ ProductO x y

{-
instance Destructable (CompactClosedObjectData a) where
  up (CompactClosedObjectData (Fix x)) = case x of
    CompactClosedOb x -> fmap (\y -> CompactClosedOb $ Fix y) $ up x
    _ -> Nothing
-}

data CompactClosedMorphismUnfixed a m =
    CompactClosedMor (SymmetricMorphismUnfixed a m)
  | Ev (CompactClosedObjectData a)
  | Unev (CompactClosedObjectData a)
type CompactClosedMorphism a = Fix (CompactClosedMorphismUnfixed a)
