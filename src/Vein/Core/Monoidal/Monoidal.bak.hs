{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Vein.Core.Monoidal.Monoidal where

import GHC.Exts (Constraint)

class Monoidal hom (ob_c :: * -> Constraint) p u | hom -> ob_c p u where
  id :: ob_c a => hom a a
  (>>>) :: (ob_c a, ob_c b, ob_c c) => hom a b -> hom b c -> hom a c

  (**) :: (ob_c a, ob_c b, ob_c a', ob_c b') =>
    hom a b -> hom a' b' -> hom (p a a') (p b b')

  unitorL :: (ob_c a) => hom (p u a) a
  unitorR :: (ob_c a) => hom (p a u) a

  ununitorL :: (ob_c a) => hom a (p u a)
  ununitorR :: (ob_c a) => hom a (p a u)

  assoc :: (ob_c a, ob_c b, ob_c c) => hom (p (p a b) c) (p a (p b c))
  unassoc :: (ob_c a, ob_c b, ob_c c) => hom (p a (p b c)) (p (p a b) c)

class Monoidal hom ob_c p u => Braided hom ob_c p u where
  braid :: (ob_c a, ob_c b) => hom (p a b) (p b a)

class Monoidal hom ob_c p u => Cartesian hom ob_c p u where
  diag :: (ob_c a) => hom a (p a a)
  aug :: (ob_c a) => hom a u

class Cartesian hom ob_c p u => CartesianClosed hom inner_hom ob_c p u where
  eval :: hom (p (inner_hom a b) a) b

class Monoidal hom ob_c p u => Symmetric hom ob_c p u
