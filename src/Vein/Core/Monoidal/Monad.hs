module Vein.Core.Monoidal.Monad where

import Vein.Core.Monoidal.Monoidal ( Object (Object, Unit, ProductO)
                                   , WithInternalHom
                                   , WI (..)
                                   , Braided (..)
                                   , CartesianClosed (..)
                                   , Cartesian (..)
                                   , docoMorphismF
                                   , docoBraided
                                   , docoCartesian
                                   , CartesianClosedBraidedCartesianMorphism
                                   , CartesianClosedBraidedCartesianMorphismF (..)
                                   , docoCartesianClosedBraidedCartesianMorphism
                                   , lenOfOb
                                   , MorphismF  ( Id
                                                , Compose
                                                , ProductM
                                                , UnitorL
                                                , UnitorR
                                                , UnunitorL
                                                , UnunitorR
                                                , Assoc
                                                , Unassoc
                                                , Morphism
                                                )
                                   )
import Control.Monad ( (>=>) )
import Data.Fix

assignBraided ::  (Monad f, Monad g) =>
                        (m -> f ([a] -> g [a]))
                    ->  (m -> f (Object o, Object o))
                    ->  Braided o m
                    ->  f ([a] -> g [a])
assignBraided assignM doco m =
  case m of
    Braided m' -> assignM m'
    Braid _ _ -> return $ \[x,y] -> return [y,x]

assignCartesian ::  (Monad f, Monad g) =>
                          (m -> f ([a] -> g [a]))
                      ->  (m -> f (Object o, Object o))
                      ->  Cartesian o m
                      ->  f ([a] -> g [a])
assignCartesian f doco m =
  case m of
    Cartesian m' -> f m'
    Diag _ -> return $ \[x] -> return [x,x]
    Aug _ -> return $ \[x] -> return []

assignCartesianClosed ::  (Monad f, Monad g, WithInternalHom o) =>
                              (m -> f ([a] -> g [a]))
                          ->  (m -> f (Object o, Object o))
                          ->  CartesianClosed m o
                          ->  f ([a] -> g [a])
assignCartesianClosed f doco m =
  case m of
    CartesianClosed m' -> f m'

assignMorphismF ::  (Monad f, Monad g) =>
                          (m -> f ([a] -> g [a]))
                      ->  (m -> f (Object o, Object o))
                      ->  (r -> f ([a] -> g [a]))
                      ->  (r -> f (Object o, Object o))
                      ->  MorphismF m o r
                      ->  f ([a] -> g [a])
assignMorphismF assignM docoM assignR docoR m =
  case m of
    Compose m1 m2 ->
      do
        m1' <- assignR m1
        m2' <- assignR m2
        return $ m1' >=> m2'

    ProductM m1 m2 ->
      do
        m1' <- assignR m1
        m2' <- assignR m2
        nofInputs_m1 <- nofInputs m1
        let splitInputs = splitAt nofInputs_m1

        return $ \xs -> 
          let (ys,zs) = splitInputs xs in
            (++) <$> m1' ys <*> m2' zs
      where
        nofInputs m' = do
          (dom, _) <- docoR m'
          pure $ lenOfOb dom

    Morphism m' -> assignM m'
    Id _ -> id'
    UnitorL x -> id'
    UnitorR x -> id'
    UnunitorL _ -> id'
    UnunitorR _ -> id'
    Assoc _ _ _ -> id'
    Unassoc _ _ _ -> id'
  where
    id' = return $ return

assignCartesianClosedBraidedCartesianMorphism ::  (Monad f, Monad g) =>
                                                        (m -> f ([a] -> g [a]))
                                                    ->  (m -> f (Object (WI o), Object (WI o)))
                                                    ->  CartesianClosedBraidedCartesianMorphism m o
                                                    ->  f ([a] -> g [a])
assignCartesianClosedBraidedCartesianMorphism assignM docoM (Fix (CartesianClosedBraidedCartesianMorphismF m)) =
    (assignCartesianClosed
      (assignCartesian
        (assignBraided
          (assignMorphismF assignM docoM
            (assignCartesianClosedBraidedCartesianMorphism assignM docoM)
            (docoCartesianClosedBraidedCartesianMorphism docoM)
          )
          docoMorphismF'
        )
        docoBraided'
      )
      docoCartesian'
    ) m
  where
    docoMorphismF' = docoMorphismF docoM (docoCartesianClosedBraidedCartesianMorphism docoM)
    docoBraided' = docoBraided docoMorphismF'
    docoCartesian' = docoCartesian docoBraided'