{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vein.Util.Counter where

import Numeric.Natural ( Natural )
import Control.Monad.State ( StateT , modify , get , runState )
import Data.Functor.Identity (Identity(..))

newtype CounterT m a = CounterT (StateT Natural m a)
  deriving (Functor, Applicative, Monad)

newtype Counter a = Counter (CounterT Identity a)
  deriving (Functor, Applicative, Monad)

count :: Monad m => CounterT m Natural
count = CounterT $ get <* modify (+1)

runCounter :: CounterT Identity a -> (a , Natural)
runCounter (CounterT m) = runState m 0
