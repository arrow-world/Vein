{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vein.Util.Counter where

import Numeric.Natural ( Natural )
import Control.Monad.State ( State , modify , get , runState )

newtype Counter a = Counter (State Natural a)
  deriving (Functor, Applicative, Monad)

count :: Counter Natural
count = Counter $ get <* modify (+1)

runCounter :: Counter a -> (a , Natural)
runCounter (Counter m) = runState m 0
