{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vein.Util.Allocator where

import Control.Monad.Reader (Reader, ask, asks, runReaderT, ReaderT, mapReaderT)
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State ( State , modify , get , runState , MonadFix )
import qualified Data.Map.Lazy as Map

newtype AllocatorT k v m a = AllocatorT (ReaderT (v -> m k) (StateT (Map.Map k v) m) a)
  deriving (Functor, Applicative, Monad)

runAllocatorT :: Monad m => (v -> m k) -> Map.Map k v -> AllocatorT k v m a -> m (a , Map.Map k v)
runAllocatorT f t (AllocatorT r) = runStateT (runReaderT r f) t

startAllocatorT :: Monad m => (v -> m k) -> AllocatorT k v m a -> m (a , Map.Map k v)
startAllocatorT f = runAllocatorT f Map.empty

allocateWith :: (v -> m k) -> AllocatorT k v m a -> StateT (Map.Map k v) m a
allocateWith f (AllocatorT r) = runReaderT r f

allocate :: (Ord k , Monad m) => v -> AllocatorT k v m k
allocate v = AllocatorT $ do
  f <- ask
  k <- lift $ lift $ f v
  modify $ Map.insert k v
  return k
