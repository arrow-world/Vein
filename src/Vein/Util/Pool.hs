{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vein.Util.Pool where

import Control.Monad.Reader (Reader, ask, asks, runReader, ReaderT, mapReaderT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State ( State , modify , get , runState , MonadFix )
import qualified Data.Map.Lazy as Map

newtype PoolT k v m a = PoolT (StateT (Map.Map k v) (ReaderT (k -> m v) m) a)
  deriving (Functor, Applicative, Monad)

pool :: (Ord k , Monad m) => k -> PoolT k v m v
pool k = PoolT $ do
  table <- get

  case Map.lookup k table of
    Just v -> return v

    Nothing -> do
      f <- lift ask
      v <- lift $ lift $ f k
      modify $ Map.insert k v
      return v