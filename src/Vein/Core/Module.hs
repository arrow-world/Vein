{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vein.Core.Module where

import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HashMap

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.State.Lazy (StateT(runStateT))

data Name = Name T.Text
  deriving (Eq, Show, Generic)

instance Hashable Name


data Named a = Named { named :: a , name :: Name }
  deriving (Eq, Show, Generic, Functor)

instance Hashable a => Hashable (Named a)


data QN = QN [Name]
  deriving (Eq, Show, Generic)

instance Hashable QN


readQN :: T.Text -> QN
readQN = QN . (map Name) . T.splitOn (T.pack ".")

showQN :: QN -> T.Text
showQN (QN names) = T.intercalate (T.pack ".") $ fmap (\(Name t) -> t) names

textsToQN :: [T.Text] -> QN
textsToQN = QN . (map Name)


type ModuleMap a = HashMap.HashMap QN a

newtype ModuleStateT a m b = ModuleStateT (StateT (ModuleMap a) m b)
  deriving (Functor,Applicative,Monad,MonadTrans)

runMonadStateT :: ModuleStateT a m b -> ModuleMap a -> m (b , ModuleMap a)
runMonadStateT (ModuleStateT s) = runStateT s