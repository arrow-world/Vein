module Vein.Core.Module where

import qualified Data.Text as T
import Numeric.Natural (Natural)
import qualified Data.Map.Lazy as Map

data Name = Name T.Text
  deriving (Eq, Show, Ord)

data Named a = Named { named :: a , name :: Name }

data QN = QN [Name]
  deriving (Eq, Show, Ord)

readQN :: T.Text -> QN
readQN = QN . (map Name) . T.splitOn (T.pack ".")

textsToQN :: [T.Text] -> QN
textsToQN = QN . (map Name)

data ModuleMap a = ModuleMap (Map.Map QN a)

union :: ModuleMap a -> ModuleMap a -> ModuleMap a
union (ModuleMap m0) (ModuleMap m1) = ModuleMap $ Map.union m0 m1