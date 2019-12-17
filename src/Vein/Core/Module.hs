module Vein.Core.Module where

import qualified Data.Text as T
import qualified Data.Map.Lazy as Map

data Name = Name T.Text
  deriving (Eq, Show, Ord)

data Named a = Named { named :: a , name :: Name }
  deriving (Eq, Show)

data QN = QN [Name]
  deriving (Eq, Show, Ord)

readQN :: T.Text -> QN
readQN = QN . (map Name) . T.splitOn (T.pack ".")

showQN :: QN -> T.Text
showQN (QN names) = T.intercalate (T.pack ".") $ fmap (\(Name t) -> t) names

textsToQN :: [T.Text] -> QN
textsToQN = QN . (map Name)

type ModuleMap a = Map.Map QN a