module Vein.Core.Cherry where

data Cherry a =
        Fruit a
    |   Branch (Cherry a) (Cherry a)
    deriving (Eq, Show)

type CherryM a = Cherry (Maybe a)