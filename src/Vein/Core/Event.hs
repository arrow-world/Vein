module Vein.Core.Event where

import Vein.Core.SimpleSF

data Event a = Event

instance Signal (Event a) where
