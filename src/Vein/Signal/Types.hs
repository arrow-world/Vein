module Vein.Signal.Types where

import qualified Vein.Core.Lambda.Types as LT
import qualified Vein.Core.Module as M

import Numeric.Natural (Natural)
import qualified Data.Text as T
import qualified Data.Map.Lazy as Map

tBehavior = LT.NewType { LT.paramTypes = [LT.Univ 0] }

tEvent = LT.NewType { LT.paramTypes = [LT.Univ 0] }

tBandlimitedAnalog =
  LT.NewType { LT.paramTypes = [LT.Type $ M.readQN $ T.pack "Signal.Analog.Frequency"] }

mod =
  Map.fromList $ map (\(s,t) -> (M.readQN $ T.pack s, t))
    [ ("Signal.Behavior", tBehavior)
    , ("Signal.Event", tEvent)
    , ("Signal.Analog.Bandlimited", tBandlimitedAnalog)
    ]
