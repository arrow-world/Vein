module Vein.Signal.Types where

import qualified Vein.Core.Lambda.Types as LT
import qualified Vein.Core.Module as M
import qualified Vein.Core.Monoidal.Types as MT

import Numeric.Natural (Natural)
import qualified Data.Text as T
import qualified Data.Map.Lazy as Map

tBehavior :: MT.Type (LT.Type M.QN)
tBehavior = MT.Type { MT.paramTypes = [LT.Univ 0] }

tEvent :: MT.Type (LT.Type M.QN)
tEvent = MT.Type { MT.paramTypes = [LT.Univ 0] }

tBandlimitedAnalog :: MT.Type (LT.Type M.QN)
tBandlimitedAnalog =
  MT.Type { MT.paramTypes = [LT.Type $ M.readQN $ T.pack "Signal.Analog.Frequency"] }

mod =
  M.ModuleMap $ Map.fromList $ map (\(s,t) -> (M.readQN $ T.pack s, t))
    [ ("Signal.Behavior", tBehavior)
    , ("Signal.Event", tEvent)
    , ("Signal.Analog.Bandlimited", tBandlimitedAnalog)
    ]
