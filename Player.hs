{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Player where
import Control.Lens

import Time
import TimeSeries
import TimeSeriesGeometry
import Checkpoint
import Direction

data Player = Player { _timeseries :: TimeSeries (Maybe Turn), _initial :: Checkpoint } 
makeLenses ''Player

change :: DTime -> Maybe TurnEvent -> Player -> Player
change dt = maybe (Player.extend dt) happened

happened :: TurnEvent -> Player -> Player
happened trn = over timeseries (insert trn)

-- Extend the player's time series by the amount dt
extend :: DTime -> Player -> Player
extend dt = over timeseries (TimeSeries.extend dt)

player p h = Player (TimeSeries [Event Nothing 0]) (Checkpoint p h)
