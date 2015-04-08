{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Player where
import Control.Lens

import TimeSeries
import TimeSeriesGeometry
import Checkpoint
import Direction

data Player = Player { _timeseries :: TimeSeries (Maybe Turn), _initial :: Checkpoint } 
makeLenses ''Player

happened :: TurnEvent -> Player -> Player
happened trn = over timeseries (insert trn)

player p h = Player (TimeSeries []) (Checkpoint p h)
