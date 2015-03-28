{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Checkpoint where
import Control.Lens

import Segment
import Geometry
import Linear
import Time

data Checkpoint = Checkpoint { _position :: Position FloatType, _heading :: Heading FloatType }
$(makeLenses ''Checkpoint)

-- Apply a direction change (or stop one) and calculate next checkpoint
apply :: Maybe Direction -> DTime -> Checkpoint -> Checkpoint
apply d = maybe extrapolate turn d

-- Calculate next checkpoint after turn
turn :: Direction -> DTime -> Checkpoint -> Checkpoint
turn d dt cp = 
    position +~ tanpt (cp ^. heading) radius omega $
    heading %~ (`Geometry.rotate` omega) $ cp
    where
    omega = rads d dt

-- Calculate next straight-line checkpoint
extrapolate :: DTime -> Checkpoint -> Checkpoint
extrapolate dt cp = position +~ displacement $ cp
    where displacement = cp ^. heading. to (^* dt)
