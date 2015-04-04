{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Checkpoint where
import Control.Lens
import Control.Applicative

import Segment
import Direction
import Geometry
import Linear
import Time hiding (FloatType)

type FloatType = Float

data Checkpoint = Checkpoint { _position :: Position FloatType, _heading :: Heading FloatType } deriving (Show, Eq)
$(makeLenses ''Checkpoint)

_coords :: Lens' (Position a) (a, a)
_coords = iso (\v -> (v ^. _x, v ^. _y)) (uncurry V2)

_pcoords :: Lens' Checkpoint (FloatType, FloatType)
_pcoords = position . _coords

_posx :: Lens' Checkpoint FloatType
_posx = _pcoords . _1

_posy :: Lens' Checkpoint FloatType
_posy = _pcoords . _2

_hcoords :: Lens' Checkpoint (FloatType, FloatType)
_hcoords = heading . _coords

-- Apply a direction change (or stop one) and calculate next checkpoint
next :: Maybe Direction -> DTime -> Checkpoint -> Checkpoint
next d = maybe extrapolate turn d

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
