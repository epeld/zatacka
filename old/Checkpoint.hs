{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Checkpoint where
import Control.Lens
import Control.Applicative

import Direction
import Linear
import Time hiding (FloatType)

type FloatType = Float
type Position = V2 FloatType
type Heading = V2 FloatType

data Checkpoint = Checkpoint { _position :: Position, _heading :: Heading } deriving (Show, Eq)
$(makeLenses ''Checkpoint)

_coords :: Lens' Position (FloatType, FloatType)
_coords = iso (\v -> (v ^. _x, v ^. _y)) (uncurry V2)

_pcoords :: Lens' Checkpoint (FloatType, FloatType)
_pcoords = position . _coords

_posx :: Lens' Checkpoint FloatType
_posx = _pcoords . _1

_posy :: Lens' Checkpoint FloatType
_posy = _pcoords . _2

_hcoords :: Lens' Checkpoint (FloatType, FloatType)
_hcoords = heading . _coords
