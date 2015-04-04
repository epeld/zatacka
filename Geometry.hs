{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Geometry where
import Linear
import Control.Lens
import Control.Monad.Trans.State.Strict

import TimeSeries
import Direction
import Checkpoint

import qualified LinearUtils as LU

data GeometryA a = Line { _start :: V2 a, _end :: V2 a } | Arc { _centre :: V2 a, _start :: V2 a, _end :: V2 a }
$(makeLenses ''GeometryA)

type Geometry = GeometryA FloatType

type Turn = Direction
type DirectionChanges = TimeSeries (Maybe Turn)
type TurnEvent = Event (Maybe Turn)

geometries :: [TurnEvent] -> State Checkpoint [Geometry]
geometries = sequence . fmap geometry

geometry :: TurnEvent -> State Checkpoint Geometry
geometry ev = do
    cp <- get
    let geom = g (ev ^. content)
        g Nothing = line cp (ev ^. duration)
        g (Just d) = arc cp (omega d $ ev ^. duration)
    position .= geom ^. end
    heading .= endHeading ev cp
    return geom

omega Direction.Left dt = -dt
omega Direction.Right dt = dt

endHeading ev cp = e (ev ^. content)
    where
    e Nothing = cp ^. heading
    e (Just d) = LU.rotation (omega d $ ev ^. duration) !* (cp ^. heading)

line cp d = Line { _start = cp ^. position, _end = cp ^. position + cp ^. heading ^* d }
arc cp omga = 
    Arc { _centre = c, _start = cp ^. position, _end = c - LU.rotation omga !* cv }
    where 
    cv = LU.centre (cp ^. heading) omga
    c = (cp ^. position) + c
