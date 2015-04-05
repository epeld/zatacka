module TimeSeriesGeometry where
import Linear
import Control.Lens
import Control.Monad.Trans.State.Strict

import Geometry
import TimeSeries
import Direction
import qualified Checkpoint as CP

import qualified LinearUtils as LU

type Turn = Direction
type DirectionChanges = TimeSeries (Maybe Turn)
type TurnEvent = Event (Maybe Turn)
type Geometry = GeometryA FloatType


geometriesS :: [TurnEvent] -> State Checkpoint [Geometry]
geometriesS = sequence . fmap geometryS


geometryS :: TurnEvent -> State Checkpoint Geometry
geometryS ev = do
    cp <- get
    let geom = g (ev ^. content)
        g Nothing = line cp (ev ^. duration)
        g (Just d) = arc cp (omegaDiff d $ ev ^. duration)
    position .= geom ^. end
    heading .= LU.rotation (correction geom) !* (cp ^. heading)
    return geom

geometry ev (CP.Checkpoint p h) = g (ev ^. content)
    where
    dt = ev ^. duration
    g Nothing = Line { _start = p, _length = 1 * dt, _heading = h }
    g (Just d) = Arc { _start = p, _omega = omegaDiff d dt }


omegaDiff Direction.Left dt = -dt
omegaDiff Direction.Right dt = dt
