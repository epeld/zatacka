module TimeSeriesGeometry where
import Linear
import Control.Lens
import Control.Applicative
import Control.Arrow
import Control.Monad.Trans.State.Strict

import Geometry
import TimeSeries
import Direction
import Checkpoint

import qualified LinearUtils as LU

type TurnEvent = Event (Maybe Turn)

geometries :: [TurnEvent] -> Checkpoint -> [Geometry]
geometries evs = evalState (geometriesS evs)

geometriesS :: [TurnEvent] -> State Checkpoint [Geometry]
geometriesS = sequence . fmap geometryS

geometryS :: TurnEvent -> State Checkpoint Geometry
geometryS ev = state $ geometry ev >>> id &&& view checkpoint1

geometry :: TurnEvent -> Checkpoint -> Geometry
geometry ev cp = 
    Geometry { 
        _checkpoint = cp,
        _correction = (*) <$> fmap sign d <*> pure dt}
    where 
    dt = ev ^. duration
    d = ev ^. content
