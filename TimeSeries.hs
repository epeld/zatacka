{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction, StandaloneDeriving #-}
module TimeSeries where
import Control.Applicative
import Control.Lens

import Time

data Event a = Event { _content :: a, _duration :: DTime }
makeLenses ''Event

deriving instance Eq a => Eq (Event a)

data TimeSeries a = TimeSeries { _events :: [Event a] }
makeLenses ''TimeSeries

instance Show a => Show (Event a) where
    show (Event a dt) = "Event " ++ show a ++ " " ++ show dt

-- Extend the last event by a given amount
extend :: DTime -> TimeSeries a -> TimeSeries a
extend dt (TimeSeries []) = TimeSeries []
extend dt (TimeSeries (x : xs)) = TimeSeries evs
    where
    evs = (x & duration +~ dt) : xs

-- Insert a new event - merging with the previous if they have the same content
insert :: Eq a => Event a -> TimeSeries a -> TimeSeries a
insert ev ts@(TimeSeries evs) = 
    if take 1 evs == [ev]
    then extend (ev ^. duration) ts 
    else TimeSeries (ev : evs)


-- Total duration of time series
total :: TimeSeries a -> DTime
total s = sum (view duration <$> s ^. events)
