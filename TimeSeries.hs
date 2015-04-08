{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module TimeSeries where
import Control.Applicative
import Control.Lens

import Time

data Event a = Event { _content :: a, _duration :: DTime }
makeLenses ''Event

data TimeSeries a = TimeSeries { _events :: [Event a] }
makeLenses ''TimeSeries

instance Show a => Show (Event a) where
    show (Event a dt) = "Event " ++ show a ++ " " ++ show dt

insert :: Eq a => Event a -> TimeSeries a -> TimeSeries a
insert ev = over events (insert' ev)

insert' :: Eq a => Event a -> [Event a] -> [Event a]
insert' ev [] = [ev]
insert' ev evs@(x : xs) = 
    if x ^. content == ev ^. content
    then (x & duration +~ ev ^.duration) : evs -- evs & _head . duration +~ ev ^. duration 
    else ev : evs


total :: TimeSeries a -> DTime
total s = sum (view duration <$> s ^. events)
