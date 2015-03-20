module Segment where
import Data.Monoid

import Geometry
import Time

data Direction = Left | Right deriving (Show, Eq)

type Segment = (Maybe Direction, DTime)
type Checkpoint a = (Position a, Heading a)
type CPEndo a = Checkpoint a -> Checkpoint a

-- Compute checkpoint at time 't', given a list of segments and a starting position
checkpoint :: [Segment] -> Time -> CPEndo a
checkpoint segs t = appEndo $ mconcat $ endo `map` cut segs t

--
--  Time Calculations
--

-- Cut off the segments that happen after a given time
-- Might split the final segment in half if it is still ongoing when the cut happens
cut :: [Segment] -> Time -> [Segment]
cut segs t = zip (dirs segs) dts
    where dts = takeWhile (> 0) (segs `clamped` t)

--
-- Recompute delta times for segments, given that 't' is the final time allowed
--
clamped :: [Segment] -> [DTime]
clamped segs t = ends' segs `vecminus` starts' segs
    where starts' = vecmin t. starts
          ends' = vecmin t. ends


--
-- Checkpoint Calculations
--

endo :: Segment -> CPEndo a
endo = Endo. uncurry apply

-- Apply a direction change (or stop one) and calculate next checkpoint
apply :: Maybe Direction -> DTime -> CPEndo a
apply d = extrapolate `maybe` turn d

-- Calculate next checkpoint after turn
turn :: Direction -> DTime -> CPEndo a
turn d dt = let omega = rads d dt in
    \(p, h) -> 
        (p + tanpt h r omega, rotation omega !* h)

-- Calculate next straight-line checkpoint
extrapolate :: DTime -> CPEndo a
extrapolate dt = first (+ dt * speed)

--
-- Getters
--

-- extract start times from segments
starts :: [Segment] -> [Time]
starts = dropLast 1. scanl (+) 0. dts

-- extract end times (= start time of previous segment)
ends :: [Segment] -> [Time]
ends = drop 1. starts

-- extract diretions from segments
dirs :: [Segment] -> [Maybe Direction]
dirs = map fst

dts :: [Segment] -> [DTime]
dts = map snd

--
--  Helpers
--

-- vector minus
vecminus :: Num a => [a] -> [a] -> [a]
vecminus = zipWith (-)

-- element-wise min
vecmin x = map (min x)


sign :: Num a => Direction -> a
sign Right = 1
sign Left = -1

rads :: Num a => Direction -> DTime -> a
rads d dt = sign d * dt * angularSpeed

speed = 1
angularSpeed = pi / 8

dropLast :: Int -> [a] -> [a]
dropLast n = map snd. zip (drop n)

