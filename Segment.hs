{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Segment where
import Prelude hiding (Right, Left)

import Control.Arrow
import Data.Monoid

import Linear
import Control.Lens

import Geometry
import Time

data Direction = Left | Right deriving (Show, Eq)

data Segment = Segment { _direction :: Maybe Direction, _duration :: DTime }
$(makeLenses ''Segment)

data Checkpoint = Checkpoint { _position :: Position FloatType, _heading :: Heading FloatType }
$(makeLenses ''Checkpoint)


-- Compute checkpoint at time 't', given a list of segments and a starting position
checkpoint :: [Segment] -> Time -> Checkpoint -> Checkpoint
checkpoint segs t = appEndo $ mconcat $ endo `map` cut segs t

--
--  Time Calculations
--

-- Cut off the segments that happen after a given time
-- Might split the final segment in half if it is still ongoing when the cut happens
cut :: [Segment] -> Time -> [Segment]
cut segs t = uncurry Segment `fmap` zip (dirs segs) dts
    where dts = takeWhile (> 0) (segs `clamped` t)

--
-- Recompute delta times for segments, given that 't' is the final time allowed
--
clamped :: [Segment] -> Time -> [DTime]
clamped segs t = ends' segs `vecminus` starts' segs
    where starts' = vecmin t. starts
          ends' = vecmin t. ends


--
-- Checkpoint Calculations
--

endo :: Segment -> Endo Checkpoint
endo s = Endo $ apply (s ^. direction) (s ^. duration)

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

--
-- Getters
--

sums :: Num a => [a] -> [a]
sums = scanl (+) 0

-- extract start times from segments
starts :: [Segment] -> [DTime]
starts = dropLast 1. sums. dts

-- extract end times (= start time of previous segment)
ends ::  [Segment] -> [DTime]
ends = drop 1. sums. dts

-- extract diretions from segments
dirs :: [Segment] -> [Maybe Direction]
dirs = fmap (^. direction)

dts :: [Segment] -> [DTime]
dts = fmap (^. duration)

--
--  Helpers
--

radius = 1 -- Turn radius

-- vector minus
vecminus :: Num a => [a] -> [a] -> [a]
vecminus = zipWith (-)

-- element-wise min
vecmin x = map (min x)


sign :: Num a => Direction -> a
sign Right = 1
sign Left = -1

rads :: Direction -> DTime -> FloatType
rads d dt = Segment.sign d * dt * angularSpeed

speed :: Num a => a
speed = 1

angularSpeed :: Floating a => a
angularSpeed = pi / 8

dropLast :: Int -> [a] -> [a]
dropLast n xs = map snd $ zip (drop n xs) xs

