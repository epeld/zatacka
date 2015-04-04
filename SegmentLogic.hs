module SegmentLogic where
import Data.Monoid
import Control.Lens
import Control.Arrow
import Control.Monad

import Direction
import Segment
import SegmentHelpers
import Checkpoint
import ListUtils
import Time

checkpoints :: [Segment] -> [Time] -> Checkpoint -> [Checkpoint]
checkpoints segs ts = sequence $ fmap (checkpoint segs) ts

-- Compute checkpoint at time 't', given a list of segments and a starting position
checkpoint :: [Segment] -> Time -> Checkpoint -> Checkpoint
checkpoint segs t = fconcat endos
    where endos = fmap endo $ cut segs t

-- Cut off the segments that happen after a given time
-- Might split the final segment in half if it is still ongoing when the cut happens
cut :: [Segment] -> Time -> [Segment]
cut segs t = uncurry SegmentA `fmap` zip (changes segs) dts
    where dts = takeWhile (> 0) (segs `clamped` t)

--
-- Recompute delta times for segments, given that 't' is the final time allowed
--
clamped :: [Segment] -> Time -> [DTime]
clamped segs t = ends' segs `vecminus` starts' segs
    where starts' = vecmin t. starts
          ends' = vecmin t. ends

endo :: Segment -> Checkpoint -> Checkpoint
endo = uncurry next . (view change &&& view duration)
