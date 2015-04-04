module SegmentHelpers where
import Control.Lens

import Direction
import Segment
import ListUtils
import Time

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
contents :: [SegmentA a] -> [a]
contents = fmap (^. content)

changes :: [Segment] -> [Maybe Direction]
changes = contents

dts :: Functor f => f Segment -> f DTime
dts = fmap (^. duration)
