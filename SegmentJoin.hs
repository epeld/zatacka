module SegmentJoin where
import Prelude hiding (Right, Left, join)

import Control.Applicative
import Control.Lens

import Segment
import ListUtils

joined :: Eq a => [SegmentA a] -> [SegmentA a] -> [SegmentA a]
joined a [] = a
joined aas (b:bs) = concat [dropLast 1 aas, mid, bs]
      where mid = concat (joined' <$> takeLast 1 aas <*> [b])

joined' :: Eq a => SegmentA a -> SegmentA a -> [SegmentA a]
joined' a b = if (a ^. content) == (b ^. content)
              then [a & duration +~ (b ^. duration)]
              else [a, b]

