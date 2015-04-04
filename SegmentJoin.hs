module SegmentJoin where
import Prelude hiding (Right, Left, join)

import Control.Applicative
import Control.Lens

import Data.Semigroup
import Data.List.NonEmpty as NonEmpty

import Segment
import ListUtils

joined :: Eq a => NonEmpty (SegmentA a) -> NonEmpty (SegmentA a) -> NonEmpty (SegmentA a)
joined as bs = fromList $ concat [NonEmpty.init as, toList mid, NonEmpty.tail bs]
      where mid = NonEmpty.last as `joined1` NonEmpty.head bs

joined1 :: Eq a => SegmentA a -> SegmentA a -> NonEmpty (SegmentA a)
joined1 a b = if a ^. content == b ^. content
              then (a & duration +~ (b ^. duration)) :| []
              else a :| [b]

