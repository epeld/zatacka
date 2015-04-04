{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Worm where
import qualified Prelude as P
import Prelude ((==), mod, (++), error)

import Control.Lens hiding (_head)

import Data.Function
import Data.Maybe
import qualified Data.List as List
import Data.List.NonEmpty as NonEmpty

import Text.Show

import Time
import Transform
import Display
import Graphics
import Input
import Segment
import SegmentJoin
import SegmentHelpers as SH
import SegmentLogic as SL
import Checkpoint
import Direction
import NonEmptyUtil

instance P.Show (SegmentA DirectionChange) where
    show (SegmentA ch dt) = P.unwords ["SegmentA", show ch, show dt]

data Worm = Worm { _segments :: NonEmpty Segment, _initial :: Checkpoint } deriving Show
$(makeLenses ''Worm)

segments' :: Getting [Segment] Worm [Segment]
segments' = segments . to toList

worm :: Checkpoint -> Worm
worm cp = Worm { _segments = s :| [], _initial = cp }
    where s = SegmentA { _content = Nothing, _duration = 0 }


checkpoints :: DTime -> Worm -> [Checkpoint]
checkpoints 0 _ = error "Must specify non-zero rendering interval"
checkpoints dt w = SL.checkpoints (w ^. segments') ts (w ^. initial)
    where ts = [0, dt .. t] ++ if ceiling (t - t / dt) then [] else [t]
          t = endTime w

endTime :: Worm -> Time
endTime w = List.last $ 0 : SH.ends (w ^. segments')

extend :: DTime -> Maybe DirectionChange -> Worm -> Worm
extend dt = Worm.nochange dt `maybe` Worm.change dt


change :: DTime -> DirectionChange -> Worm -> Worm
change dt chg = segments %~ joined (SegmentA chg dt :| [])


nochange :: DTime -> Worm -> Worm
nochange dt = segments . _head . duration +~ dt
