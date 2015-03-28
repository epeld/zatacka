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

import ListUtils

data SegmentA a = Segment { _content :: a, _duration :: DTime }
$(makeLenses ''SegmentA)

type Segment = SegmentA DirectionChange

change :: Lens Segment (SegmentA b) DirectionChange b
change = content

data Direction = Left | Right deriving (Show, Eq)

type DirectionChange = Maybe Direction


--
--  Helpers
--

radius = 1 -- Turn radius

sign :: Num a => Direction -> a
sign Right = 1
sign Left = -1

rads :: Direction -> DTime -> FloatType
rads d dt = Segment.sign d * dt * angularSpeed

speed :: Num a => a
speed = 1

angularSpeed :: Floating a => a
angularSpeed = pi / 8

