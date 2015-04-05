{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Geometry where
import Linear hiding (rotate)

import Control.Lens
import Control.Applicative
import Control.Monad.Trans.State.Strict

import Data.Maybe

import Checkpoint
import TimeSeries
import Direction

import qualified LinearUtils as LU

data Geometry = Geometry { _checkpoint :: Checkpoint, _correction :: Maybe FloatType } deriving (Show, Eq)
$(makeLenses ''Geometry)

--
-- Vectors, 0 meaning start and 1 meaning end
--

heading0 :: Lens' Geometry Heading
heading0 = checkpoint . Checkpoint.heading

heading1 = to heading1'
heading1' a = rotate ang (a ^. heading0)
    where ang = a ^. correction .? 0

position0 :: Lens' Geometry Position
position0 = checkpoint . Checkpoint.position

position1 = to position1'
position1' a = a ^. position0 + cp - correctionM a !* cp
    where cp = centre a .? zero

checkpoint0 = checkpoint
checkpoint1 = to checkpoint1'
checkpoint1' g = g ^. checkpoint & position .~ (g ^. position1) & heading .~ (g ^. heading1)

--
-- Arc center-points
--

centre :: Geometry -> Maybe Position
centre x = centre' h <$> x ^. correction
    where h = x ^. heading0

centre' :: Heading -> FloatType -> Position
centre' h omga = signum omga *^ LU.uniperp h


--
-- Rotation matrix to carry out a course correction
--

correctionM :: Geometry -> M22 FloatType
correctionM a = fmap LU.rotation (a ^. correction) .? identity


rotate :: FloatType -> V2 FloatType -> V2 FloatType
rotate a v = LU.rotation a !* v

(.?) :: Maybe a -> a -> a
(.?) = flip fromMaybe
infixl 6 .? 
