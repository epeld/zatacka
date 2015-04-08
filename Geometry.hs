{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Geometry where
import Prelude hiding (length)
import Linear hiding (rotate)

import Control.Lens
import Control.Applicative
import Control.Monad.Trans.State.Strict

import Data.Maybe

import Checkpoint
import TimeSeries
import Direction

import qualified LinearUtils as LU

-- length describes either line length or unit arc length = (radians) depending on shape
data Geometry = Geometry { _checkpoint :: Checkpoint, _shape :: Shape, _length :: FloatType} deriving (Show, Eq)

data Shape = Line | Arc deriving (Show, Eq)

makeLenses ''Geometry
makeLenses ''Shape

--
-- Vectors, 0 meaning start and 1 meaning end
--

courseCorrection a = a ^. shape . to courseCorrection'
    where
    courseCorrection' Arc = Just $ a ^. length
    courseCorrection' Line = Nothing

heading0 :: Lens' Geometry Heading
heading0 = checkpoint . Checkpoint.heading

heading1 = to heading1'
heading1' a = maybe h (\corr -> rotate (-corr) h) (courseCorrection a)
    where h = a ^. heading0

position0 :: Lens' Geometry Position
position0 = checkpoint . Checkpoint.position

position1 = to position1'
position1' a = case a ^. shape of
    Arc -> a ^. position0 + cp - (LU.rotation (- a ^. length)) !* cp
    Line -> a ^. position0 + a ^. heading0 ^* a ^. length
    where
    cp = centre a .? zero

checkpoint0 = checkpoint
checkpoint1 = to checkpoint1'
checkpoint1' g = g ^. checkpoint & position .~ (g ^. position1) & heading .~ (g ^. heading1)

--
-- Arc center-points
--

centre :: Geometry -> Maybe Position
centre x = centre' h <$> courseCorrection x
    where h = x ^. heading0

centre' :: Heading -> FloatType -> Position
centre' h omga = -1 * signum omga *^ LU.uniperp h





-- Rotate a radians counter-clockwise
rotate :: FloatType -> V2 FloatType -> V2 FloatType
rotate a v = LU.rotation a !* v

(.?) :: Maybe a -> a -> a
(.?) = flip fromMaybe
infixl 6 .? 
