{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Geometry where
import Linear

import Control.Lens
import Control.Applicative
import Control.Monad.Trans.State.Strict

import Data.Maybe

import TimeSeries
import Direction

import qualified LinearUtils as LU

data GeometryA a = Line { _start :: V2 a, _length :: a, _heading :: V2 a } | Arc { _start :: V2 a, _omega :: a }
$(makeLenses ''GeometryA)

correction :: Num a => GeometryA a -> a
correction (Arc _ o) = o
correction _ = 0

correctionM :: Num a => GeometryA a -> M22 a
correctionM = LU.rotation . correction

heading0 :: (Eq a, Floating a) => GeometryA a -> V2 a
heading0 (Arc s o) = signum o *^ LU.uniperp (s - ce)
heading0 (Line _ _ h) = h

heading1 :: (Eq a, Floating a) => GeometryA a -> V2 a
heading1 a = LU.rotation (correction a) !* heading0 a

position0 :: GeometryA a -> a
position0 a = a ^. start

position1 x = position0 + cp - correctionM !* cp
    where cp = centre x .? zero

centre :: (Floating a, Eq a) => GeometryA a -> Maybe (V2 a)
centre x = pure (centre' h) <$> x ^? omega
    where h = heading0 x

centre' :: (Floating a, Eq a) => V2 a -> a -> V2 a
centre' h omga = signum omga *^ LU.uniperp h

(.?) :: Maybe a -> a -> a
(.?) = flip fromMaybe
infixl 6 .? 
