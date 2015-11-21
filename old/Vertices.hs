module Vertices where
import Data.List

import Linear
import Control.Lens
import Control.Applicative

import Geometry
import Checkpoint
import Combinatorics


tuplify :: V2 a -> (a,a)
tuplify (V2 a b) = (a,b)

-- Produce the coordinates necessary to render a thick line between v1 and v2 (using e.g OpenGL Triangles)
-- The coordinates are arranged in 3-tuples describing each triangle
thickLine :: (Num a, Ord a) => a -> (a,a) -> (a,a) -> [((a, a), (a, a), (a,a))]
thickLine sz v1 v2 
    | aligned v1 v2 = triplets $ cartesian (extremes $ fst <$> vs) (extremes $ snd <$> vs) -- Note: this draws twice the number of triangles necessary..
    | otherwise = [(i, j, k) | i <- intersect vs b1, (j, k) <- pairs (intersect vs b2)]
    where
    vs = boxTrim $ concat [b1,b2]
    b1 = boxed sz v1
    b2 = boxed sz v2

--
-- Trim off points that don't touch at least one edge of the bounding box
boxTrim :: Ord a => [(a,a)] -> [(a,a)]
boxTrim pts = filter (\pt -> pt ^._1 `elem` extremes xs || pt ^._2 `elem` extremes ys) pts
    where
    xs = fmap (^. _1) pts
    ys = fmap (^. _2) pts


aligned :: Eq a => (a,a) -> (a,a) -> Bool
aligned v1@(x1,y1) v2@(x2,y2) = x1 == x2 || y1 == y2

