module Combinatorics where
import Control.Lens

triplets :: [a] -> [(a,a,a)]
triplets (a : b : c : xs) = [(a,b,c)] ++ [(x,y,z) | x <- [a,b,c], (y,z) <- pairs xs] ++ [(x,y,z) | (x,y) <- pairs [a,b,c], z <- xs] ++ triplets xs
triplets _ = []

-- return the list of all unordered pairs
pairs :: [a] -> [(a,a)]
pairs (a : b : xs) = [(a,b)] ++ [(x,y) | x <- [a,b], y <- xs] ++ pairs xs
pairs _ = []


boxed :: Num a => a -> (a,a) -> [(a,a)]
boxed sz v = fmap (\pt -> pt & both *~ sz & _1 +~ v ^. _1 & _2 +~ v ^. _2) box

box :: Num a => [(a,a)]
box = cartesian [-1,1] [-1,1]

cartesian :: [a] -> [b] -> [(a,b)]
cartesian as bs = [(a,b) | a <- as, b <- bs]

extremes :: Ord t => [t] -> [t]
extremes xs = [minimum xs, maximum xs]
