module Control where
import Worm

changes :: [Input] -> [DirectionChange]
changes input = []

-- TODO write lookup from key to Change

control :: Transform Worm
control dt input = process dt (changes input)

process dt [] = segments . head . duration +~ dt
process dt (x : _) = process' x -- events appear so that first element happened last

process' :: DTime -> Event -> Worm -> Worm
process' dt ch = segments `over` join [Segment ch dt]

--
-- Helpers
--

head :: Lens [a] [a] a a
head _ [] = []
head f (x:xs) = (f x) : xs
