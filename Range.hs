module Range where

inclusive :: (Ord a, Num a) => a -> a -> a -> [a]
inclusive s e 0 = [s, e] -- Avoid inifinite loops because of 0 increments
inclusive s e ic = (takeWhile (< e) $ iterate (ic +) s) ++ [e]

-- inclusive 0 1 0 = [0, 1]
-- inclusive 0 1 0.5 = [0, 0.5, 1]
-- etc
