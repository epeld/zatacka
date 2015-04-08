module Range where

inclusive :: (Ord a, Num a) => a -> a -> a -> [a]
inclusive s e 0 = [s, e] -- Avoid inifinite loops
inclusive s e ic = (takeWhile (< e) $ iterate (ic +) s) ++ [e]
