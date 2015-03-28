module ListUtils where
import Data.Monoid

dropLast :: Int -> [a] -> [a]
dropLast n = reverse. drop n. reverse

takeLast :: Int -> [a] -> [a]
takeLast n = reverse. take n. reverse

-- vector minus
vecminus :: Num a => [a] -> [a] -> [a]
vecminus = zipWith (-)

-- element-wise min
vecmin x = map (min x)

fconcat :: [(a -> a)] -> a -> a
fconcat = appEndo . mconcat . fmap Endo
