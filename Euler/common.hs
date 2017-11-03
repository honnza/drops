module Common where

fib :: [Integer]
fib = 1 : 2 : zipWith (+) fib (tail fib)

parseSSV :: Read r => String -> [[r]]
parseSSV str = map (map read) $ map words $ lines str