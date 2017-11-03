module Common where

fib :: [Integer]
fib = 1 : 2 : zipWith (+) fib (tail fib) :: [Integer]

parseSSV :: Read a => String -> [[a]]
parseSSV str = map (map read) $ map words $ lines str