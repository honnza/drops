e01 = print $ sum [x | x <- [0..999], x `mod` 3 == 0 || x `mod` 5 == 0]

e02 = print $ sum [x | x <- takeWhile (< 4000000) fib, even x]
  where fib = 1 : 2 : zipWith (+) fib (tail fib) :: [Integer]

main = e02