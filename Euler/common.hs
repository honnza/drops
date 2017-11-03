module Common where

fib = 1 : 2 : zipWith (+) fib (tail fib) :: [Integer]