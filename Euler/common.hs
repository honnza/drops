module Common where
import Data.List
import Data.Numbers.Primes

fib :: [Integer]
fib = 1 : 2 : zipWith (+) fib (tail fib)

divisorSum :: Integer -> Integer
divisorSum x = product $ map factor $ group $ primeFactors x
  where factor :: [Integer] -> Integer
        factor ps@(p:_) = (1 - p ^ (length ps+1)) `div` (1 - p)

say :: Int -> String
say x = let digitWords = words "zero one two three four five six seven eight nine"
            tensWords = words "zero ten twenty thirty forty fifty sixty seventy eighty ninety"
            teenWords = words "ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen"

            units = x `mod` 10
            tens = x `div` 10 `mod` 10
            hundreds = x `div` 100 `mod` 10
            thousands = x `div` 1000 `mod` 10
            
            strUnits = case () of _
                                       | tens == 1 -> teenWords !! units
                                       | tens == 0 -> digitWords !! units
                                       | units == 0 -> tensWords !! tens
                                       | otherwise -> say(tens * 10) ++ "-" ++ say(units)
            strHundreds = case () of _
                                       | hundreds == 0 -> digitWords !! thousands ++ " thousand"
                                       | thousands == 0 -> digitWords !! hundreds ++ " hundred"
                                       | otherwise -> say (thousands * 1000) ++ " " ++ say(hundreds * 100)
        in  case () of _
                                       | x < 100 -> strUnits
                                       | x `mod` 100 == 0 -> strHundreds
                                       | otherwise -> strHundreds ++ " and " ++ strUnits

parseSSV :: Read r => String -> [[r]]
parseSSV str = map (map read) $ map words $ lines str