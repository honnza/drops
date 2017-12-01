import Data.Char

data Part = PartA | PartB

day01 :: Part -> String -> Int
day01 part input
  = sum $ map (digitToInt.fst) $ filter (uncurry (==)) pairs
  where pairs = case part of PartA -> zip digits $ tail digits ++ [head digits]
                             PartB -> zip digits $ drop (length digits `div` 2) digits ++ digits
        digits = filter isDigit input

main = print . day01 PartB =<< readFile "day01in.txt"