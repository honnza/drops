import Data.Char

data Part = PartA | PartB

day01 :: Part -> String -> Int
day01 part input
  = sum $ map (digitToInt.fst) $ filter (uncurry (==)) pairs
  where pairs = case part of PartA -> zip digits $ tail digits ++ [head digits]
                             PartB -> zip digits $ drop (length digits `div` 2) digits ++ digits
        digits = filter isDigit input

day02:: Part -> String -> Int
day02 part input
  = sum $ map (lineFn . map read . words) $ lines input
  where lineFn r = case part of PartA -> maximum r - minimum r
                                PartB -> head [n `div` d | n <- r, d <- r, n /= d, n `mod` d == 0]

main = print . day02 PartB =<< readFile "day02in.txt"