import Data.Char
import Data.List
import Debug.Trace
import qualified Data.Array.Unboxed as DA
import qualified Data.HashMap.Strict as H

data Part = PartA | PartB deriving Eq

day01 :: Part -> String -> Int
day01 part input
  = sum $ map (digitToInt.fst) $ filter (uncurry (==)) pairs
  where pairs = case part of PartA -> zip digits $ tail digits ++ [head digits]
                             PartB -> zip digits $ drop (length digits `div` 2) digits ++ digits
        digits = filter isDigit input

day02 :: Part -> String -> Int
day02 part input
  = sum $ map (lineFn . map read . words) $ lines input
  where lineFn r = case part of PartA -> maximum r - minimum r
                                PartB -> head [n `div` d | n <- r, d <- r, n /= d, n `mod` d == 0]

day03 :: Part -> Int -> Int
day03 PartA input =
  let major_coord = ceiling $ fromIntegral input ** 0.5 * 0.5 - 0.5
      minor_diag  = ((2 * major_coord + 1) ^ 2 - input) `mod` (2 * major_coord)
      minor_coord = abs $ major_coord - minor_diag
  in  minor_coord + major_coord

day03 PartB input = iter (H.singleton (0, 0) 1) (0, 0)
  where iter :: H.HashMap (Int, Int) Int -> (Int, Int) -> Int
        iter hsh (x, y) | hsh H.! (x, y) > input = hsh H.! (x, y)
                        | otherwise = iter (nextHsh hsh $ nextCoord (x, y)) $ nextCoord (x, y)
        nextHsh hsh (nx, ny) = H.insert (nx, ny) (nextValue hsh (nx, ny)) hsh
        nextValue hsh (nx, ny) = sum [H.lookupDefault 0 (nx + dx, ny + dy) hsh
                                     | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
        nextCoord (x, y) | x ==  y && y < 0 = (x, y + 1)
                         | x == -y && y < 0 = (x - 1, y)
                         | abs x == y       = (x + 1, y)
                         | abs x < abs y    = (x + signum y, y)
                         | otherwise        = (x, y - signum x)

day04 :: Part -> String -> Int
day04 part input = length [() | l <- lines input, 
                                let ws = (case part of PartA -> id; PartB -> map sort) $ words l,
                                ws == nub ws]

day05 :: Part -> String -> Int
day05 part input = iter 0 0 $ DA.listArray (0, length inArray - 1) inArray
  where inArray = map read $ words input :: [Int]
        iter :: Int -> Int -> DA.Array Int Int -> Int
        iter t ix ary | not $ DA.inRange (DA.bounds ary) ix = t
        iter t ix ary = iter (t+1) (ix + ary DA.! ix) (ary DA.// [(ix, nextOffset $ ary DA.! ix)])
        nextOffset offset | offset >= 3 && part == PartB = offset - 1
                          | otherwise = offset + 1

day06 :: String -> (Int, Int)
day06 input = iter 0 (map read $ words input) H.empty
  where iter :: Int -> [Int] -> H.HashMap [Int] Int -> (Int, Int)
        iter t state seen | H.member state seen = (t, t - seen H.! state)
        iter t state seen = iter (t+1) (nextState state) (H.insert state t seen)
        nextState :: [Int] -> [Int]
        nextState state = 
          let stateSize = length state
              maxValue = maximum state
              Just maxIndex = elemIndex maxValue state
              in  [ (if index == maxIndex then 0 else el) 
                  + (maxValue `div` stateSize)
                  + (if (index - maxIndex - 1) `mod` stateSize < maxValue `mod` stateSize then 1 else 0)
                  | (el, index) <- zip state [0..]]

-- main = print . day05 PartB =<< readFile "day05in.txt"
-- main = print $ day03 PartB 325489
main = print $ day06 "4 10 4 1 8 4 9 14 5 1 14 15 0 15 3 5"
