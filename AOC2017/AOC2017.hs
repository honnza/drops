import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace
import Text.RegexPR
import Control.Applicative
import Text.ParserCombinators.ReadP
import qualified Data.HashMap.Lazy as HL
import qualified Data.Array.Unboxed as DA
import qualified Data.HashMap.Strict as H

data Part = PartA | PartB deriving Eq

parsePRGroups :: Int -> [(Int, String)] -> [Maybe String]
parsePRGroups n groups = [fmap snd $ find ((== i) . fst) groups | i <- [1 .. n]]

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

data Day07Node = Day07Node String Int [Day07Node] deriving (Eq, Show)
day07 :: Part -> String -> Either String Int
day07 part input = let nodesByName :: H.HashMap String Day07Node
                       nodesByName = H.fromList [(name, Day07Node name (read weightStr) children)
                                                | (_, groups) <- gmatchRegexPR "([a-z]*) \\((\\d+)\\)(?: -> ([a-z, ]+))?" input,
                                                  let [Just name, Just weightStr, maybeChildStr] = parsePRGroups 3 groups,
                                                  let children = map (nodesByName H.!) $ splitRegexPR ", " $ fromMaybe "" maybeChildStr]
                       [root] = HL.elems nodesByName \\ [child | Day07Node _ _ children <- H.elems nodesByName, child <- children]
                       treeWeight :: Day07Node -> Int
                       treeWeight (Day07Node _ weight children) = weight + sum (map treeWeight children)
                       unbalancedNodes = [(node, correctWeight - wrongWeight + correctNodeWeight)
                                         | node@(Day07Node name weight children) <- H.elems nodesByName,
                                           let childWeights = map treeWeight children,
                                           (>= 2) $ length $ nub childWeights,
                                           let ([correctWeight:_], [[wrongWeight]]) = partition ((>1).length) $ group $ sort childWeights,
                                           let [correctNodeWeight] = [weight | node@(Day07Node _ weight _) <- children, treeWeight node == wrongWeight]
                                         ]
                       [fixedWeight] = [fixedWeight 
                                       | (Day07Node _ _ children, fixedWeight) <- unbalancedNodes,
                                         null $ intersect children $ map fst unbalancedNodes]
                   in  case part of PartA -> case root of Day07Node name _ _ -> Left name
                                    PartB -> Right fixedWeight

day08 :: String -> (Int, Int)
day08 input = case foldl' iter (0, H.empty) (lines input) 
              of   (submax, regs) -> (maximum $ H.elems regs, submax)
  where iter :: (Int, H.HashMap String Int) -> String -> (Int, H.HashMap String Int)
        iter (submax, regs) line =
          let Just (_, groups) = matchRegexPR "([a-z]+) (inc|dec) (-?\\d+) if ([a-z]+) (>|>=|==|!=|<|<=) (-?\\d+)" line
              [Just regWr, Just opWr, Just nrWr, Just regCond, Just opCond, Just nrCond] = parsePRGroups 6 groups
              newHash = if testCond opCond (H.lookupDefault 0 regCond regs) (read nrCond) 
                        then H.insert regWr (fn opWr (H.lookupDefault 0 regWr regs) (read nrWr)) regs
                        else regs
              fn :: String -> Int -> Int -> Int; fn "dec" = (-); fn "inc" = (+)
              testCond "<" = (<); testCond "<=" = (<=); testCond "==" = (==)
              testCond "!=" = (/=); testCond ">" = (>); testCond ">=" = (>=)
              
          in  (maximum $ submax : H.elems newHash, newHash)

data Day09Node = Day09Stream [Day09Node] | Day09Garbage Int deriving Show
  
day09 :: String -> (Int, Int)
day09 input =
  let parseNode :: ReadP Day09Node
      parseNode = parseStream <|> parseGarbage
      parseStream = fmap Day09Stream $ between (char '{') (char '}') $ parseNode `sepBy` (char ',')
      parseGarbage = fmap (Day09Garbage . sum) $ (char '<' >>) $ flip manyTill (char '>') $
        (char '!' >> get >> return 0) <++ (get >> return 1)
      [(stream, _)] = readP_to_S parseStream input
        
      score :: Int -> Day09Node -> Int
      score depth (Day09Stream elems) = depth + (sum $ map (score $ depth + 1) elems)
      score _ (Day09Garbage _) = 0
      
      garbage :: Day09Node -> Int
      garbage (Day09Stream elems) = sum $ map garbage elems
      garbage (Day09Garbage n) = n
  in  (score 1 stream, garbage stream)

main = print . day09 =<< readFile "day09in.txt"
-- main = print $ day03 PartB 325489
-- main = print $ day06 "4 10 4 1 8 4 9 14 5 1 14 15 0 15 3 5"
