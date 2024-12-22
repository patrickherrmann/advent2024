module Day21 where

import Data.MemoTrie

part1 :: String -> String
part1 = show . sum . map (complexity 3) . lines

part2 :: String -> String
part2 = show . sum . map (complexity 26) . lines

complexity :: Int -> String -> Int
complexity i code = numPart * presses
  where
    numPart = read $ filter (/= 'A') code
    presses = numPresses i code

numPresses :: Int -> String -> Int
numPresses i code = go (3, 4) (map numCoord code)
  where
    go _ [] = 0
    go c (x : xs) = minimum [dirPresses (i - 1) (s ++ "A") | s <- numTransitions c x] + go x xs

memoDirPresses = memo2 dirPresses

dirPresses :: Int -> String -> Int
dirPresses i code
  | i == 0 = length code
  | otherwise = go (3, 1) (map dirCoord code)
      where
        go _ [] = 0
        go c (x : xs) = minimum [memoDirPresses (i - 1) (s ++ "A") | s <- dirTransitions c x] + go x xs

numCoord :: Char -> (Int, Int)
numCoord = \case
  '7' -> (1, 1)
  '8' -> (2, 1)
  '9' -> (3, 1)
  '4' -> (1, 2)
  '5' -> (2, 2)
  '6' -> (3, 2)
  '1' -> (1, 3)
  '2' -> (2, 3)
  '3' -> (3, 3)
  '0' -> (2, 4)
  'A' -> (3, 4)

numTransitions :: (Int, Int) -> (Int, Int) -> [String]
numTransitions = transitions (1, 4)

dirCoord :: Char -> (Int, Int)
dirCoord = \case
  '^' -> (2, 1)
  'A' -> (3, 1)
  '<' -> (1, 2)
  'v' -> (2, 2)
  '>' -> (3, 2)

dirTransitions :: (Int, Int) -> (Int, Int) -> [String]
dirTransitions = transitions (1, 1)

transitions :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [String]
transitions gap a@(xa, ya) b@(xb, yb) 
    | a == gap = []
    | a == b = [""]
    | xa == xb = [v]
    | ya == yb = [h]
    | otherwise = map (h ++) (transitions gap (xb, ya) b) ++ map (v ++) (transitions gap (xa, yb) b)
  where
    h = if xb > xa then replicate (xb - xa) '>' else replicate (xa - xb) '<'
    v = if yb > ya then replicate (yb - ya) 'v' else replicate (ya - yb) '^'
