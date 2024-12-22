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
    presses = numpadPresses i code

numpadPresses :: Int -> String -> Int
numpadPresses = \case
  0 -> length
  i -> sum . map (fewestPresses (i - 1) . numpadPaths) . transitions

dirpadPresses :: Int -> String -> Int
dirpadPresses = memo2 $ \case
  0 -> length
  i -> sum . map (fewestPresses (i - 1) . dirpadPaths) . transitions

fewestPresses :: Int -> [String] -> Int
fewestPresses i paths = minimum [dirpadPresses i (p ++ "A") | p <- paths]

transitions :: String -> [(Char, Char)]
transitions code = zip ('A':code) code

numpadPaths :: (Char, Char) -> [String]
numpadPaths (x, y) = pathsAvoiding (numCoord ' ') (numCoord x) (numCoord y)

dirpadPaths :: (Char, Char) -> [String]
dirpadPaths (x, y) = pathsAvoiding (dirCoord ' ') (dirCoord x) (dirCoord y)

pathsAvoiding :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [String]
pathsAvoiding gap a@(xa, ya) b@(xb, yb) 
    | a == gap = []
    | a == b = [""]
    | xa == xb = [v]
    | ya == yb = [h]
    | otherwise = map (h ++) (pathsAvoiding gap (xb, ya) b) ++ map (v ++) (pathsAvoiding gap (xa, yb) b)
  where
    h = if xb > xa then replicate (xb - xa) '>' else replicate (xa - xb) '<'
    v = if yb > ya then replicate (yb - ya) 'v' else replicate (ya - yb) '^'

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
  ' ' -> (1, 4)
  '0' -> (2, 4)
  'A' -> (3, 4)

dirCoord :: Char -> (Int, Int)
dirCoord = \case
  ' ' -> (1, 1)
  '^' -> (2, 1)
  'A' -> (3, 1)
  '<' -> (1, 2)
  'v' -> (2, 2)
  '>' -> (3, 2)
