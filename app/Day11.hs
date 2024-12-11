module Day11 where

import Data.MemoTrie

type Stone = Integer

part1 :: String -> String
part1 = show . lengthAfter 25 . parseInput

part2 :: String -> String
part2 = show . lengthAfter 75 . parseInput

lengthAfter :: Int -> [Stone] -> Int
lengthAfter n = sum . map (stoneLengthAfter n)
  where
    stoneLengthAfter = curry . memo $ \case
      (0, _) -> 1
      (n', stone) -> lengthAfter (n' - 1) (blink stone)

blink :: Stone -> [Stone]
blink = \case
  0 -> [1]
  x -> if even n then [read a, read b] else [x * 2024]
    where
      s = show x
      n = length s
      (a, b) = splitAt (n `div` 2) s

parseInput :: String -> [Stone]
parseInput = map read . words
