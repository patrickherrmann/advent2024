module Day01 where

import Parsing
import Data.List (sort)
import qualified Data.MultiSet as MultiSet

part1 :: String -> String
part1 input = show $ sum $ zipWith diff (sort xs) (sort ys)
  where
    (xs, ys) = parseLists input
    diff x y = abs (x - y)

part2 :: String -> String
part2 input = show $ sum $ map similarity xs
  where
    (xs, ys) = parseLists input
    similarity x = x * MultiSet.occur x ysSet
    ysSet = MultiSet.fromList ys

parseLists :: String -> ([Int], [Int])
parseLists = unzip . parseUnsafe (line `sepBy` newline)
  where
    line = do
      a <- decimal
      space
      b <- decimal
      return (a, b)
