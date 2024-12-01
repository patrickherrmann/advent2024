module Day01 where

import Parsing
import Data.List
import qualified Data.MultiSet as MultiSet

part1 :: String -> String
part1 input = show $ sum $ zipWith diff (sort xs) (sort ys)
  where
    (xs, ys) = parseLists input
    diff x y = abs (x - y)

part2 :: String -> String
part2 input = show $ sum $ map (\y -> y * MultiSet.occur y xsSet) ys
  where
    (xs, ys) = parseLists input
    xsSet = MultiSet.fromList xs

parseLists :: String -> ([Int], [Int])
parseLists = unzip . parseUnsafe (line `sepBy` newline)
  where
    line = do
      a <- decimal
      space
      b <- decimal
      return (a, b)
