module Day01 where
import Data.List
import Data.Set (fromList, member)

part1 :: String -> String
part1 input = show $ sum $ zipWith diff (sort xs) (sort ys)
  where
    (xs, ys) = parseInput input
    diff x y = abs (x - y)

part2 :: String -> String
part2 input = show $ sum $ filter (`member` xsSet) ys
  where
    xsSet = fromList xs
    (xs, ys) = parseInput input

parseInput :: String -> ([Int], [Int])
parseInput = unzip . map parseLine . lines
  where
    parseLine :: String -> (Int, Int)
    parseLine = (\[x, y] -> (read x, read y)) . words
