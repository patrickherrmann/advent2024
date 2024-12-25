module Day25 where

import Data.List
import Data.List.Split

part1 :: String -> String
part1 = show . length . compatible . parseInput

part2 :: String -> String
part2 = const "Merry Christmas!"

compatible :: ([Key], [Lock]) -> [(Key, Lock)]
compatible (keys, locks) = [(key, lock) | key <- keys, lock <- locks, fits key lock]

fits :: Key -> Lock -> Bool
fits key lock = all (<8) $ zipWith (+) key lock

type Key = [Int]
type Lock = [Int]

parseInput :: String -> ([Key], [Lock])
parseInput input = (map counts keyGrids, map counts lockGrids)
  where
    grids = splitOn "\n\n" input
    (keyGrids, lockGrids) = partition isKey grids
    counts = map (length . filter (== '#')) . transpose . lines
    isKey g = head g == '.'
