module Day04 where

import Data.List

part1 :: String -> String
part1 input = show . sum $ map (\p -> search "XMAS" p + search "SAMX" p) (paths grid)
  where
    grid = lines input
    paths g = g ++ transpose g ++ tlbr g ++ trbl g
    tlbr g = transpose (zipWith drop [0..] g) ++ transpose (zipWith drop [1..] (transpose g))
    trbl g = tlbr (reverse g)

search :: String -> String -> Int
search needle haystack
  | null haystack = 0
  | needle `isPrefixOf` haystack = 1 + search needle (drop (length needle) haystack)
  | otherwise = search needle (tail haystack)

part2 :: String -> String
part2 _ = "Day 04b not implemented yet"
