module Day08 where

import Data.List (nub)
import qualified Data.MultiMap as Map

type Coord = (Int, Int)

part1 :: String -> String
part1 input = show . length . nub . filter inBounds $ antennaPairs input >>= uncurry antinodes
  where
    antinodes :: Coord -> Coord -> [Coord]
    antinodes (x1, y1) (x2, y2) = [(x2 + (x2 - x1), y2 + (y2 - y1)), (x1 + (x1 - x2), y1 + (y1 - y2))]
    inBounds (x, y) = x >= 0 && x < 50 && y >= 0 && y < 50

part2 :: String -> String
part2 input = show . length . filter isAntinode $ coords
  where
    isAntinode c = any (c `antinodeOf`) (antennaPairs input)
    coords = [(x, y) | x <- [0..49], y <- [0..49]]

antinodeOf :: Coord -> (Coord, Coord) -> Bool
antinodeOf (x, y) ((x1, y1), (x2, y2)) = (x - x1) `rem` dx == 0 && (y - y1) `rem` dy == 0 && (x - x1) `div` dx == (y - y1) `div` dy
  where
    dx = x2 - x1
    dy = y2 - y1

antennaPairs :: String -> [(Coord, Coord)]
antennaPairs input = Map.elems (antennae (0, 0) input) >>= pairs
  where
    antennae (x, y) = \case
      [] -> Map.empty
      ('.':cs) -> antennae (x + 1, y) cs
      ('\n':cs) -> antennae (0, y + 1) cs
      (c:cs) -> Map.insert c (x, y) $ antennae (x + 1, y) cs


pairs :: [a] -> [(a, a)]
pairs = \case
  [] -> []
  (x:xs) -> map (x,) xs ++ pairs xs
