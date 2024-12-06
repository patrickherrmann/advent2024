module Day06 where

import Data.List (nub)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

part1 :: String -> String
part1 (BC.pack -> input) = show . length . nub $ advance [(x0, y0)] dirs
  where
    charAt (x, y) = BC.indexMaybe input $ y * (n + 1) + x
    Just n = B.elemIndex 10 input
    Just i0 = B.elemIndex 94 input -- ^
    (y0, x0) = i0 `divMod` (n + 1)
    dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)] ++ dirs
    move (x, y) (dx, dy) = (x + dx, y + dy)
    advance path ds = case charAt (move (head path) (head ds)) of
      Just '#' -> advance path (tail ds)
      Just '.' -> advance (move (head path) (head ds) : path) ds
      Nothing -> path

part2 :: String -> String
part2 _ = "Day 06b not implemented yet"
