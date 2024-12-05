module Day04 where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

part1 :: String -> String
part1 (BC.pack -> input) = show . sum $ map (\c -> length (filter (check c) xmases)) coords
  where
    check (x, y) = all (\(c, (dx, dy)) -> charAt (x + dx, y + dy) == Just c)
    coords = [(x, y) | y <- [0..n - 1], x <- [0..n - 1]]
    xmases = zip "XMAS" <$> dirs
    dirs = tail $ zip <$> [same, inc, dec] <*> [same, inc, dec]
    inc = [0..]
    dec = [0,-1..]
    same = [0,0..]
    charAt (x, y) = BC.indexMaybe input $ y * (n + 1) + x
    Just n = B.elemIndex 10 input

part2 :: String -> String
part2 _ = "Day 04b not implemented yet"
