module Day04 where

import Data.List (inits, tails)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

part1 :: String -> String
part1 = show . countMatches xmases
  where
    xmases = zip "XMAS" <$> rays
    rays = tail $ zip <$> dirs <*> dirs
    dirs = [[0,0..], [0,1..],[0,-1..]]

part2 :: String -> String
part2 = show . countMatches xmases
  where
    xmases = (('A', (0, 0)) :) <$> mmsss
    mmsss = zip "MMSS" <$> rotations [(-1, -1), (1, -1), (1, 1), (-1, 1)]
    rotations s = init $ zipWith (++) (tails s) (inits s)

type Pattern = [(Char, (Int, Int))]

countMatches :: [Pattern] -> String -> Int
countMatches pats (BC.pack -> input) = sum $ map (\c -> length (filter (check c) pats)) coords
  where
    check (x, y) = all (\(c, (dx, dy)) -> charAt (x + dx, y + dy) == Just c)
    coords = [(x, y) | y <- [0..n - 1], x <- [0..n - 1]]
    charAt (x, y) = BC.indexMaybe input $ y * (n + 1) + x
    Just n = B.elemIndex 10 input
