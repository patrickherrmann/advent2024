module Day09 where

import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq

part1 :: String -> String
part1 input = show $ checksum $ take (sum fileSizes) condense
  where
    fileContent = concat $ zipWith replicate fileSizes [0..]
    fileSizes = everyOther ints
    ints = map (read . (: [])) input
    condense = go 0 ints (reverse fileContent)
    go fid (fs:ss:is) filler = replicate fs fid ++ take ss filler ++ go (fid + 1) is (drop ss filler)
    go fid (fs:[]) _ = replicate fs fid
    go _ [] _ = []

checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..]

everyOther :: [Int] -> [Int]
everyOther (fs:_:fss) = fs : everyOther fss
everyOther (fs:[]) = [fs]
everyOther _ = []

data Block = File Int Int | Space Int deriving (Eq, Show)

part2 :: String -> String
part2 input = "TODO"
  where
    blocks = parseInput input

parseInput :: String -> Seq Block
parseInput = go 0
  where
    go :: Int -> String -> Seq Block
    go fid = \case
      (f:s:bs) -> File fid (read [f]) <| Space (read [s]) <| go (fid + 1) bs
      (f:[]) -> Seq.singleton $ File fid (read [f])
      [] -> Seq.empty
