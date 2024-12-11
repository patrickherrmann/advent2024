{-# LANGUAGE OverloadedLists #-}

module Day09 where

import Data.Sequence (Seq, (<|), pattern (:<|), pattern (:|>), pattern Empty, (|>), (><))
import qualified Data.Sequence as Seq

part1 :: String -> String
part1 input = show $ checksum1 $ take (sum fileSizes) condense
  where
    fileContent = concat $ zipWith replicate fileSizes [0..]
    fileSizes = everyOther ints
    ints = map (read . (: [])) input
    condense = go 0 ints (reverse fileContent)
    go fid (fs:ss:is) filler = replicate fs fid ++ take ss filler ++ go (fid + 1) is (drop ss filler)
    go fid (fs:[]) _ = replicate fs fid
    go _ [] _ = []

checksum1 :: [Int] -> Int
checksum1 = sum . zipWith (*) [0..]

everyOther :: [Int] -> [Int]
everyOther (fs:_:fss) = fs : everyOther fss
everyOther (fs:[]) = [fs]
everyOther _ = []

part2 :: String -> String
part2 = show . checksum2 0 . reformat . parseInput

reformat :: Seq Block -> Seq Block
reformat = \case
  Empty -> []
  beforeSpace :|> Space s -> reformat beforeSpace >-< [Space s]
  beforeFile :|> File fid fsize -> case findGap fsize beforeFile of
    (beforeGap, Space s :<| afterGap) -> reformat ((beforeGap |> File fid fsize |> Space (s - fsize)) >-< afterGap) >-< [Space fsize]
    _ -> reformat beforeFile |> File fid fsize

checksum2 :: Int -> Seq Block -> Int
checksum2 pos = \case
  Empty -> 0
  Space s :<| bs -> checksum2 (pos + s) bs
  File fid fsize :<| bs -> (sum $ zipWith (*) [pos..] (replicate fsize fid)) + checksum2 (pos + fsize) bs

type Fid = Int
data Block = File Fid Int | Space Int deriving (Eq, Show)

-- Seq concatenation that melds adjacent spaces
(>-<) :: Seq Block -> Seq Block -> Seq Block
(as :|> Space a) >-< (Space b :<| bs) = as >< (Space (a + b) <| bs)
as >-< bs = as >< bs

findGap :: Int -> Seq Block -> (Seq Block, Seq Block)
findGap s = Seq.breakl $ \case Space s' -> s' >= s; _ -> False

parseInput :: String -> Seq Block
parseInput = go 0
  where
    go :: Int -> String -> Seq Block
    go fid = \case
      (f:s:bs) -> File fid (read [f]) <| Space (read [s]) <| go (fid + 1) bs
      (f:[]) -> [File fid (read [f])]
      [] -> []
