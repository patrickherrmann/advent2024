{-# LANGUAGE OverloadedLists #-}

module Day09 where

import Data.Sequence (Seq, (<|), pattern (:<|), pattern (:|>), pattern Empty, (|>), (><))
import qualified Data.Sequence as Seq

part1 :: String -> String
part1 = show . checksum 0 . reformat1 . parseInput

reformat1 :: Seq Block -> Seq Block
reformat1 = \case
  Empty -> []
  beforeSpace :|> Space s -> reformat1 beforeSpace >-< [Space s]
  beforeFile :|> File fid fsize -> case findGap 1 beforeFile of
    (beforeGap, Space s :<| afterGap) -> if s < fsize
      then reformat1 ((beforeGap |> File fid s) >-< (afterGap |> File fid (fsize - s))) >-< [Space s]
      else reformat1 ((beforeGap |> File fid fsize |> Space (s - fsize)) >-< afterGap) >-< [Space fsize]
    _ -> reformat1 beforeFile |> File fid fsize

part2 :: String -> String
part2 = show . checksum 0 . reformat2 . parseInput

reformat2 :: Seq Block -> Seq Block
reformat2 = \case
  Empty -> []
  beforeSpace :|> Space s -> reformat2 beforeSpace >-< [Space s]
  beforeFile :|> File fid fsize -> case findGap fsize beforeFile of
    (beforeGap, Space s :<| afterGap) -> reformat2 ((beforeGap |> File fid fsize |> Space (s - fsize)) >-< afterGap) >-< [Space fsize]
    _ -> reformat2 beforeFile |> File fid fsize

checksum :: Int -> Seq Block -> Int
checksum pos = \case
  Empty -> 0
  Space s :<| bs -> checksum (pos + s) bs
  File fid fsize :<| bs -> (sum $ zipWith (*) [pos..] (replicate fsize fid)) + checksum (pos + fsize) bs

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
