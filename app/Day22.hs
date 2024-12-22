module Day22 where

import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map

part1 :: String -> String
part1 = show . sum . map (last . secrets . read) . lines

part2 :: String -> String
part2 = show . maximum . Map.elems . (Map.unionsWith (+)) . map (buys . prices . read) . lines

type Secret = Int
type Price = Int
type Signal = (Int, Int, Int, Int)

buys :: [Price] -> Map Signal Price
buys ps = go Map.empty (zip (tail ps) (diffs ps))
  where
    go m xs@((_, d1):(_, d2):(_, d3):(p, d4):_) = go (Map.insertWith (flip const) (d1, d2, d3, d4) p m) (tail xs)
    go m _ = m

diffs :: [Price] -> [Int]
diffs = zipWith (-) =<< tail

prices :: Secret -> [Price]
prices = map (`rem` 10) . secrets

secrets :: Secret -> [Secret]
secrets = take 2001 . iterate evolve

evolve :: Secret -> Secret
evolve s = s3
  where
    s1 = 0xFFFFFF .&. ((s `shiftL` 6) `xor` s)
    s2 = 0xFFFFFF .&. ((s1 `shiftR` 5) `xor` s1)
    s3 = 0xFFFFFF .&. ((s2 `shiftL` 11) `xor` s2)
