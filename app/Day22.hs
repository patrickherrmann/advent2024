module Day22 where

import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map

part1 :: String -> String
part1 = show . sum . map (last . secrets . read) . lines

part2 :: String -> String
part2 = show . maximum . Map.elems . Map.unionsWith (+) . map (buys . prices . read) . lines

type Secret = Int
type Price = Int
type Signal = (Int, Int, Int, Int)

buys :: [Price] -> Map Signal Price
buys ps = Map.fromListWith (flip const) $ zip (quads (diffs ps)) (drop 4 ps)

quads :: [a] -> [(a, a, a, a)]
quads = \case
  xs@(a:b:c:d:_) -> (a, b, c, d) : quads (tail xs)
  _ -> []

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
