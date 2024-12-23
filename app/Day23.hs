module Day23 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

part1 :: String -> String
part1 = show . length . lans . graph . bidir . parseInput

part2 :: String -> String
part2 _ = "Day 23b not implemented yet"

lans :: Map String [String] -> [[String]]
lans g = nub . map sort $ go (Map.assocs g)
  where
    go ((a, a's):xs) 
      | head a == 't' = [[a, b, c] | (b, c) <- pairs a's, b `elem` g Map.! c] ++ go xs
      | otherwise = go xs
    go [] = []

graph :: [(String, String)] -> Map String [String]
graph = Map.fromListWith (++) . map (\(a, b) -> (a, [b]))

bidir :: [(String, String)] -> [(String, String)]
bidir ((a, b):xs) = (a, b) : (b, a) : bidir xs
bidir [] = []

pairs :: [a] -> [(a, a)]
pairs (x:xs) = map (x,) xs ++ pairs xs
pairs [] = []

parseInput :: String -> [(String, String)]
parseInput = map (\l -> (take 2 l, drop 3 l)) . lines
