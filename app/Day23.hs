module Day23 where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.List

part1 :: String -> String
part1 = show . length . lans . graph . parseInput

part2 :: String -> String
part2 = password . maximumBy (comparing length) . maximalCliques . graph . parseInput

password :: [Vertex] -> String
password = intercalate "," . sort

maximalCliques :: Graph -> [[Vertex]]
maximalCliques g = foldr add [] (Map.keys g)
  where
    add x cliques
      | any (x `elem`) cliques = cliques
      | otherwise = maximalClique g x : cliques

maximalClique :: Graph -> Vertex -> [Vertex]
maximalClique g v = foldr invite [v] (Map.keys g)
  where
    invite x clique
      | x == v = clique
      | all (`elem` (g ! x)) clique = x : clique
      | otherwise = clique

lans :: Graph -> [[Vertex]]
lans g = nub . map sort $ Map.assocs g >>= \case
  (a, vs) | head a == 't' -> [[a, b, c] | (b, c) <- pairs vs, b `elem` g ! c]
  _ -> []

type Graph = Map String [String]
type Vertex = String
type Edge = (String, String)

graph :: [Edge] -> Graph
graph = Map.fromListWith (++) . map (\(a, b) -> (a, [b])) . undirected

undirected :: [Edge] -> [Edge]
undirected = (>>= \(a, b) -> [(a, b), (b, a)])

pairs :: [a] -> [(a, a)]
pairs = \case
  x:xs -> map (x,) xs ++ pairs xs
  [] -> []

parseInput :: String -> [Edge]
parseInput = map (\l -> (take 2 l, drop 3 l)) . lines
