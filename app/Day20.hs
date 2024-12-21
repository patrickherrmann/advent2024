module Day20 where

import Data.Array.Unboxed
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq, pattern Empty, pattern (:<|))
import qualified Data.Sequence as Seq 

part1 :: String -> String
part1 = show . length . filter (>= 100) . map snd . cheats . parseInput

part2 :: String -> String
part2 _ = "Day 20b not implemented yet"

type Coord = (Int, Int)
type Grid = UArray Coord Char
type Cheat = ((Coord, Coord), Int)

cheats :: Grid -> [Cheat]
cheats g = path m start >>= cheatsFrom
  where
    start = coordOf g 'S'
    m = bfs g
    cheatsFrom (c, cost) = mapMaybe cheatTo (possibleCheats c)
      where
        cheatTo c' = do
          (_, cost') <- Map.lookup c' m
          return ((c, c'), cost - cost' - 2)


possibleCheats :: Coord -> [Coord]
possibleCheats (x, y) = [(x + 2, y), (x - 2, y), (x, y + 2), (x, y - 2), (x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1), (x - 1, y - 1)]

path :: Map Coord (PrevCoord, Int) -> Coord -> [(Coord, Int)]
path dists c = case Map.lookup c dists of
  Nothing -> []
  Just (c', cost) -> (c, cost) : path dists c'

type PrevCoord = Coord

bfs :: Grid -> Map Coord (PrevCoord, Int)
bfs g = go (Seq.singleton s0) Map.empty
  where
    s0 = (coordOf g 'E', (-1, -1), 0)
    neighbors (c@(x, y), _, cost) = zip3 [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)] (repeat c) (repeat (cost + 1))
    go q dists = case q of
      Empty -> dists
      s@(c, c', cost) :<| q' -> if Map.member c dists || g ! c == '#'
        then go q' dists
        else go (q' <> Seq.fromList (neighbors s)) (Map.insert c (c', cost) dists)

coordOf :: Grid -> Char -> Coord
coordOf g c = head [coord | coord <- range (bounds g), g ! coord == c]

parseInput :: String -> Grid
parseInput input = listArray ((0, 0), (n - 1, n - 1)) (filter (/= '\n') input)
  where
    Just n = elemIndex '\n' input
