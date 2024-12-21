module Day20 where

import Data.Array.Unboxed
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq, pattern Empty, pattern (:<|))
import qualified Data.Sequence as Seq 

part1 :: String -> String
part1 = show . length . filter (>= 100) . cheats 2 . parseInput

part2 :: String -> String
part2 = show . length . filter (>= 100) . ([2..20] >>=) . flip cheats . parseInput

type Coord = (Int, Int)
type Grid = UArray Coord Char

cheats :: Int -> Grid -> [Int]
cheats n g = path m start >>= cheatsFrom
  where
    start = coordOf g 'S'
    m = bfs g
    cheatsFrom (c, cost) = mapMaybe cheatTo (distFrom c n)
      where
        cheatTo c' = do
          (_, cost') <- Map.lookup c' m
          return (cost - cost' - n)


distFrom :: Coord -> Int -> [Coord]
distFrom (x0, y0) n = [(x, y) | x <- [x0 - n .. x0 + n], y <- [y0 - n .. y0 + n], abs (x - x0) + abs (y - y0) == n]

path :: Map Coord (PrevCoord, Int) -> Coord -> [(Coord, Int)]
path dists c = case Map.lookup c dists of
  Nothing -> []
  Just (c', cost) -> (c, cost) : path dists c'

type PrevCoord = Coord

bfs :: Grid -> Map Coord (PrevCoord, Int)
bfs g = go (Seq.singleton s0) Map.empty
  where
    s0 = (coordOf g 'E', (-1, -1), 0)
    neighbors (c, _, cost) = zip3 (moves c) (repeat c) (repeat (cost + 1))
    moves (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
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
