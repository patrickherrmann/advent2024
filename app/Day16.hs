module Day16 where

import Data.Array.Unboxed
import Data.List (elemIndex)
import Debug.Trace

part1 :: String -> String
part1 = show . search . parseInput

part2 :: String -> String
part2 _ = "Day 16b not implemented yet"

type Coord = (Int, Int)
type Grid = UArray Coord Char
type Cost = Int

type State = (Coord, Dir, Cost)
data Dir = N | E | S | W deriving (Eq, Show)

search :: Grid -> Int
search g = go [state0 g] []
  where
    go [] _ = maxBound
    go ((c, dir, cost):ss) visited
      | (c, dir) `elem` visited = go ss visited
      | otherwise = case g ! c of
        '#' -> go ss visited
        'E' -> min cost $ go ss visited
        _ -> go ([(move dir c, dir, cost + 1)] ++ ss ++ [(c, dir', cost + 1000) | dir' <- turns dir]) ((c, dir) : visited)

state0 :: Grid -> State
state0 g = (start g, E, 0)

move :: Dir -> Coord -> Coord
move d (y, x) = case d of
  N -> (y - 1, x)
  E -> (y, x + 1)
  S -> (y + 1, x)
  W -> (y, x - 1)

turns :: Dir -> [Dir]
turns = \case
  N -> [W, E]
  E -> [N, S]
  S -> [E, W]
  W -> [S, N]

start :: Grid -> Coord
start g = head [c | c <- indices g, g ! c == 'S']

parseInput :: String -> Grid
parseInput s = listArray ((1, 1), (n, n)) $ filter (/= '\n') s
  where Just n = elemIndex '\n' s
