module Day16 where


import Data.Array.Unboxed
import Data.List (elemIndex)
import Data.PQueue.Prio.Min (MinPQueue, pattern (:<), pattern Empty)
import qualified Data.PQueue.Prio.Min as PQ
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

part1 :: String -> String
part1 = show . search . parseInput

part2 :: String -> String
part2 _ = "Day 16b not implemented yet"

type Coord = (Int, Int)
type Grid = UArray Coord Char
type Cost = Int

type State = (Coord, Dir)
data Dir = N | E | S | W deriving (Eq, Ord, Show)
type Lineage = (Cost, [State])
type Result = Map State Lineage

search :: Grid -> Result
search g = go (PQ.singleton 0 s0) [] (Map.singleton s0 (0, []))
  where
    s0 = (start g, E)
    go :: MinPQueue Cost State -> [State] -> Result -> Result
    go q visited result = case q of
      Empty -> error "No path found"
      (cost, s@(c, dir)) :< q' -> case g ! c of
        'E' -> result
        '#' -> go q' visited result
        _ -> go q'' visited' result'
          where
            visited' = s : visited
            lineage = getLineage s result
            ns = filter (not . (`elem` visited) . fst) $ neighbors (s, lineage)
            (q'', result') = foldl step (q', result) ns
            step (_q, _r) (ns, nl1@(ncost, nparents)) = (_q', _r')
              where
                nl2 = getLineage ns _r
                _r' = Map.insert ns (merge nl1 nl2) _r
                _q' = PQ.insert ncost ns _q

merge :: Lineage -> Lineage -> Lineage
merge a@(aCost, aParents) b@(bCost, bParents) = case compare aCost bCost of
  EQ -> (aCost, aParents ++ bParents)
  LT -> a
  GT -> b

neighbors :: (State, Lineage) -> [(State, Lineage)]
neighbors (s@(c, dir), (cost, parents)) = ((move dir c, dir), (cost + 1, [s])) : [((c, dir'), (cost + 1000, [s])) | dir' <- turns dir]

getLineage :: State -> Result -> Lineage
getLineage s r = Map.findWithDefault (maxBound, []) s r

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

end :: Grid -> Coord
end g = head [c | c <- indices g, g ! c == 'E']

parseInput :: String -> Grid
parseInput s = listArray ((1, 1), (n, n)) $ filter (/= '\n') s
  where Just n = elemIndex '\n' s
