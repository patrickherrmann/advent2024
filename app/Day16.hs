module Day16 where

import Data.Array.Unboxed
import Data.List (elemIndex, nub)
import Data.PQueue.Prio.Min (MinPQueue, pattern (:<), pattern Empty)
import qualified Data.PQueue.Prio.Min as PQ
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Debug.Trace

part1 :: String -> String
part1 = show . lowestCost . parseInput

part2 :: String -> String
part2 = show . length . goodSpots . parseInput

type Coord = (Int, Int)
type Grid = UArray Coord Char
type Cost = Int

type State = (Coord, Dir)
data Dir = N | E | S | W deriving (Eq, Ord, Show)
type Lineage = (Cost, [State])
type Result = Map State Lineage

lowestCost :: Grid -> Cost
lowestCost g = minimum . map fst $ mapMaybe (\s -> Map.lookup s r) $ endStates g
  where r = search g

-- dfs starting with the end states, find all the coordinates along
-- the best path from the start
goodSpots :: Grid -> [Coord]
goodSpots g = nub $ map fst $ go (endStates g) []
  where
    r = search g
    go [] visited = visited
    go (s:ss) visited
      | s `elem` visited = go ss visited
      | s == startState g = go ss (s : visited)
      | otherwise = let (cost, parents) = getLineage s r
                    in go (parents ++ ss) (s : visited)

search :: Grid -> Result
search g = go (PQ.singleton 0 (startState g)) [] (Map.singleton (startState g) (0, []))
  where
    go :: MinPQueue Cost State -> [State] -> Result -> Result
    go q visited result = case q of
      Empty -> error "No path found"
      (cost, s@(c, dir)) :< q' -> case g ! c of
        'E' -> result
        '#' -> go q' visited result
        _ -> go q'' visited' result'
          where
            visited' = s : visited
            ns = filter (not . (`elem` visited) . fst) $ neighbors (s, getLineage s result)
            (q'', result') = foldl step (q', result) ns
            step (_q, _r) (ns, (altcost, altparents))
                | altcost == cost = (_q, Map.insert ns (cost, altparents ++ parents) _r)
                | altcost < cost = (PQ.insert altcost ns _q, Map.insert ns (altcost, altparents) _r)
                | otherwise = (_q, _r)
              where (cost, parents) = getLineage ns result

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

startState :: Grid -> State
startState g = (start g, E)

endStates :: Grid -> [State]
endStates g = [(end g, d) | d <- [N, E, S, W]]

parseInput :: String -> Grid
parseInput s = listArray ((1, 1), (n, n)) $ filter (/= '\n') s
  where Just n = elemIndex '\n' s
