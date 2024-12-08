{-# LANGUAGE Strict #-}

module Day06 where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Set (Set)
import qualified Data.Set as Set

part1 :: String -> String
part1 (BC.pack -> input) = show . Set.size . Set.map fst . fst $ sim input Nothing
 
part2 :: String -> String
part2 (BC.pack -> input) = show . length $ filter (\i -> snd $ sim input (Just i)) $ obstacleIndices
  where
    indicesInPath = Set.map (coordToIndex . fst) . fst $ sim input Nothing
    coordToIndex (x, y) = y * (n + 1) + x
    obstacleIndices = filter (`Set.member` indicesInPath) $ BC.elemIndices '.' input
    Just n = BC.elemIndex '\n' input

type Coord = (Int, Int)
data Dir = U | R | D | L deriving (Enum, Bounded, Eq, Ord, Show)

nextDir :: Dir -> Dir
nextDir = \case
  L -> U
  d -> succ d

offset :: Dir -> Coord
offset = \case
  U -> (0, -1)
  R -> (1, 0)
  D -> (0, 1)
  L -> (-1, 0)

type Visited = Set (Coord, Dir)

sim :: B.ByteString -> Maybe Int -> (Visited, Bool)
sim input obstruction = go Set.empty ((x0, y0), U)
  where
    charAt (x, y)
      | idx < 0 || idx >= B.length input = Nothing
      | Just idx == obstruction = Just '#'
      | otherwise = BC.indexMaybe input idx
      where
        idx = y * (n + 1) + x
    Just n = BC.elemIndex '\n' input
    Just i0 = BC.elemIndex '^' input
    (y0, x0) = i0 `divMod` (n + 1)
    move ((x, y), (offset -> (dx, dy))) = (x + dx, y + dy)
    go :: Visited -> (Coord, Dir) -> (Visited, Bool)
    go visited s@((x, y), d)
      | s `Set.member` visited = (visited, True) -- infinite loop
      | otherwise =
        case charAt next of
          Just '#' -> go nextVisited ((x, y), nextDir d)
          Just _   -> go nextVisited (next, d)
          Nothing  -> (nextVisited, False) -- left the grid
        where
          next = move s
          nextVisited = Set.insert s visited

