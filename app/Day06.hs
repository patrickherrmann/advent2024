module Day06 where

import Data.List (nub)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

part1 :: String -> String
part1 = show . length . pathCoords . parseInput
 
part2 :: String -> String
part2 input = show . length $ filter (loops . states) g's
  where
    g = parseInput input
    g's = map (\c -> g { obstacle = Just c }) (tail (pathCoords g))

pathCoords :: Grid -> [Coord]
pathCoords = nub . map fst . states

states :: Grid -> [(Coord, Dir)]
states g = go (s0 g)
  where
    go (c, d) = case charAt g c of
      Just '#' -> go (move (opp d) c, turnRight d)
      Just _ -> (c, d) : go (move d c, d)
      Nothing -> []

loops :: Eq a => [a] -> Bool
loops as = go as as
  where
    go (t:ts) (_:h:hs) = t == h || go ts hs
    go _ _ = False

type Coord = (Int, Int)
type Dir = (Int, Int)

data Grid = Grid
  { bytes :: ByteString
  , size :: Int
  , c0 :: Coord
  , obstacle :: Maybe Coord
  }

parseInput :: String -> Grid
parseInput input = Grid b n (x0, y0) Nothing
  where
    b = BC.pack input
    Just n = BC.elemIndex '\n' b
    Just i0 = BC.elemIndex '^' b
    (y0, x0) = i0 `divMod` (n + 1)

s0 :: Grid -> (Coord, Dir)
s0 g = (c0 g, (0, -1))

charAt :: Grid -> Coord -> Maybe Char
charAt g c@(x, y)
  | Just c == obstacle g = Just '#'
  | x < 0 || y < 0 || x >= size g || y >= size g = Nothing
  | otherwise = BC.indexMaybe (bytes g) (x + y * (size g + 1))

move :: Dir -> Coord -> Coord
move (dx, dy) (x, y) = (x + dx, y + dy)

turnRight :: Dir -> Dir
turnRight = \case
  (0, -1) -> (1, 0)
  (1, 0) -> (0, 1)
  (0, 1) -> (-1, 0)
  (-1, 0) -> (0, -1)

opp :: Dir -> Dir
opp (dx, dy) = (-dx, -dy)
