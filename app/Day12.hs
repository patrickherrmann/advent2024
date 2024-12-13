module Day12 where

import Data.Array.Unboxed

part1 :: String -> String
part1 = show . sum . map cost . searchGrid . parseInput

part2 :: String -> String
part2 = show . sum . map discountCost . searchGrid . parseInput

type Coord = (Int, Int)
type Grid = UArray Coord Char

data Region = Region { perimeter :: Int, sides :: Int, searched :: [Coord], members :: [Coord] } deriving (Show)

searchGrid :: Grid -> [Region]
searchGrid g = foldr go [] (indices g)
  where
    go c rs
      | any (c `elem`) (members <$> rs) = rs
      | otherwise = searchRegion g c : rs

searchRegion :: Grid -> Coord -> Region
searchRegion g c0 = go c0 c0 (Region 0 0 [] [])
  where
    label = g ! c0
    outsideRegion c = not (inRange (bounds g) c) ||  g ! c /= label
    incPerim r = r { perimeter = perimeter r + 1 }
    incSides parent c r = if sameSide then r else r { sides = sides r + 1 }
      where
        (inside, outside) = sideContinuation (parent, c)
        sameSide = (not . outsideRegion) inside && outsideRegion outside
    go parent c r
      | c `elem` members r = r
      | c `elem` searched r = (incPerim . incSides parent c) r
      | outsideRegion c = ((incPerim . incSides parent c) r) { searched = c : searched r }
      | otherwise = foldr (go c) (r { searched = c : searched r, members = c : members r }) (neighbors c)

cost :: Region -> Int
cost r = perimeter r * length (members r)

discountCost :: Region -> Int
discountCost r = sides r * length (members r)

neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- Given coords passing from inside to outside of a region,
-- return the coords immedately to the left of these. If they
-- are also passing from inside to outside, this pair is a continuation
-- of an already-counted side.
sideContinuation :: (Coord, Coord) -> (Coord, Coord)
sideContinuation ((x, y), (x', y'))
  | x == x' = let dy = y' - y in ((x + dy, y), (x + dy, y'))
  | y == y' = let dx = x' - x in ((x, y - dx), (x', y - dx))
  | otherwise = error "coords must be adjacent"

parseInput :: String -> Grid
parseInput input = listArray ((1, 1), (n, n)) (concat ls)
  where
    ls = lines input
    n = length ls
