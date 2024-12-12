module Day12 where

import Data.Array.Unboxed

part1 :: String -> String
part1 = show . sum . map cost . searchGrid . parseInput

part2 :: String -> String
part2 _ = "Day 12b not implemented yet"

type Coord = (Int, Int)
type Grid = UArray Coord Char

data Region = Region { label :: Char, perimeter :: Int, searched :: [Coord], members :: [Coord] } deriving (Show)

searchGrid :: Grid -> [Region]
searchGrid g = foldr go [] (indices g)
  where
    go c rs
      | any (c `elem`) (members <$> rs) = rs
      | otherwise = searchRegion g c : rs

searchRegion :: Grid -> Coord -> Region
searchRegion g c0 = go c0 (Region (g ! c0) 0 [] [])
  where
    go c r
      | not (inRange (bounds g) c) = r { perimeter = perimeter r + 1 }
      | c `elem` members r = r
      | c `elem` searched r = r { perimeter = perimeter r + 1 }
      | g ! c /= label r = r { perimeter = perimeter r + 1, searched = c : searched r }
      | otherwise = foldr go (r { searched = c : searched r, members = c : members r }) (neighbors c)

cost :: Region -> Int
cost r = perimeter r * length (members r)

neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

parseInput :: String -> Grid
parseInput input = listArray ((1, 1), (n, n)) (concat ls)
  where
    ls = lines input
    n = length ls
