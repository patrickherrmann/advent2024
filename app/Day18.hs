module Day18 where

import Parsing
import Algorithm.Search
import Data.Maybe (fromJust, isNothing)
import Data.List (find)

part1 :: String -> String
part1 = show . fst . fromJust . path . take 1024 . parseInput

part2 :: String -> String
part2 input = show $ blocks !! (disconnection - 1)
  where
    disconnection = fromJust $ find (\n -> isNothing $ path $ take n blocks) [1024..]
    blocks = parseInput input

path :: [Coord] -> Maybe (Int, [Coord])
path blocks = aStar (neighbors `pruning` (`elem` blocks)) (\_ _ -> 1) (manhattan (70, 70)) (== (70, 70)) (0, 0)

type Coord = (Int, Int)

neighbors :: Coord -> [Coord]
neighbors (x, y) = filter inBounds [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

inBounds :: Coord -> Bool
inBounds (x, y) = x >= 0 && y >= 0 && x <= 70 && y <= 70

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

parseInput :: String -> [Coord]
parseInput = parseUnsafe $ coord `sepBy` newline
  where coord = (,) <$> decimal <* char ',' <*> decimal
