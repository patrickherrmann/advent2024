module Day14 where

import Parsing
import Data.Array.Unboxed

part1 :: String -> String
part1 = show . safetyFactor . countQuads . (!! 100) . iterate (map move) . parseInput

part2 :: String -> String
part2 = draw . grid . (!! 6243) . iterate (map move) . parseInput

safetyFactor :: (Int, Int, Int, Int) -> Int
safetyFactor (nw, ne, sw, se) = nw * ne * sw * se

countQuads :: [Robot] -> (Int, Int, Int, Int)
countQuads = foldr go (0, 0, 0, 0)
  where
    go r (nw, ne, sw, se)
      | x r < 50 && y r < 51 = (nw + 1, ne, sw, se)
      | x r > 50 && y r < 51 = (nw, ne + 1, sw, se)
      | x r < 50 && y r > 51 = (nw, ne, sw + 1, se)
      | x r > 50 && y r > 51 = (nw, ne, sw, se + 1)
      | otherwise = (nw, ne, sw, se)

data Robot = Robot { x :: Int, y :: Int, vx :: Int, vy :: Int } deriving (Show)

move :: Robot -> Robot
move (Robot x y vx vy) = Robot (wrap width $ x + vx) (wrap height $ y + vy) vx vy

width :: Int
width = 101

height :: Int
height = 103

wrap :: Int -> Int -> Int
wrap n x
  | x < 0 = x + n
  | x >= n = x - n
  | otherwise = x

shannonEntropy :: [Double] -> Double
shannonEntropy probabilities =
  - sum [p * logBase 2 p | p <- probabilities, p > 0]

type Grid = UArray (Int, Int) Int

draw :: Grid -> String
draw g = unlines [[if g ! (x, y) > 0 then '#' else '.' | x <- [0..width - 1]] | y <- [0..height - 1]]

grid :: [Robot] -> Grid
grid rs = accumArray (+) 0 ((0, 0), (width - 1, height - 1)) [((x r, y r), 1) | r <- rs]

parseInput :: String -> [Robot]
parseInput = parseUnsafe (robot `sepBy` newline)
  where
    num = signed space decimal
    coord = (,) <$> (num <* string ",") <*> num
    robot = do
      (x, y) <- string "p=" *> coord
      (vx, vy) <- string " v=" *> coord
      return $ Robot x y vx vy
