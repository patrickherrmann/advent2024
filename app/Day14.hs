module Day14 where

import Parsing
import Data.Array.Unboxed
import qualified Data.MultiSet as MultiSet
import Data.Maybe (mapMaybe, fromJust)
import Data.List (findIndex)

part1 :: String -> String
part1 = show . safetyFactor . (!! 100) . iterate (map move) . parseInput

part2 :: String -> String
part2 = show . fromJust . findIndex lowEntropy . iterate (map move) . parseInput

safetyFactor :: [Robot] -> Int
safetyFactor = product . counts . mapMaybe quad

counts :: (Ord a) => [a] -> [Int]
counts = map snd . MultiSet.toOccurList . MultiSet.fromList

data Quad = NW | NE | SW | SE deriving (Eq, Ord)

quad :: Robot -> Maybe Quad
quad r
  | x r < 50 && y r < 51 = Just NW
  | x r > 50 && y r < 51 = Just NE
  | x r < 50 && y r > 51 = Just SW
  | x r > 50 && y r > 51 = Just SE
  | otherwise = Nothing

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

lowEntropy :: [Robot] -> Bool
lowEntropy rs = xEntropy < 0.9 && yEntropy < 0.9
  where
    xEntropy = entropy (map x rs) / maxEntropy width
    yEntropy = entropy (map y rs) / maxEntropy height

maxEntropy :: Int -> Double
maxEntropy = logBase 2 . fromIntegral

entropy :: (Ord a) => [a] -> Double
entropy xs = - sum [p * logBase 2 p | p <- probs, p > 0]
  where
    probs = map (\c -> fromIntegral c / total) (counts xs)
    total = fromIntegral (length xs)

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
