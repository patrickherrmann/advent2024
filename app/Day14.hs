module Day14 where

import Parsing
import qualified Data.MultiSet as MultiSet
import Data.Maybe (mapMaybe, fromJust)
import Data.List (findIndex)

part1 :: String -> String
part1 = show . safetyFactor . (!! 100) . iterate (map move) . parseInput

part2 :: String -> String
part2 input = show index
  where
    index = firstEqual (iterate (+ width) xMod) (iterate (+ height) yMod)
    xMod = minIndex $ map (entropy . map x) (take width states)
    yMod = minIndex $ map (entropy . map y) (take height states)
    states = iterate (map move) (parseInput input)

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

minIndex :: (Ord a) => [a] -> Int
minIndex xs = fromJust $ findIndex (== minimum xs) xs

firstEqual :: (Ord a) => [a] -> [a] -> a
firstEqual (x:xs) (y:yx) = case compare x y of
  LT -> firstEqual xs (y:yx)
  EQ -> x
  GT -> firstEqual (x:xs) yx

entropy :: (Ord a) => [a] -> Double
entropy xs = - sum [p * logBase 2 p | p <- probs, p > 0]
  where
    probs = map (\c -> fromIntegral c / total) (counts xs)
    total = fromIntegral (length xs)

parseInput :: String -> [Robot]
parseInput = parseUnsafe (robot `sepBy` newline)
  where
    num = signed space decimal
    coord = (,) <$> (num <* string ",") <*> num
    robot = do
      (x, y) <- string "p=" *> coord
      (vx, vy) <- string " v=" *> coord
      return $ Robot x y vx vy
