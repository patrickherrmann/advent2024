module Day15 where

import Data.Array.Unboxed
import Data.Array.ST
import Data.STRef
import Control.Monad.ST
import Control.Monad (forM_, zipWithM_, mapM, foldM)

part1 :: String -> String
part1 input = show $ gpsSum finalGrid
  where
    (normalGrid -> grid, dirs) = parseInput input
    finalGrid = simulate1 dirs grid

part2 :: String -> String
part2 _ = "Day 15b not implemented yet"

type Coord = (Int, Int)
type Grid = UArray Coord Char
type Dir = Char

gpsSum :: Grid -> Int
gpsSum grid = sum [gps c | c <- indices grid, grid ! c == 'O']

gps :: Coord -> Int
gps (y, x) = (y - 1) * 100 + (x - 1)

simulate1 :: [Dir] -> Grid -> Grid
simulate1 dirs grid = runSTUArray $ do
  g <- thaw grid
  foldM (move g) (25, 25) dirs
  return g

move :: STUArray s (Int, Int) Char -> Coord -> Dir -> ST s Coord
move g p (offset -> d) = push c0
  where
    c0 = add d p
    push c = readArray g c >>= \case
      '#' -> return p
      'O' -> push (add d c)
      '.' -> do
        writeArray g c 'O'
        writeArray g p '.'
        writeArray g c0 '@'
        return c0

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

offset :: Dir -> (Int, Int)
offset = \case
  '^' -> (-1, 0)
  '>' -> (0, 1)
  'v' -> (1, 0)
  '<' -> (0, -1)

normalGrid :: String -> Grid
normalGrid = listArray ((1, 1), (50, 50))

wideGrid :: String -> Grid
wideGrid = listArray ((1, 1), (50, 100)) . (>>= widen)
  where
    widen = \case
      '.' -> ".."
      '#' -> "##"
      'O' -> "[]"
      '@' -> "@."

parseInput :: String -> (String, [Dir])
parseInput s = (filter (/= '\n') gridString, dirs)
  where
    (gridString, dirsString) = splitAt (50 * 51) s
    dirs = filter (`elem` "^>v<") dirsString
