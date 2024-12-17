module Day15 where

import Data.Array.Unboxed
import Data.Array.ST
import Data.STRef
import Control.Monad.ST
import Control.Monad (forM_, zipWithM_, mapM, foldM)
import Data.List (elemIndex)

part1 :: String -> String
part1 input = show $ gpsSum finalGrid
  where
    (grid, dirs) = parseInput input
    finalGrid = simulate dirs grid

part2 :: String -> String
part2 input = show $ gpsSum finalGrid
  where
    (grid, dirs) = parseInput input
    finalGrid = simulate dirs (widen grid)

type Coord = (Int, Int)
type Grid = UArray Coord Char
type Dir = Char

gpsSum :: Grid -> Int
gpsSum grid = sum [gps c | c <- indices grid, grid ! c `elem` "O["]

gps :: Coord -> Int
gps (y, x) = (y - 1) * 100 + (x - 1)

simulate :: [Dir] -> Grid -> Grid
simulate dirs grid = runSTUArray $ do
  g <- thaw grid
  foldM (move g) (robotPosition grid) dirs
  return g

move :: STUArray s Coord Char -> Coord -> Dir -> ST s Coord
move g p (offset -> d) = push [p] [] >>= \case
    Nothing -> return p
    Just cs -> do
      forM_ cs $ \c -> do
        char <- readArray g c
        writeArray g (add d c) char
        writeArray g c '.'
      return (add d p)
    where
      vert = fst d /= 0
      push [] vs = return $ Just vs
      push (c:cs) vs 
        | c `elem` vs = push cs vs
        | otherwise = readArray g c >>= \case
          '#' -> return Nothing
          '.' -> push cs vs
          '[' | vert -> push (cs ++ [add d c, add (offset '>') c]) (c : vs)
          ']' | vert -> push (cs ++ [add d c, add (offset '<') c]) (c : vs)
          _ -> push (cs ++ [add d c]) (c : vs)

robotPosition :: Grid -> Coord
robotPosition grid = head [c | c <- indices grid, grid ! c == '@']

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

offset :: Dir -> (Int, Int)
offset = \case
  '^' -> (-1, 0)
  '>' -> (0, 1)
  'v' -> (1, 0)
  '<' -> (0, -1)

widen :: Grid -> Grid
widen g = listArray ((1, 1), (yn, xn * 2)) $ elems g >>= widenChar
  where 
    (_, (yn, xn)) = bounds g
    widenChar = \case
      '.' -> ".."
      '#' -> "##"
      'O' -> "[]"
      '@' -> "@."

parseInput :: String -> (Grid, [Dir])
parseInput s = (grid, filter (`elem` "^>v<") dirsString)
  where
    grid = listArray ((1, 1), (n, n)) $ filter (/= '\n') gridString
    (gridString, dirsString) = splitAt (n * (n + 1)) s
    Just n = elemIndex '\n' s
