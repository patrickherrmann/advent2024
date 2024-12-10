module Day10 where

import Data.Array.Unboxed
import Data.Array.ST
import Data.Char (ord)
import Control.Monad.ST
import Control.Monad (forM_, foldM, void)

part1 :: String -> String
part1 input = show $ sum $ map (scores !) (trailheads g)
  where
    g = parseInput input
    scores = runSTUArray $ do
      a <- newArray (bounds g) 0
      forM_ (indices g) $ incScores g a
      return a


part2 :: String -> String
part2 input = show $ sum $ map (ratings !) (trailheads g)
  where
    g = parseInput input
    ratings = runSTUArray $ do
      a <- newArray (bounds g) 0
      forM_ (indices g) $ incRatings g a
      return a

type Coord = (Int, Int)
type Height = Int
type Grid = UArray Coord Height
type Values s = STUArray s Coord Int

trailheads :: Grid -> [Coord]
trailheads g = [c | c <- indices g, g ! c == 0]

incScores :: Grid -> Values s -> Coord -> ST s ()
incScores g scores = void . go 9 []
  where
    go h v c
      | inRange (bounds g) c && g ! c == h && not (c `elem` v) = do
        s <- readArray scores c
        writeArray scores c (s + 1)
        let v' = c : v
        if (h > 0)
          then foldM (go (h -1)) v' (neighbors c)
          else return v'
      | otherwise = return v

incRatings :: Grid -> Values s -> Coord -> ST s ()
incRatings g ratings = go 9
  where
    go h c
      | inRange (bounds g) c && g ! c == h = do
        s <- readArray ratings c
        writeArray ratings c (s + 1)
        if (h > 0)
          then forM_ (neighbors c) (go (h -1))
          else return ()
      | otherwise = return ()

neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

parseInput :: String -> Grid
parseInput input = listArray ((1, 1), (n, n)) hs
  where
    hs = map (\c -> ord c - ord '0') (concat ls)
    ls = lines input
    n = length ls
