module Day13 where

import Parsing
import Data.Maybe
import Data.Ratio

part1 :: String -> String
part1 = show . sum . mapMaybe cost . parseInput

part2 :: String -> String
part2 = show . sum . mapMaybe (cost . correct) . parseInput

data Machine = Machine { ax :: Rational, ay :: Rational, bx :: Rational, by :: Rational, px :: Rational, py :: Rational }

cost :: Machine -> Maybe Integer
cost (Machine ax ay bx by px py)
  | integral a && integral b = Just $ 3 * numerator a + numerator b
  | otherwise = Nothing
  where 
    -- Line intersection
    a = (by * px - bx * py) / ((ax * by) - (ay * bx))
    b = (ax * py - ay * px) / ((ax * by) - (ay * bx))
    integral r = denominator r == 1

correct :: Machine -> Machine
correct m = m { px = px m + 10000000000000, py = py m + 10000000000000 }

parseInput :: String -> [Machine]
parseInput = parseUnsafe (machine `sepBy` string "\n\n")
  where
    machine = do
      ax <- string "Button A: X+" *> decimal
      ay <- string ", Y+" *> decimal <* newline
      bx <- string "Button B: X+" *> decimal
      by <- string ", Y+" *> decimal <* newline
      px <- string "Prize: X=" *> decimal
      py <- string ", Y=" *> decimal
      return $ Machine ax ay bx by px py
