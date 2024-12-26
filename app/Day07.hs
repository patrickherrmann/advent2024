module Day07 where

import Parsing
import Prelude hiding ((||))

part1 :: String -> String
part1 = show . sum . map fst . filter (possible [(*), (+)]) . parseEquations

part2 :: String -> String
part2 = show . sum . map fst . filter (possible [(*), (+), (||)]) . parseEquations

(||) :: Op
a || b = a * 10 ^ (length (show b)) + b

type Equation = (Integer, [Integer])
type Op = Integer -> Integer -> Integer

possible :: [Op] -> Equation -> Bool
possible ops (lhs, rhs) = go (head rhs) (tail rhs)
  where
    go acc [] = acc == lhs
    go acc _ | acc > lhs = False
    go acc (x:xs) = any id [go (acc `op` x) xs | op <- ops]

parseEquations :: String -> [Equation]
parseEquations = parseUnsafe $ equation `sepBy` newline
  where
    equation = do
      lhs <- decimal
      void $ string ": "
      rhs <- decimal `sepBy` char ' '
      return (lhs, rhs)

