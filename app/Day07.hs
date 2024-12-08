module Day07 where

import Parsing
import Prelude hiding ((||))

part1 :: String -> String
part1 = show . sum . map fst . filter (possible [(*), (+)]) . parseEquations

part2 :: String -> String
part2 = show . sum . map fst . filter (possible [(*), (+), (||)]) . parseEquations

(||) :: Op
a || b = read $ show a ++ show b

type Equation = (Integer, [Integer])
type Op = Integer -> Integer -> Integer

possible :: [Op] -> Equation -> Bool
possible ops (lhs, x:xs) = any (== lhs) $ values ops [x] xs

values :: [Op] -> [Integer] -> [Integer] -> [Integer]
values ops acc = \case
  [] -> acc
  x:xs -> values ops (ops <*> acc <*> [x] ) xs

parseEquations :: String -> [Equation]
parseEquations = parseUnsafe $ equation `sepBy` newline
  where
    equation = do
      lhs <- decimal
      void $ string ": "
      rhs <- decimal `sepBy` char ' '
      return (lhs, rhs)

