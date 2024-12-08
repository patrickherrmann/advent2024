module Day07 where

import Parsing

part1 :: String -> String
part1 = show . sum . map fst . filter possible . parseEquations
  where
    possible (lhs, x:xs) = any (== lhs) $ values [x] xs
    values :: [Integer] -> [Integer] -> [Integer]
    values acc = \case
      [] -> acc
      x:xs -> values ( [(*), (+)] <*> acc <*> [x] ) xs
    

part2 :: String -> String
part2 _ = "Day 07b not implemented yet"

type Equation = (Integer, [Integer])

parseEquations :: String -> [Equation]
parseEquations = parseUnsafe $ equation `sepBy` newline
  where
    equation = do
      lhs <- decimal
      void $ string ": "
      rhs <- decimal `sepBy` char ' '
      return (lhs, rhs)

