module Day05 where

import Parsing
import Data.List (elemIndex)

part1 :: String -> String
part1 input = show . sum . map middle $ filter (\u -> all (satisfies u) rules) updates
  where
    (rules, updates) = parseInput input
    middle s = s !! (length s `div` 2)
    satisfies update (a, b) = case (elemIndex a update, elemIndex b update) of
      (Just ai, Just bi) -> ai < bi
      _ -> True

part2 :: String -> String
part2 _ = "Day 05b not implemented yet"

type Rule = (Int, Int)
type Update = [Int]

parseInput :: String -> ([Rule], [Update])
parseInput = parseUnsafe $ do
    rules <- rule `sepEndBy` newline
    void $ newline
    updates <- update `sepEndBy` newline
    return (rules, updates)
  where
    rule = do
      a <- decimal
      void $ char '|'
      b <- decimal
      return (a, b)
    update = decimal `sepBy` char ','
