module Day05 where

import Parsing
import Data.List
import Control.Monad (msum)

part1 :: String -> String
part1 input = show . sum . map middle $ filter (followsAll rules) updates
  where
    (rules, updates) = parseInput input

part2 :: String -> String
part2 input = show . sum . map middle $ map reorder badUpdates
  where
    (rules, updates) = parseInput input
    badUpdates = filter (not . followsAll rules) updates
    reorder u = case msum (violation u <$> rules) of
      Nothing -> u
      Just i -> reorder $ moveToFront i u
    moveToFront i xs = let (a, b) = splitAt i xs in head b : a ++ tail b

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

followsAll :: [Rule] -> Update -> Bool
followsAll rules update = all (satisfies update) rules

satisfies :: Update -> Rule -> Bool
satisfies u r = violation u r == Nothing

violation :: Update -> Rule -> Maybe Int
violation update (a, b) = case (elemIndex a update, elemIndex b update) of
  (Just ai, Just bi) -> if ai < bi then Nothing else Just ai
  _ -> Nothing

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)
