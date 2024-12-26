module Day19 where

import Parsing
import Data.List
import Data.MemoTrie

part1 :: String -> String
part1 input = show $ length $ filter (matches towels) designs
  where
    (towels, designs) = parseInput input

part2 :: String -> String
part2 input = show total
  where
    total = sum $ map (matches towels) designs :: Int
    (towels, designs) = parseInput input

class MatchRes r where success :: r; rollup :: [r] -> r
instance MatchRes Bool where success = True; rollup = any id
instance MatchRes Int where success = 1; rollup = sum

matches :: MatchRes m => [Towel] -> Design -> m
matches towels = go
  where
    go = memo $ \case
      "" -> success
      design -> rollup [go (drop (length t) design) | t <- towels, t `isPrefixOf` design]

type Towel = String
type Design = String

parseInput :: String -> ([Towel], [Design])
parseInput = parseUnsafe $ do
  towels <- many letterChar `sepBy` string ", "
  void $ newline *> newline
  designs <- many letterChar `sepBy` newline
  return (towels, designs)

