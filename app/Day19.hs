module Day19 where

import Parsing
import Data.Map (Map)
import qualified Data.Map as Map

part1 :: String -> String
part1 input = show $ length $ filter (matches dfa) designs
  where
    (towels, designs) = parseInput input
    dfa = combos towels

part2 :: String -> String
part2 input = show $ sum $ map (matchCount dfa) designs
  where
    (towels, designs) = parseInput input
    dfa = combos towels

combos :: [String] -> DFA
combos = star . foldl1 alt . map literal

data DFA = Node Int (Map Char DFA)

literal :: String -> DFA
literal = \case
  [] -> Node 1 Map.empty
  (c:cs) -> Node 0 $ Map.singleton c (literal cs)

alt :: DFA -> DFA -> DFA
alt (Node a m1) (Node b m2) = Node (a + b) (Map.unionWith alt m1 m2)

app :: DFA -> DFA -> DFA
app (Node a edges) other = if a > 0 then alt n' other else n'
  where n' = Node a $ Map.map (`app` other) edges

star :: DFA -> DFA
star dfa = dfa `app` star dfa

matches :: DFA -> String -> Bool
matches dfa s = matchCount dfa s > 0

matchCount :: DFA -> String -> Int
matchCount (Node a m) = \case
  [] -> a
  (c:cs) -> case Map.lookup c m of
    Just dfa -> matchCount dfa cs
    Nothing -> 0

type Towel = String
type Design = String

parseInput :: String -> ([Towel], [Design])
parseInput = parseUnsafe $ do
  towels <- many letterChar `sepBy` string ", "
  void $ newline *> newline
  designs <- many letterChar `sepBy` newline
  return (towels, designs)
