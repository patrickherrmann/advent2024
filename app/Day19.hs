module Day19 where

import Parsing
import Data.Map (Map)
import qualified Data.Map as Map

part1 :: String -> String
part1 input = show $ length $ filter (matches trie) designs
  where
    (towels, designs) = parseInput input
    trie = combos towels

part2 :: String -> String
part2 input = show $ sum $ map (matchCount trie) designs
  where
    (towels, designs) = parseInput input
    trie = combos towels

combos :: [String] -> Trie
combos = plus . foldl1 alt . map literal

data Trie = Node Int (Map Char Trie)

literal :: String -> Trie
literal = \case
  "" -> Node 1 Map.empty
  (c:cs) -> Node 0 $ Map.singleton c (literal cs)

alt :: Trie -> Trie -> Trie
alt (Node a e1) (Node b e2) = Node (a + b) (Map.unionWith alt e1 e2)

app :: Trie -> Trie -> Trie
app (Node a e) other = if a > 0 then alt n' other else n'
  where n' = Node a $ Map.map (`app` other) e

plus :: Trie -> Trie
plus trie = trie `app` plus trie

matches :: Trie -> String -> Bool
matches trie s = matchCount trie s > 0

matchCount :: Trie -> String -> Int
matchCount (Node a m) = \case
  "" -> a
  (c:cs) -> case Map.lookup c m of
    Just trie -> matchCount trie cs
    Nothing -> 0

type Towel = String
type Design = String

parseInput :: String -> ([Towel], [Design])
parseInput = parseUnsafe $ do
  towels <- many letterChar `sepBy` string ", "
  void $ newline *> newline
  designs <- many letterChar `sepBy` newline
  return (towels, designs)
