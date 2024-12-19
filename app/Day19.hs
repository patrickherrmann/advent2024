module Day19 where

import Prelude hiding (or, cycle)
import Parsing
import Data.Map (Map)
import qualified Data.Map as Map

part1 :: String -> String
part1 input = show $ length $ filter (matches nfa) designs
  where
    (towels, designs) = parseInput input
    nfa = buildNFA towels

part2 :: String -> String
part2 _ = "Day 19b not implemented yet"

buildNFA :: [String] -> NFA
buildNFA = nfaCycle . foldl1 nfaOr . map str

type Towel = String
type Design = String

data NFA = Node Bool (Map Char NFA) deriving (Eq, Show)

nfaEmpty :: NFA
nfaEmpty = Node False Map.empty

str :: String -> NFA
str [] = Node True Map.empty
str (c:cs) = Node False $ Map.singleton c (str cs)

nfaOr :: NFA -> NFA -> NFA
nfaOr (Node a m1) (Node b m2) = Node (a || b) (Map.unionWith nfaOr m1 m2)

nfaAppend :: NFA -> NFA -> NFA
nfaAppend (Node a edges) other = if a then n' `nfaOr` other else n'
  where n' = Node a $ Map.map (`nfaAppend` other) edges

nfaCycle :: NFA -> NFA
nfaCycle nfa = let nfa' = nfaCycle nfa in nfa `nfaAppend` nfa'

matches :: NFA -> String -> Bool
matches (Node a _) [] = a
matches (Node _ m) (c:cs) = case Map.lookup c m of
  Nothing -> False
  Just nfa -> matches nfa cs

parseInput :: String -> ([Towel], [Design])
parseInput = parseUnsafe $ do
  towels <- many letterChar `sepBy` string ", "
  void $ newline *> newline
  designs <- many letterChar `sepBy` newline
  return (towels, designs)
