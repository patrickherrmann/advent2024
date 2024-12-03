module Day03 where

import Parsing
import Data.List (foldl')

part1 :: String -> String
part1 = show . foldl' interpret 0 . parseInput
  where
    interpret s (Mul a b) = a * b + s
    interpret s _ = s

part2 :: String -> String
part2 = show . fst . foldl' interpret (0, Enabled) . parseInput
  where
    interpret (s, _) Do = (s, Enabled)
    interpret (s, _) Dont = (s, Disabled)
    interpret (s, Enabled) (Mul a b) = (a * b + s, Enabled)
    interpret (s, Disabled) _ = (s, Disabled)

data Expr = Do | Dont | Mul Int Int
data Mode = Enabled | Disabled

parseInput :: String -> [Expr]
parseInput = parseUnsafe $ search expr
  where
    search p = (eof *> return []) <|> ((:) <$> try p <*> search p) <|> (anySingle *> search p)
    expr = (Dont <$ string "don't()") <|> (Do <$ string "do()") <|> mul
    mul = do
      void $ string "mul("
      a <- decimal
      void $ char ','
      b <- decimal
      void $ char ')'
      return $ Mul a b
