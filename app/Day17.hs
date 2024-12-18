{-# LANGUAGE StrictData #-}
module Day17 where

import Parsing
import Data.Bits
import Data.Array.Unboxed
import Data.Foldable (toList)
import Data.List (find)
import Data.Maybe (fromJust)

part1 :: String -> String
part1 = show . run . parseInput

part2 :: String -> String
part2 = show . a . fromJust . find quine . variations . parseInput

variations :: Machine -> [Machine]
variations m = [m { a = a' } | a' <- [0..]]

quine :: Machine -> Bool
quine m = run m == elems (instructions m)

run :: Machine -> [Int]
run m
  | not $ inRange (bounds $ instructions m) (ip m) = []
  | otherwise = case opcode m of
      0 -> run $ m { a = a m `div` (2 ^ combo m), ip = ip m + 2 }
      1 -> run $ m { b = b m `xor` operand m, ip = ip m + 2 }
      2 -> run $ m { b = combo m `rem` 8, ip = ip m + 2 }
      3 -> run $ if a m == 0 then m { ip = ip m + 2 } else m { ip = operand m }
      4 -> run $ m { b = b m `xor` c m, ip = ip m + 2 }
      5 -> combo m `rem` 8 : (run $ m { ip = ip m + 2 })
      6 -> run $ m { b = a m `div` (2 ^ combo m), ip = ip m + 2 }
      7 -> run $ m { c = a m `div` (2 ^ combo m), ip = ip m + 2 }

opcode :: Machine -> Int
opcode m = instructions m ! ip m

operand :: Machine -> Int
operand m = instructions m ! (ip m + 1)
    
combo :: Machine -> Int
combo m = case operand m of
  4 -> a m
  5 -> b m
  6 -> c m
  n -> n

data Machine = Machine { a :: Int, b :: Int, c :: Int, ip :: Int, instructions :: UArray Int Int }
  deriving (Show, Eq)

parseInput :: String -> Machine
parseInput = parseUnsafe $ do 
  a <- string "Register A: " *> decimal <* newline
  b <- string "Register B: " *> decimal <* newline
  c <- string "Register C: " *> decimal <* newline
  void newline
  is <- string "Program: " *> (decimal `sepBy` char ',')
  let n = length is
  return $ Machine a b c 0 (listArray (0, n - 1) is)
