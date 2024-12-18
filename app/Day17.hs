{-# LANGUAGE StrictData #-}
module Day17 where

import Parsing
import Data.Bits
import Data.Array.Unboxed
import Data.Foldable (toList)
import Data.List (find)
import Data.Maybe (fromJust)

part1 :: String -> String
part1 = show . reverse . output . run . parseInput

part2 :: String -> String
part2 = show . a . fromJust . find quine . variations . parseInput

variations :: Machine -> [Machine]
variations m = [m { a = a' } | a' <- [0..]]

quine :: Machine -> Bool
quine m = output (run m) == reverse (elems (instructions m))

run :: Machine -> Machine
run m
  | inRange (bounds $ instructions m) (ip m) = run $ step m
  | otherwise = m

step :: Machine -> Machine
step m = case opcode m of
    0 -> m { a = a m `div` (2 ^ combo m), ip = ip m + 2 }
    1 -> m { b = b m `xor` operand m, ip = ip m + 2 }
    2 -> m { b = combo m `rem` 8, ip = ip m + 2 }
    3 -> if a m == 0 then m { ip = ip m + 2 } else m { ip = operand m }
    4 -> m { b = b m `xor` c m, ip = ip m + 2 }
    5 -> m { output = (combo m `rem` 8) : output m, ip = ip m + 2 }
    6 -> m { b = a m `div` (2 ^ combo m), ip = ip m + 2 }
    7 -> m { c = a m `div` (2 ^ combo m), ip = ip m + 2 }

opcode :: Machine -> Int
opcode m = instructions m ! ip m

operand :: Machine -> Int
operand m = instructions m ! (ip m + 1)
    
combo :: Machine -> Int
combo m = case operand m of
  0 -> 0
  1 -> 1
  2 -> 2
  3 -> 3
  4 -> a m
  5 -> b m
  6 -> c m
  _ -> error "reserved"

data Machine = Machine { a :: Int, b :: Int, c :: Int, ip :: Int, output :: [Int], instructions :: UArray Int Int }
  deriving (Show)

parseInput :: String -> Machine
parseInput = parseUnsafe $ do 
  a <- string "Register A: " *> decimal <* newline
  b <- string "Register B: " *> decimal <* newline
  c <- string "Register C: " *> decimal <* newline
  void newline
  is <- string "Program: " *> (decimal `sepBy` char ',')
  let n = length is
  return $ Machine a b c 0 [] (listArray (0, n - 1) is)
