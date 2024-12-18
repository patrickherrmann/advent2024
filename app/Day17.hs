{-# LANGUAGE StrictData #-}
module Day17 where

import Parsing
import Data.Bits
import Data.Array.Unboxed
import Control.Monad (guard)

part1 :: String -> String
part1 = show . run . parseInput

part2 :: String -> String
part2 = show . findA . parseInput

run :: Machine -> [Int]
run m
  | not $ inRange (bounds $ instructions m) (ip m) = []
  | otherwise = case opcode of
      0 -> run $ m { a = a m `shiftR` combo, ip = ip m + 2 }
      1 -> run $ m { b = b m `xor` operand, ip = ip m + 2 }
      2 -> run $ m { b = combo .&. 7, ip = ip m + 2 }
      3 -> run $ if a m == 0 then m { ip = ip m + 2 } else m { ip = operand }
      4 -> run $ m { b = b m `xor` c m, ip = ip m + 2 }
      5 -> combo .&. 7 : (run $ m { ip = ip m + 2 })
      6 -> run $ m { b = a m `shiftR` combo, ip = ip m + 2 }
      7 -> run $ m { c = a m `shiftR` combo, ip = ip m + 2 }
    where
      opcode = instructions m ! ip m
      operand = instructions m ! (ip m + 1)
      combo = case operand of
        4 -> a m
        5 -> b m
        6 -> c m
        n -> n

findA :: Machine -> Int
findA m = minimum $ go (elems $ instructions m)
  where
    go [] = [0]
    go is = do
      highBits <- go (tail is)
      lowBits <- [0..7]
      let a = (highBits `shiftL` 3) .|. lowBits
      guard $ (run m { a = a }) == is
      return a

data Machine = Machine { a, b, c, ip :: Int, instructions :: UArray Int Int }

parseInput :: String -> Machine
parseInput = parseUnsafe $ do 
  a <- string "Register A: " *> decimal <* newline
  b <- string "Register B: " *> decimal <* newline
  c <- string "Register C: " *> decimal <* newline
  void newline
  is <- string "Program: " *> (decimal `sepBy` char ',')
  let n = length is
  return $ Machine a b c 0 (listArray (0, n - 1) is)
