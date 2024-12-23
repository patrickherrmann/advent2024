module Day24 where

import Parsing
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.List

part1 :: String -> String
part1 = show . readReg 'z' . parseInput

part2 :: String -> String
part2 = const $ intercalate "," $ sort (swaps >>= \(a, b) -> [a, b])

-- determined manually using below utility functions
swaps :: [(String, String)]
swaps = [("z05", "gdd"), ("pqt", "z37"), ("cwt", "z09"), ("css", "jmv")]

perfSwaps :: Device -> Device
perfSwaps d = foldr (uncurry swap) d swaps

badOutputPorts :: Device -> [String]
badOutputPorts d = map fst $ filter snd $ zip (regPorts 'z' d) badBits
  where
    expected = readReg 'x' d + readReg 'y' d
    actual = readReg 'z' d
    badBits = zipWith (/=) (toBits expected) (toBits actual)

badOutputPortsFor :: Int -> Int -> Device -> [String]
badOutputPortsFor a b = badOutputPorts . writeReg 'x' a . writeReg 'y' b

add :: Int -> Int -> Device -> Int
add a b = readReg 'z' . writeReg 'x' a . writeReg 'y' b

type Device = Map String Port
type Reg = Char -- x, y, z
data Op = OR | AND | XOR deriving (Show)
data Port = L Bool | Gate Op String String deriving (Show)

regPorts :: Reg -> Device -> [String]
regPorts c = sort . filter (\n -> head n == c) . Map.keys

fromBits :: [Bool] -> Int
fromBits = foldr (\b n -> n * 2 + fromEnum b) 0

toBits :: Int -> [Bool]
toBits = unfoldr (\n -> Just (n `mod` 2 == 1, n `div` 2))

readReg :: Reg -> Device -> Int
readReg c d = fromBits $ map (signal d) (regPorts c d)

writeReg :: Reg -> Int -> Device -> Device
writeReg c n d = Map.union (Map.fromList newInitWires) d
  where newInitWires = zip (regPorts c d) (map L (toBits n))

signal :: Device -> String -> Bool
signal d n = case d ! n of
  L x -> x
  Gate o a b -> case o of
    OR -> signal d a || signal d b
    AND -> signal d a && signal d b
    XOR -> signal d a /= signal d b

upstream :: Device -> String -> [String]
upstream d n = case d ! n of
  L _ -> []
  Gate _ a b -> nub $ [a, b] ++ upstream d a ++ upstream d b

swap :: String -> String -> Device -> Device
swap a b d = Map.insert a (d ! b) $ Map.insert b (d ! a) d

parseInput :: String -> Device
parseInput = parseUnsafe $ do
    initWires <- initWire `sepEndBy` newline
    newline
    gates <- gate `sepEndBy` newline
    return $ Map.fromList $ initWires ++ gates
  where
    name = many alphaNumChar
    bool = string "0" *> return False
      <|> string "1" *> return True
    op = string "AND" *> return AND
      <|> string "OR" *> return OR
      <|> string "XOR" *> return XOR
    initWire = do
      n <- name
      string ": "
      w <- bool
      return (n, L w)
    gate = do
      a <- name
      o <- char ' ' *> op <* char ' '
      b <- name
      string " -> "
      n <- name
      return (n, Gate o a b)
