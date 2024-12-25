module Day24 where

import Parsing
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.List
import Text.Printf

part1 :: String -> String
part1 = show . readReg 'z' . parseInput

part2 :: String -> String
part2 = intercalate "," . sort . (>>= \(a, b) -> [a, b]) . repair . parseInput

type Device = Map String Port
type Reg = Char -- x, y, z
data Op = OR | AND | XOR deriving (Show, Eq)
data Port = L Bool | Gate Op String String deriving (Show)
type Swap = (String, String)

instance Eq Port where
  L x == L y = x == y
  Gate o a b == Gate o' a' b' = o == o' && ((a == a' && b == b') || (a == b' && b == a'))
  _ == _ = False

repair :: Device -> [Swap]
repair d = go [] d
  where
    go swaps d' = case adder (portNames d') 44 of
      Right _ -> swaps
      Left (a, b) -> go ((a, b) : swaps) (swap a b d')

adder :: [(Port, String)] -> Int -> Either Swap (String, String)
adder pn = \case
  0 -> do
    z <- findGate (Gate XOR (x_ 0) (y_ 0)) pn
    cout <- findGate (Gate AND (x_ 0) (y_ 0)) pn
    return (z, cout)
  n -> do
    (_, cin) <- adder pn (n - 1)
    s <- findGate (Gate XOR (x_ n) (y_ n)) pn
    z <- findGate (Gate XOR s cin) pn
    dcarry <- findGate (Gate AND (x_ n) (y_ n)) pn
    icarry <- findGate (Gate AND s cin) pn
    cout <- findGate (Gate OR dcarry icarry) pn
    return (z, cout)

findGate :: Port -> [(Port, String)] -> Either Swap String
findGate p pn = case lookup p pn of
  Just n -> Right n
  Nothing -> Left $ findSwap p (map fst pn)

findSwap :: Port -> [Port] -> Swap
findSwap p = \case
  [] -> error "no swap found"
  g:gs -> case matchGate p g of
    Just s -> s
    Nothing -> findSwap p gs

matchGate :: Port -> Port -> Maybe Swap
matchGate (Gate op a b) = \case
  Gate op' a' b' | op' == op && a' == a -> Just (b, b')
  Gate op' a' b' | op' == op && b' == a -> Just (b, a')
  Gate op' a' b' | op' == op && a' == b -> Just (a, b')
  Gate op' a' b' | op' == op && b' == b -> Just (a, a')
  _ -> Nothing

x_ :: Int -> String
x_ = ('x':) . printf "%02d"

y_ :: Int -> String
y_ = ('y':) . printf "%02d"

z_ :: Int -> String
z_ = ('z':) . printf "%02d"

regPorts :: Reg -> Device -> [String]
regPorts c = sort . filter (\n -> head n == c) . Map.keys

fromBits :: [Bool] -> Int
fromBits = foldr (\b n -> n * 2 + fromEnum b) 0

readReg :: Reg -> Device -> Int
readReg c d = fromBits $ map (readPort d) (regPorts c d)

readPort :: Device -> String -> Bool
readPort d n = case d ! n of
  L x -> x
  Gate o a b -> case o of
    OR -> readPort d a || readPort d b
    AND -> readPort d a && readPort d b
    XOR -> readPort d a /= readPort d b

swap :: String -> String -> Device -> Device
swap a b d = Map.insert a (d ! b) $ Map.insert b (d ! a) d

portNames :: Device -> [(Port, String)]
portNames = map swap . Map.toList
  where swap (n, p) = (p, n)

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
