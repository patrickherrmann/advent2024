module Day24 where

import Parsing
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.List
import Text.Printf
import Data.MemoTrie
import Data.Maybe
import Data.Functor
import Control.Monad (msum)

part1 :: String -> String
part1 = show . readReg 'z' . parseInput

part2 :: String -> String
part2 = intercalate "," . sort . repair . parseInput

type Device = Map String Wire
data Wire = Input Bool | Gate Op String String deriving (Show)
data Op = OR | AND | XOR deriving (Show, Eq)
type Swap = (String, String)

instance Eq Wire where
  Gate o a b == Gate o' a' b' = o == o' && ((a == a' && b == b') || (a == b' && b == a'))
  _ == _ = False

repair :: Device -> [String]
repair = go []
  where
    go swapped d = case adder (wireNames d) 44 of
      Right ("z44", "z45") -> swapped
      Right (z, cout) -> z:cout:swapped
      Left (a, b) -> go (a:b:swapped) (swap a b d)

adder :: [(Wire, String)] -> Int -> Either Swap (String, String)
adder wn = \case
  0 -> do
    z <- findGate (Gate XOR (x_ 0) (y_ 0))
    cout <- findGate (Gate AND (x_ 0) (y_ 0))
    return (z, cout)
  n -> do
    (_, cin) <- adder wn (n - 1)
    s <- findGate (Gate XOR (x_ n) (y_ n))
    z <- findGate (Gate XOR s cin)
    dcarry <- findGate (Gate AND (x_ n) (y_ n))
    icarry <- findGate (Gate AND s cin)
    cout <- findGate (Gate OR dcarry icarry)
    return (z, cout)
  where
    x_ = ('x':) . printf "%02d"
    y_ = ('y':) . printf "%02d"
    findGate g = case lookup g wn of
      Just n -> Right n
      Nothing -> Left $ fromJust $ msum [matchGate g w | (w, _) <- wn]

matchGate :: Wire -> Wire -> Maybe Swap
matchGate (Gate op a b) = \case
  Gate op' a' b' | op' == op && a' == a -> Just (b, b')
  Gate op' a' b' | op' == op && b' == a -> Just (b, a')
  Gate op' a' b' | op' == op && a' == b -> Just (a, b')
  Gate op' a' b' | op' == op && b' == b -> Just (a, a')
  _ -> Nothing

regWires :: Char -> Device -> [String]
regWires c = sort . filter (\n -> head n == c) . Map.keys

fromBits :: [Bool] -> Int
fromBits = foldr (\b n -> n * 2 + fromEnum b) 0

readReg :: Char -> Device -> Int
readReg c d = fromBits $ map (probe d) (regWires c d)

probe :: Device -> String -> Bool
probe d = go
  where
    go = memo $ \n -> case d ! n of
      Input x -> x
      Gate o a b -> case o of
        OR -> go a || go b
        AND -> go a && go b
        XOR -> go a /= go b

swap :: String -> String -> Device -> Device
swap a b d = Map.insert a (d ! b) $ Map.insert b (d ! a) d

wireNames :: Device -> [(Wire, String)]
wireNames = map (\(a, b) -> (b, a)) . Map.toList

parseInput :: String -> Device
parseInput = parseUnsafe $ do
    inputs <- input `sepEndBy` newline
    void newline
    gates <- gate `sepEndBy` newline
    return $ Map.fromList $ inputs ++ gates
  where
    name = some alphaNumChar
    bool = string "0" $> False <|> string "1" $> True
    op = string "AND" $> AND <|> string "OR" $> OR <|> string "XOR" $> XOR
    input = do
      n <- name <* string ": "
      b <- bool
      pure (n, Input b)
    gate = do
      a <- name
      o <- space *> op <* space
      b <- name
      n <- string " -> " *> name
      pure (n, Gate o a b)
