module Main where

import System.Environment (getArgs)
import Data.Map (Map, fromList, (!))
import Data.Foldable (for_)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25

main :: IO ()
main = do
  dayParts <- parseArgs <$> getArgs
  for_ dayParts $ \(day, part) ->  do
    putStr $ "Running day " ++ day ++ part ++ "..."
    input <- readFile $ "input/Day" ++ day ++ ".txt"
    let output = (solutions ! (day, part)) input
    writeFile ("output/Day" ++ day ++ part ++ ".txt") output
    putStrLn "done."

parseArgs :: [String] -> [(Day, Part)]
parseArgs = \case
  [day, part] -> [(day, part)]
  [day] -> (day,) <$> bothParts
  [] -> (,) <$> allDays <*> bothParts
  _ -> error "Usage: advent-of-code [day] [part]"

allDays :: [String]
allDays = map twoDigits [1..25]
  where
    twoDigits :: Int -> String
    twoDigits n = if n < 10 then "0" ++ show n else show n

bothParts :: [String]
bothParts = ["a", "b"]

type Day = String
type Part = String
type DayPart = (Day, Part)
type Input = String
type Output = String
type Solution = Input -> Output

solutions :: Map DayPart Solution
solutions = fromList
  [ (("01", "a"), Day01.a)
  , (("01", "b"), Day01.b)
  , (("02", "a"), Day02.a)
  , (("02", "b"), Day02.b)
  , (("03", "a"), Day03.a)
  , (("03", "b"), Day03.b)
  , (("04", "a"), Day04.a)
  , (("04", "b"), Day04.b)
  , (("05", "a"), Day05.a)
  , (("05", "b"), Day05.b)
  , (("06", "a"), Day06.a)
  , (("06", "b"), Day06.b)
  , (("07", "a"), Day07.a)
  , (("07", "b"), Day07.b)
  , (("08", "a"), Day08.a)
  , (("08", "b"), Day08.b)
  , (("09", "a"), Day09.a)
  , (("09", "b"), Day09.b)
  , (("10", "a"), Day10.a)
  , (("10", "b"), Day10.b)
  , (("11", "a"), Day11.a)
  , (("11", "b"), Day11.b)
  , (("12", "a"), Day12.a)
  , (("12", "b"), Day12.b)
  , (("13", "a"), Day13.a)
  , (("13", "b"), Day13.b)
  , (("14", "a"), Day14.a)
  , (("14", "b"), Day14.b)
  , (("15", "a"), Day15.a)
  , (("15", "b"), Day15.b)
  , (("16", "a"), Day16.a)
  , (("16", "b"), Day16.b)
  , (("17", "a"), Day17.a)
  , (("17", "b"), Day17.b)
  , (("18", "a"), Day18.a)
  , (("18", "b"), Day18.b)
  , (("19", "a"), Day19.a)
  , (("19", "b"), Day19.b)
  , (("20", "a"), Day20.a)
  , (("20", "b"), Day20.b)
  , (("21", "a"), Day21.a)
  , (("21", "b"), Day21.b)
  , (("22", "a"), Day22.a)
  , (("22", "b"), Day22.b)
  , (("23", "a"), Day23.a)
  , (("23", "b"), Day23.b)
  , (("24", "a"), Day24.a)
  , (("24", "b"), Day24.b)
  , (("25", "a"), Day25.a)
  , (("25", "b"), Day25.b)
  ]
