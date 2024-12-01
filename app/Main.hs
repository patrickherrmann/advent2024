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
    writeFile ("output/Day" ++ day ++ "-" ++ part ++ ".txt") output
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
bothParts = ["part1", "part2"]

type Day = String
type Part = String
type DayPart = (Day, Part)
type Input = String
type Output = String
type Solution = Input -> Output

solutions :: Map DayPart Solution
solutions = fromList
  [ (("01", "part1"), Day01.part1)
  , (("01", "part2"), Day01.part2)
  , (("02", "part1"), Day02.part1)
  , (("02", "part2"), Day02.part2)
  , (("03", "part1"), Day03.part1)
  , (("03", "part2"), Day03.part2)
  , (("04", "part1"), Day04.part1)
  , (("04", "part2"), Day04.part2)
  , (("05", "part1"), Day05.part1)
  , (("05", "part2"), Day05.part2)
  , (("06", "part1"), Day06.part1)
  , (("06", "part2"), Day06.part2)
  , (("07", "part1"), Day07.part1)
  , (("07", "part2"), Day07.part2)
  , (("08", "part1"), Day08.part1)
  , (("08", "part2"), Day08.part2)
  , (("09", "part1"), Day09.part1)
  , (("09", "part2"), Day09.part2)
  , (("10", "part1"), Day10.part1)
  , (("10", "part2"), Day10.part2)
  , (("11", "part1"), Day11.part1)
  , (("11", "part2"), Day11.part2)
  , (("12", "part1"), Day12.part1)
  , (("12", "part2"), Day12.part2)
  , (("13", "part1"), Day13.part1)
  , (("13", "part2"), Day13.part2)
  , (("14", "part1"), Day14.part1)
  , (("14", "part2"), Day14.part2)
  , (("15", "part1"), Day15.part1)
  , (("15", "part2"), Day15.part2)
  , (("16", "part1"), Day16.part1)
  , (("16", "part2"), Day16.part2)
  , (("17", "part1"), Day17.part1)
  , (("17", "part2"), Day17.part2)
  , (("18", "part1"), Day18.part1)
  , (("18", "part2"), Day18.part2)
  , (("19", "part1"), Day19.part1)
  , (("19", "part2"), Day19.part2)
  , (("20", "part1"), Day20.part1)
  , (("20", "part2"), Day20.part2)
  , (("21", "part1"), Day21.part1)
  , (("21", "part2"), Day21.part2)
  , (("22", "part1"), Day22.part1)
  , (("22", "part2"), Day22.part2)
  , (("23", "part1"), Day23.part1)
  , (("23", "part2"), Day23.part2)
  , (("24", "part1"), Day24.part1)
  , (("24", "part2"), Day24.part2)
  , (("25", "part1"), Day25.part1)
  , (("25", "part2"), Day25.part2)
  ]
