module Day02 where

part1 :: String -> String
part1 = show . length . filter safe . parseReports

part2 :: String -> String
part2 = show . length . filter (\r -> safe r || any safe (dampenings r)) . parseReports

safe :: Report -> Bool
safe r = all safeInc r' || all safeDec r'
  where
    r' = zipWith subtract r (tail r)
    safeInc dl = dl >= 1 && dl <= 3
    safeDec dl = dl <= -1 && dl >= -3

-- All the ways to skip a level
dampenings :: Report -> [Report]
dampenings = \case
  [] -> []
  (l:ls) -> ls : map (l :) (dampenings ls)

type Report = [Int]

parseReports :: String -> [Report]
parseReports = map report . lines
  where
    report = map read . words
