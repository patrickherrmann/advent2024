module Day02 where

part1 :: String -> String
part1 = show . length . filter safe . map diffs . parseReports
  where
    diffs r = zipWith subtract r (tail r)
    safe r' = all safeInc r' || all safeDec r'
    safeInc dl = dl >= 1 && dl <= 3
    safeDec dl = dl <= -1 && dl >= -3

part2 :: String -> String
part2 _ = "Day 02b not implemented yet"

parseReports :: String -> [[Int]]
parseReports = map report . lines
  where
    report = map read . words
