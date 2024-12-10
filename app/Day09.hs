module Day09 where

part1 :: String -> String
part1 input = show $ checksum $ take (sum fileSizes) condense
  where
    fileContent = concat $ zipWith replicate fileSizes [0..]
    fileSizes = everyOther ints
    ints = map (read . (: [])) input
    condense = go 0 ints (reverse fileContent)
    go fid (fs:ss:is) filler = replicate fs fid ++ take ss filler ++ go (fid + 1) is (drop ss filler)
    go fid (fs:[]) _ = replicate fs fid
    go _ [] _ = []

checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..]

everyOther :: [Int] -> [Int]
everyOther (fs:_:fss) = fs : everyOther fss
everyOther (fs:[]) = [fs]
everyOther _ = []

part2 :: String -> String
part2 _ = "Day 09b not implemented yet"
