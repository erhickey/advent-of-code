module Y2017.Day2 (solve) where

maxMinusMin :: [Int] -> Int
maxMinusMin xs = maximum xs - minimum xs

factorQuotient :: [Int] -> Int
factorQuotient xs = sum $ map go xs
  where go n = case factors of
          [] -> 0
          [x] -> div n x
          where factors = filter ((==0) . mod n) $ filter (/=n) xs

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ns = map (map read . words) . lines $ input
    part1 = show . sum $ map maxMinusMin ns
    part2 = show . sum $ map factorQuotient ns
