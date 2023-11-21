module Y2017.Day2 (solve) where

maxMinusMin :: [Int] -> Int
maxMinusMin xs = maximum xs - minimum xs

factorQuotient :: [Int] -> Int
factorQuotient xs = sum $ map go xs
  where go n = case factors of
          [] -> 0
          [x] -> div n x
          where factors = filter ((==0) . mod n) $ filter (/=n) xs

main = do
  input <- map (map read . words) . lines <$> readFile "day2.input"
  print . (++) "Part 1: " . show . sum $ map maxMinusMin input
  print . (++) "Part 2: " . show . sum $ map factorQuotient input

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
