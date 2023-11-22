module Y2021.Day6 (solve) where

passDay :: [Int] -> [Int]
passDay [f0, f1, f2, f3, f4, f5, f6, f7, f8] = [f1, f2, f3, f4, f5, f6, f7 + f0, f8, f0]

initialize :: [Int] -> [Int]
initialize xs = map go [0..8]
  where go n = length $ filter (==n) xs

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ns = initialize . read . (\s -> "[" ++ s ++ "]") $ input
    part1 = show . sum $ iterate passDay ns !! 80
    part2 = show . sum $ iterate passDay ns !! 256
