module Y2021.Day7 (solve) where

solver :: (Int -> Int) -> [Int] -> Int
solver f xs = minimum $ map go [minimum xs..maximum xs]
  where go x = sum $ map (f . abs . (-)x) xs

summation :: Int -> Int
summation n = n * (n + 1) `div` 2

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ns = read . (\s -> "[" ++ s ++ "]") $ input
    part1 = show $ solver id ns
    part2 = show $ solver summation ns
