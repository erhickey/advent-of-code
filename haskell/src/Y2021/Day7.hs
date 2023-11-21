module Y2021.Day7 (solve) where

solver :: (Int -> Int) -> [Int] -> Int
solver f xs = minimum $ map go [minimum xs..maximum xs]
  where go x = sum $ map (f . abs . (-)x) xs

summation :: Int -> Int
summation n = n * (n + 1) `div` 2

main = do
  input <- read . (\s -> "[" ++ s ++ "]") <$> readFile "day7.input"
  print . (++) "Part 1: " . show $ solver id input
  print . (++) "Part 2: " . show $ solver summation input

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
