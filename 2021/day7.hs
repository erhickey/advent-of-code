solve :: (Int -> Int) -> [Int] -> Int
solve f xs = minimum $ map go [minimum xs..maximum xs]
  where go x = sum $ map (f . abs . (-)x) xs

summation :: Int -> Int
summation n = n * (n + 1) `div` 2

main = do
  input <- read . (\s -> "[" ++ s ++ "]") <$> readFile "day7.input"
  print . (++) "Part 1: " . show $ solve id input
  print . (++) "Part 2: " . show $ solve summation input
