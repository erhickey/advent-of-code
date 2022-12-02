slidingWindow :: [a] -> [[a]]
slidingWindow = reverse . go []
  where
    go acc (x:y:z:xs) = go ([x,y,z]:acc) (y:z:xs)
    go acc _ = acc

solve :: [Int] -> Int
solve xs@(x:_) = sum $ zipWith (\x y -> fromEnum $ x > y) xs (x:xs)

main = do
  input <- map read . lines <$> readFile "day1.input"
  print . (++) "Part 1: " . show $ solve input
  print . (++) "Part 2: " . show . solve . map sum $ slidingWindow input
