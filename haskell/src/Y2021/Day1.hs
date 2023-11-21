module Y2021.Day1 (solve) where

slidingWindow :: [a] -> [[a]]
slidingWindow = reverse . go []
  where
    go acc (x:y:z:xs) = go ([x,y,z]:acc) (y:z:xs)
    go acc _ = acc

solver :: [Int] -> Int
solver xs@(x:_) = sum $ zipWith (\x y -> fromEnum $ x > y) xs (x:xs)

main = do
  input <- map read . lines <$> readFile "day1.input"
  print . (++) "Part 1: " . show $ solver input
  print . (++) "Part 2: " . show . solver . map sum $ slidingWindow input

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
