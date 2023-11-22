module Y2021.Day1 (solve) where

slidingWindow :: [a] -> [[a]]
slidingWindow = reverse . go []
  where
    go acc (x:y:z:xs) = go ([x,y,z]:acc) (y:z:xs)
    go acc _ = acc

solver :: [Int] -> Int
solver xs@(x:_) = sum $ zipWith (\x y -> fromEnum $ x > y) xs (x:xs)

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ns = map read $ lines input
    part1 = show $ solver ns
    part2 = show . solver . map sum $ slidingWindow ns
