massToFuel :: Int -> Int
massToFuel = subtract 2 . flip div 3

fuelScan :: Int -> [Int]
fuelScan = drop 1 . takeWhile (> 0) . iterate massToFuel

main = do
  input <- map read . lines <$> readFile "day1.input"
  print . (++) "Part 1: " . show . sum $ map massToFuel input
  print . (++) "Part 2: " . show . sum $ concatMap fuelScan input
