module Y2019.Day1 (solve) where

massToFuel :: Int -> Int
massToFuel = subtract 2 . flip div 3

fuelScan :: Int -> [Int]
fuelScan = drop 1 . takeWhile (> 0) . iterate massToFuel

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ns = map read $ lines input
    part1 = show . sum $ map massToFuel ns
    part2 = show . sum $ concatMap fuelScan ns
