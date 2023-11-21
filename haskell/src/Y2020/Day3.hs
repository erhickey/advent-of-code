module Y2020.Day3 (solve) where

parseLine :: String -> [Int]
parseLine = map parse
  where parse '.' = 0
        parse '#' = 1

hits :: [[Int]] -> Int -> Int -> Int
hits trees run rise = sum $ zipWith go [0,run..] [0,rise..length trees -1]
  where go x y = trees !! y !! (x `mod` l)
        l = length $ head trees

main = do
  trees <- map parseLine . lines <$> readFile "day3.input"
  print . (++) "Part 1: " . show $ hits trees 3 1
  print . (++) "Part 2: " . show . product $
    [ hits trees 1 1
    , hits trees 3 1
    , hits trees 5 1
    , hits trees 7 1
    , hits trees 1 2
    ]

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
