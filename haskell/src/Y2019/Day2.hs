module Y2019.Day2 (solve) where

import Data.List.Split (splitOn)

import Y2019.Intcode (runIntcodeEasy)

modIntcode :: [Int] -> Int -> Int -> [Int]
modIntcode (x:_:_:xs) n v = x:n:v:xs

p2 :: [Int] -> Int -> Int
p2 xs patt = head [ 100 * x + y | x <- [0..99], y <- [0..99], head (runIntcodeEasy $ modIntcode xs x y) == patt ]

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ns = map read $ splitOn "," input
    part1 = show . head . runIntcodeEasy $ modIntcode ns 12 2
    part2 = show $ p2 ns 19690720
