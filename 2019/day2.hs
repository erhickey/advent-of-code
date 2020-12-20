import Data.List.Split (splitOn)

import Intcode (runIntcodeEasy)

modIntcode :: [Int] -> Int -> Int -> [Int]
modIntcode (x:_:_:xs) n v = x:n:v:xs

part2 :: [Int] -> Int -> Int
part2 xs patt = head [ 100 * x + y | x <- [0..99], y <- [0..99], head (runIntcodeEasy $ modIntcode xs x y) == patt ]

main = do
  input <- map read . splitOn "," <$> readFile "day2.input"
  print . (++) "Part 1: " . show . head . runIntcodeEasy $ modIntcode input 12 2
  print . (++) "Part 2: " . show $ part2 input 19690720
