module Y2019.Day9 (solve) where

import Data.List.Split (splitOn)

import Y2019.Intcode (ProgramState(..), runIntcodeXMem)

boost :: [Int] -> Int -> [Int]
boost xs n = psOutput . runIntcodeXMem 1 $ ProgramState xs [n] [] 0 0

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ns = map read $ splitOn "," input
    part1 = show $ boost ns 1
    part2 = show $ boost ns 2
