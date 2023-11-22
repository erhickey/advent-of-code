module Y2019.Day5 (solve) where

import Data.List.Split (splitOn)

import Y2019.Intcode (ProgramState(..), runIntcode)

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ns = map read $ splitOn "," input
    p1 = ProgramState ns [1] [] 0 0
    p2 = ProgramState ns [5] [] 0 0
    part1 = show . psOutput $ runIntcode p1
    part2 = show . psOutput $ runIntcode p2
