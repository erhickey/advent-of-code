module Y2019.Day4 (solve) where

import Data.List (group, sort)

import Data.List.Split (splitOn)

validSequence :: Ord a => [a] -> Bool
validSequence s = sort s == s

hasAdjacentDupe :: Eq a => [a] -> Bool
hasAdjacentDupe = any ((> 1) . length) . group

hasCouple :: Eq a => [a] -> Bool
hasCouple = elem 2 . map length . group

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    [s, e] = splitOn "-" input
    valid = filter (\s -> hasAdjacentDupe s && validSequence s) $ map show [(read s::Int)..(read e::Int)]
    part1 = show $ length valid
    part2 = show . length $ filter hasCouple valid
