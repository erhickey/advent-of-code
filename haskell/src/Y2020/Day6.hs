module Y2020.Day6 (solve) where

import Data.List (intersect, nub)

import Data.List.Split (splitOn)

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ps = splitOn "\n\n" input
    part1 = show . sum $ map (length . nub . filter (/='\n')) ps
    part2 = show . sum $ map ((\(x:xs) -> length $ foldl intersect x xs) . words) ps
