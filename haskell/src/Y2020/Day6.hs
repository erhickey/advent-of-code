module Y2020.Day6 (solve) where

import Data.List (intersect, nub)

import Data.List.Split (splitOn)

main = do
  input <- splitOn "\n\n" <$> readFile "day6.input"
  print . (++) "Part 1: " . show . sum $ map (length . nub . filter (/='\n')) input
  print . (++) "Part 2: " . show . sum $ map ((\(x:xs) -> length $ foldl intersect x xs) . words) input

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
