module Y2019.Day4 (solve) where

import Data.List (group, sort)

validSequence :: Ord a => [a] -> Bool
validSequence s = sort s == s

hasAdjacentDupe :: Eq a => [a] -> Bool
hasAdjacentDupe = any ((> 1) . length) . group

hasCouple :: Eq a => [a] -> Bool
hasCouple = elem 2 . map length . group

main = do
  let valid = filter (\s -> hasAdjacentDupe s && validSequence s) $ map show [240298..784956]
  print . (++) "Part 1: " . show $ length valid
  print . (++) "Part 2: " . show . length $ filter hasCouple valid

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
