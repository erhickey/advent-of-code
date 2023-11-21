module Y2015.Day2 (solve) where

import Data.List (sort)

type Dimensions = (Int, Int, Int)

surfaceArea :: Dimensions -> Int
surfaceArea (min, med, max) = 2*min*med + 2*med*max + 2*max*min

slack :: Dimensions -> Int
slack (min, med, _) = min * med

ribbon :: Dimensions -> Int
ribbon (min, med, max) = (min*med*max) + (min + min + med + med)

parseDimensions :: String -> Dimensions
parseDimensions = (\(x1:x2:x3:_) -> (x1, x2, x3)) . sort . map read . split 'x'

split :: Char -> String -> [String]
split delim xs = words [if c == delim then ' ' else c | c <- xs ]

main = do
  dimensions <- map parseDimensions . lines <$> readFile "day2.input"
  print . (++) "Part 1: " . show $ (sum $ map surfaceArea dimensions) + (sum $ map slack dimensions)
  print . (++) "Part 2: " . show . sum $ map ribbon dimensions

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
