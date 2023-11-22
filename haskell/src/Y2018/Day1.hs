module Y2018.Day1 (solve) where

import Data.IntSet (empty, insert, member)
import Data.List (scanl')

parseLine :: String -> Int
parseLine ('+':xs) = parseLine xs
parseLine s = read s

-- Use an IntSet and lazy evaluation for a fast result
firstDupe :: [Int] -> Int
firstDupe xs = fst . head . filter (uncurry member) . zip xs . scanl (flip insert) empty $ xs

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ns = map parseLine $ lines input
    part1 = show . sum $ ns
    part2 = show . firstDupe . scanl' (+) 0 . cycle $ ns
