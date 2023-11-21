module Y2020.Day4 (solve) where

import Data.Bifunctor (second)

import Data.List.Split (splitOn)
import Text.Regex.Posix ((=~))

parsePassport :: [String] -> [(String, String)]
parsePassport = map (second tail . break (==':'))

isValid :: [(String, String)] -> Bool
isValid xs = all (`elem` map fst xs) rs
  where rs = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

stripInvalid :: [(String, String)] -> [(String, String)]
stripInvalid = foldl go []
  where valid ("byr", s) = s =~ "^[1][9][2-9][0-9]$|^[2][0][0][0-2]$"
        valid ("iyr", s) = s =~ "^[2][0][1][0-9]$|^[2][0][2][0]$"
        valid ("eyr", s) = s =~ "^[2][0][2][0-9]$|^[2][0][3][0]$"
        valid ("hgt", s) = s =~ "^[1][5-8][0-9]cm$|[1][9][0-3]cm$|[5][9]in$|[6][0-9]in$|[7][0-6]in$"
        valid ("hcl", s) = s =~ "^#[0-9a-f]{6}$"
        valid ("ecl", s) = s `elem` ["amb","blu","brn","gry","grn","hzl","oth"]
        valid ("pid", s) = s =~ "^[0-9]{9}$"
        valid _ = False
        go acc x
          | valid x = x:acc
          | otherwise = acc

main = do
  ps <- map (parsePassport . words) . splitOn "\n\n" <$> readFile "day4.input"
  print . (++) "Part 1: " . show . length $ filter isValid ps
  print . (++) "Part 2: " . show . length . filter isValid $ map stripInvalid ps

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
