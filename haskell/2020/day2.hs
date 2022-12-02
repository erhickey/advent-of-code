{-# LANGUAGE OverloadedStrings #-}
import Data.Either (rights)
import qualified Data.Text.IO as T (readFile)
import qualified Data.Text as T (lines)

import Data.Attoparsec.Text (char, decimal, letter, many', parseOnly, Parser, string)

type Password = (Int, Int, Char, String)

lineParser :: Parser Password
lineParser = do
  low <- decimal
  char '-'
  high <- decimal
  char ' '
  c <- letter
  string ": "
  s <- many' letter
  return (low, high, c, s)

isValid :: Password -> Bool
isValid (l, h, c, s) = freq >= l && freq <= h
  where freq = length $ filter (==c) s

isValid2 :: Password -> Bool
isValid2 (l, h, c, s)
  | c1 == c = c2 /= c
  | c2 == c = True
  | otherwise = False
  where c1 = s !! (l - 1)
        c2 = s !! (h - 1)

main = do
  input <- rights . map (parseOnly lineParser) . T.lines <$> T.readFile "day2.input"
  print . (++) "Part 1: " . show . length $ filter isValid input
  print . (++) "Part 2: " . show . length $ filter isValid2 input
