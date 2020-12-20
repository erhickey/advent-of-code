{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as BS (lines, readFile)
import Data.Either (rights)

import Data.Attoparsec.ByteString.Char8 (char, decimal, letter_ascii, many', parseOnly, Parser, string)

type Password = (Int, Int, Char, String)

lineParser :: Parser Password
lineParser = do
  low <- decimal
  char '-'
  high <- decimal
  char ' '
  c <- letter_ascii
  string ": "
  s <- many' letter_ascii
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
  input <- rights . map (parseOnly lineParser) . BS.lines <$> BS.readFile "day2.input"
  print . (++) "Part 1: " . show . length $ filter isValid input
  print . (++) "Part 2: " . show . length $ filter isValid2 input
