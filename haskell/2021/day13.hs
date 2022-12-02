{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Data.Char (toUpper)
import Data.Either (rights)
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.List.Unique (uniq)
import qualified Data.Text as T (pack)
import Data.Attoparsec.Text (char, decimal, parseOnly, Parser, string)

type Coord = (Int, Int)
data Axis = X | Y deriving (Read)
type Fold = (Axis, Int)

parseInput :: String -> ([Coord], [Fold])
parseInput s = (map parseCoords $ lines cs, rights . map (parseOnly foldParser . T.pack) $ lines fs)
  where
    (cs:fs:_) = splitOn "\n\n" s
    parseCoords c = read $ "(" ++ c ++ ")"

foldParser :: Parser Fold
foldParser = do
  _ <- string "fold along "
  f <- char 'x' <|> char 'y'
  _ <- char '='
  n <- decimal
  pure (read [toUpper f], n)

fold :: [Coord] -> Fold -> [Coord]
fold cs (X, n) = map (\(x, y) -> (if x > n then n - (x - n) else x, y)) $ filter ((/=n) . fst) cs
fold cs (Y, n) = map (\(x, y) -> (x, if y > n then n - (y - n) else y)) $ filter ((/=n) . snd) cs

printGrid :: [Coord] -> [String]
printGrid cs = map (\y -> map (\x -> printChar (x, y)) [0..maxX]) [0..maxY]
  where
    maxX = maximum $ map fst cs
    maxY = maximum $ map snd cs
    printChar c
      | c `elem` cs = '#'
      | otherwise = ' '

main = do
  (cs, fs) <- parseInput <$> readFile "day13.input"
  print . (++) "Part 1: " . show . length . uniq . sort . fold cs $ head fs
  print "Part 2:"
  mapM_ print . printGrid $ foldl fold cs fs
