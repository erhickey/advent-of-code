{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Y2020.Day7 (solve) where

import Control.Applicative ((<|>))
import Data.Either (rights)
import Data.Map ((!), Map)
import qualified Data.Map as M (empty, insertWith, notMember)
import qualified Data.Text as T (lines, pack)
import Data.Set (Set)
import qualified Data.Set as S (empty, insert, size, union)

import Data.Attoparsec.Text (anyChar, char, decimal,  manyTill, parseOnly, Parser, string)

lineParser :: Parser [(String, (Int, String))]
lineParser = do
  b <- manyTill anyChar $ string " bags contain "
  map (b,) <$> bagParser

bagParser :: Parser [(Int, String)]
bagParser = do
  n <- decimal
  char ' '
  b <- manyTill anyChar $ string " bag"
  sepParser n b

sepParser :: Int -> String -> Parser [(Int, String)]
sepParser n b = do
  string "s, " >> ((n, b):) <$> bagParser
  <|> (string ", " >> ((n, b):) <$> bagParser)
  <|> (string "." >> return [(n, b)])
  <|> (string "s." >> return [(n, b)])

containers :: Map String [String] -> String -> Set String
containers m = go S.empty
  where go s b
          | b `M.notMember` m = s
          | otherwise = foldl (\acc x -> S.union acc (go acc x)) s' bs
          where s' = foldl (flip S.insert) s bs
                bs = m ! b

contained :: Map String [(Int, String)] -> String -> Int
contained m b
  | b `M.notMember` m = 0
  | otherwise = sum $ map go bs
  where bs = m ! b
        go (x, y) = x + x * contained m y

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    xs = concat . rights . map (parseOnly lineParser) . T.lines $ T.pack input
    p1m = foldl (\m (ob, (_, ib)) -> M.insertWith (++) ib [ob] m) M.empty xs
    p2m = foldl (\m (ob, ib) -> M.insertWith (++) ob [ib] m) M.empty xs
    part1 = show . S.size $ containers p1m "shiny gold"
    part2 = show $ contained p2m "shiny gold"
