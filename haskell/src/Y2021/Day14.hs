{-# LANGUAGE OverloadedStrings #-}

module Y2021.Day14 (solve) where

import Data.Either (rights)
import Data.List.Split (splitOn)
import Data.Map.Strict ((!), Map)
import qualified Data.Map.Strict as M (alter, assocs, empty, fromList, toList)
import qualified Data.Text as T (pack)
import Data.Attoparsec.Text (anyChar, manyTill, parseOnly, Parser, string)

type Rules = Map String Char
type Pairs = Map String Int
type Chars = Map Char Int
type Polymer = (Pairs, Chars)

parse :: String -> (Polymer, Rules)
parse s = ((countMap $ pairs t, countMap t), rules)
  where
    (t:r:_) = splitOn "\n\n" s
    rules = M.fromList . rights . map (parseOnly ruleParser . T.pack) $ lines r

countMap :: Ord a => [a] -> Map a Int
countMap = foldl (addNToCountMap 1) M.empty

addNToCountMap :: Ord a => Int -> Map a Int -> a -> Map a Int
addNToCountMap n m k = M.alter (pure . maybe n (+n)) k m

pairs :: String -> [String]
pairs = go []
  where
    go acc [] = acc
    go acc [_] = acc
    go acc (x:y:ys) = go ([x,y]:acc) (y:ys)

ruleParser :: Parser (String, Char)
ruleParser = do
  s <- manyTill anyChar $ string " -> "
  c <- anyChar
  pure (s, c)

step :: Rules -> Polymer -> Polymer
step rs (ps, cs) = foldl go (M.empty, cs) $ M.assocs ps
  where
    go (ps', cs') (pair, n) = (addNToCountMap n (addNToCountMap n ps' pair1) pair2, addNToCountMap n cs' c)
        where
          c = rs ! pair
          (pair1, pair2) = let (p1:p2:_) = pair in ([p1,c], [c,p2])

answer :: Polymer -> Int
answer (_, cs) = maximum ls - minimum ls
  where ls = map snd $ M.toList cs

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    (polymer, rules) = parse input
    steps = iterate (step rules) polymer
    part1 = show . answer $ steps !! 10
    part2 = show . answer $ steps !! 40
