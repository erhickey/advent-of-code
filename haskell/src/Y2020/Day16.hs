{-# LANGUAGE OverloadedStrings #-}

module Y2020.Day16 (solve) where

import Control.Applicative (getZipList, many, ZipList(..))
import Data.Bifunctor (second)
import Data.List (isPrefixOf, partition)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S (delete, elemAt, fromList, intersection, size)
import qualified Data.Text as T (pack)

import Data.Attoparsec.Text (anyChar, char, decimal, manyTill, parseOnly, Parser, sepBy, string)

data RuleBounds = RuleBounds
  { rbMin :: Int
  , rbMax :: Int
  }
  deriving (Eq, Ord)

data Rule = Rule
  { ruleName :: String
  , rPos     :: Int
  , rBounds1 :: RuleBounds
  , rBounds2 :: RuleBounds
  }
  deriving (Eq, Ord)

type Ticket = [Int]

type Field = (Int, String)

inputParser :: Parser ([Rule], Ticket, [Ticket])
inputParser = do
  rs <- manyTill ruleParser $ string "\n"
  string "your ticket:\n"
  t <- ticketParser
  string "\nnearby tickets:\n"
  ts <- many ticketParser
  pure (rs, t, ts)

ticketParser :: Parser [Int]
ticketParser = do
  ns <- decimal `sepBy` char ','
  char '\n'
  pure ns

ruleParser :: Parser Rule
ruleParser = do
  name <- manyTill anyChar $ string ": "
  min1 <- decimal
  char '-'
  max1 <- decimal
  string " or "
  min2 <- decimal
  char '-'
  max2 <- decimal
  char '\n'
  pure $ Rule name (-1) (RuleBounds min1 max1) (RuleBounds min2 max2)

isValidValue :: Int -> Rule -> Bool
isValidValue t r
  | t >= rbMin (rBounds1 r) && t <= rbMax (rBounds1 r) = True
  | t >= rbMin (rBounds2 r) && t <= rbMax (rBounds2 r) = True
  | otherwise = False

invalidValues :: [Rule] -> Ticket -> [Int]
invalidValues rs = mapMaybe go
  where
    go t
      | any (isValidValue t) rs = Nothing
      | otherwise = Just t

allInvalids :: [Rule] -> [Ticket] -> [Int]
allInvalids rs = concatMap (invalidValues rs)

isValidTicket :: [Rule] -> Ticket -> Bool
isValidTicket rs = all ((==True) . go)
  where go t = any (isValidValue t) rs

transpose :: [Ticket] -> [[Int]]
transpose = getZipList . traverse ZipList

determineFields :: [Rule] -> [Ticket] -> [Field]
determineFields rs ts = map (second ruleName) $ go [] is
  where
    -- transpose list of tickets to list of each field
    cs = zip [0..] $ transpose ts
    -- get valid rules for each field
    vs = map (second (map (validRules rs))) cs
    -- intersection of valid rules for each position
    is = map (second (foldr1 S.intersection)) vs
    -- determine position of each field by process of elimination
    go acc [] = acc
    go acc (x@(c, xs):ys)
      | S.size xs == 1 = go ((c, rule):acc) $ map (second (S.delete rule)) ys
      | otherwise = go acc $ ys ++ [x]
      where rule = S.elemAt 0 xs

validRules :: [Rule] -> Int -> Set Rule
validRules rs t = S.fromList $ mapMaybe go rs
  where
    go r
      | isValidValue t r = Just r
      | otherwise = Nothing

getDepartureFields :: [Field] -> [Int]
getDepartureFields = map fst . filter (("departure" `isPrefixOf`) . snd)

getFields :: Ticket -> [Int] -> [Int]
getFields t = map (t !!)

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    Right (rs, t, ts) = parseOnly inputParser $ T.pack input
    (valid, invalid) = partition (isValidTicket rs) ts
    part1 = show . sum $ allInvalids rs invalid
    part2 = show . product . getFields t . getDepartureFields $ determineFields rs (t:valid)
