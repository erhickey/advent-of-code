module Y2018.Day4 (solve) where

-- {-# LANGUAGE OverloadedStrings #-}
-- import Control.Applicative ((<|>))
-- import Data.Bifunctor (second)
-- import qualified Data.Text.IO as T (readFile)
-- import qualified Data.Text as T (lines)
-- import Data.Either (rights)
-- import Data.List (group, maximumBy, sort)
-- import qualified Data.IntMap as M (empty, insertWith, IntMap, toList)
-- import Data.Ord (comparing)

-- import Data.Attoparsec.Text (anyChar, count, decimal, digit, parseOnly, Parser, string)

-- data LogEntry = Sleep Int | Wake Int | Start Int

-- logMinuteParser :: Parser Int
-- logMinuteParser = do
--   _ <- count 15 anyChar
--   read <$> count 2 digit

-- logActionParser :: Int -> Parser LogEntry
-- logActionParser n = do
--       string "wakes up" >> return (Wake n)
--   <|> (string "falls asleep" >> return (Sleep n))
--   <|> (string "Guard #" >> Start <$> decimal)

-- logEntryParser :: Parser LogEntry
-- logEntryParser = do
--   minute <- logMinuteParser
--   string "] "
--   logActionParser minute

-- readLogEntries :: [LogEntry] -> M.IntMap [Int]
-- readLogEntries = snd . foldl go (0, M.empty)
--   where go (_, m) (Start g) = (g, m)
--         go (g, m) (Sleep s) = (g, M.insertWith (++) g [s] m)
--         go (g, m) (Wake w) = (g, M.insertWith (\[y] (x:xs) -> [x..y-1] ++ xs) g [w] m)

-- mostFrequent :: Ord a => [a] -> [a]
-- mostFrequent = maximumBy (comparing length) . group . sort

-- main = do
--   input <- M.toList . readLogEntries . rights . map (parseOnly logEntryParser) . sort . T.lines <$> T.readFile "day4.input"
--   let (p1Guard, p1Minutes) = maximumBy (comparing $ length . snd) input
--       (p2Guard, p2Minute:_) = maximumBy (comparing $ length . snd) $ map (second mostFrequent) input
--   print . (++) "Part 1: " . show $ p1Guard * head (mostFrequent p1Minutes)
--   print . (++) "Part 2: " . show $ p2Guard * p2Minute

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
