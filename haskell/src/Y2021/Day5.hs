module Y2021.Day5 (solve) where

-- {-# LANGUAGE OverloadedStrings #-}

-- import Control.Applicative (liftA2)
-- import Data.Bifunctor (bimap)
-- import Data.Either (rights)
-- import Data.List (group, sort)

-- import Data.Attoparsec.Text (char, decimal, parseOnly, Parser, string)
-- import qualified Data.Text as T (lines, pack)

-- type Point = (Int, Int)
-- type LineSegment = (Point, Point)
-- type FullLineSegment = [Point]

-- lineSegmentParser :: Parser LineSegment
-- lineSegmentParser = do
--   x1 <- decimal
--   _ <- char ','
--   y1 <- decimal
--   _ <- string " -> "
--   x2 <- decimal
--   _ <- char ','
--   y2 <- decimal
--   pure ((x1, y1), (x2, y2))

-- slope :: LineSegment -> (Int, Int)
-- slope ((x1, y1), (x2, y2))
--   | run == 0 = (riseSign 1, 0)
--   | rise == 0 = (0, runSign 1)
--   | otherwise = (rise `div` gcf, run `div` gcf)
--   where
--     rise = y2 - y1
--     run = x2 - x1
--     riseSign = if y2 > y1 then id else negate
--     runSign = if x2 > x1 then id else negate
--     gcf = gcd rise run

-- horzOrVertLine :: LineSegment -> Bool
-- horzOrVertLine l
--   | abs rise == 1 && abs run /= 1 = True
--   | abs rise /= 1 && abs run == 1 = True
--   | otherwise = False
--   where (rise, run) = slope l

-- fortyFiveDegreeLine :: LineSegment -> Bool
-- fortyFiveDegreeLine = (==) (1, 1) . bimap abs abs . slope

-- fullLine :: LineSegment -> FullLineSegment
-- fullLine line@(p1, p2) = (p2:) . takeWhile (p2 /=) $ iterate (bimap (+run) (+rise)) p1
--   where (rise, run) = slope line

-- answer :: [FullLineSegment] -> Int
-- answer = length . filter ((>1) . length) . group . sort . concat

-- main = do
--   input <- rights . map (parseOnly lineSegmentParser) . T.lines . T.pack <$> readFile "day5.input"
--   print . (++) "Part 1: " . show . answer . map fullLine $ filter horzOrVertLine input
--   print . (++) "Part 2: " . show . answer . map fullLine $ filter (liftA2 (||) horzOrVertLine fortyFiveDegreeLine) input

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
