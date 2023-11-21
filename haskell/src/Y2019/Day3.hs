module Y2019.Day3 (solve) where

import Control.Applicative (liftA2)
import Data.List (elemIndex)
import qualified Data.Set as S (findMin, fromList, intersection, map)

import Data.List.Split (splitOn)

data Direction = U | D | L | R

type Movement = (Direction, Int)
type Coord = (Int, Int)

move :: Direction -> Coord -> Coord
move U (x, y) = (x, y + 1)
move D (x, y) = (x, y - 1)
move L (x, y) = (x - 1, y)
move R (x, y) = (x + 1, y)

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

parse :: String -> Movement
parse ('U':xs) = (U, read xs)
parse ('D':xs) = (D, read xs)
parse ('L':xs) = (L, read xs)
parse ('R':xs) = (R, read xs)

coords :: [Movement] -> [Coord]
coords = reverse . foldl go []
  where go [] x = fm x (0, 0)
        go acc@(a:_) x = fm x a ++ acc
        fm (d, n) c = reverse . drop 1 . take (n + 1) $ iterate (move d) c

intersectionDistance :: [Coord] -> [Coord] -> Int
intersectionDistance xs ys = S.findMin $ S.map (manhattanDistance (0, 0)) is
  where is = S.intersection (S.fromList xs) (S.fromList ys)

intersectionSteps :: [Coord] -> [Coord] -> Maybe Int
intersectionSteps xs ys = S.findMin $ S.map go is
  where is = S.intersection (S.fromList xs) (S.fromList ys)
        go x = (+2) <$> liftA2 (+) (elemIndex x xs) (elemIndex x ys)

main = do
  wires <- map (map parse . splitOn ",") . lines <$> readFile "day3.input"
  let (w1:w2:_) = map coords wires
  print . (++) "Part 1: " . show $ intersectionDistance w1 w2
  print . (++) "Part 2: " . show $ intersectionSteps w1 w2

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
