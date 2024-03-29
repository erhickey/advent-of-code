module Y2021.Day2 (solve) where

import Data.Char (isDigit)

data Direction = Down | Up | Forward
type Movement = (Direction, Int)
type Position = (Int, Int, Int)

parse :: String -> Movement
parse cs
  | s == "forward " = (Forward, read n)
  | s == "down " = (Down, read n)
  | s == "up " = (Up, read n)
  where (s, n) = break isDigit cs

move :: Position -> Movement -> Position
move (x, y, a) (Forward, n) = (x + n, y, a)
move (x, y, a) (Down, n) = (x, y + n, a)
move (x, y, a) (Up, n) = (x, y - n, a)

moveWithAim :: Position -> Movement -> Position
moveWithAim (x, y, a) (Forward, n) = (x + n, y + (n * a), a)
moveWithAim (x, y, a) (Down, n) = (x, y, a + n)
moveWithAim (x, y, a) (Up, n) = (x, y, a - n)

answer :: Position -> Int
answer (x, y, _) = x * y

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ms = map parse $ lines input
    part1 = show . answer $ foldl move (0, 0, 0) ms
    part2 = show . answer $ foldl moveWithAim (0, 0, 0) ms
