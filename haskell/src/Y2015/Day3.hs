module Y2015.Day3 (solve) where

import Data.Set (fromList, size, union)

type Coord = (Int, Int)

data Direction = N | E | S | W

move :: Coord -> Direction -> Coord
move (x, y) N = (x, y + 1)
move (x, y) E = (x + 1, y)
move (x, y) S = (x, y - 1)
move (x, y) W = (x - 1, y)

parseInstruction :: Char -> Direction
parseInstruction '^' = N
parseInstruction '>' = E
parseInstruction 'v' = S
parseInstruction '<' = W

-- divvy a list up into two lists
-- each containing every other element from the original list
divvy :: [a] -> ([a], [a])
divvy = go ([], [])
  where
    go acc [] = acc
    go (xs, ys) [x] = (xs ++ [x], ys)
    go (xs, ys) (x:y:zs) = go (xs ++ [x], ys ++ [y]) zs

main = do
  directions <- map parseInstruction <$> readFile "day3.input"
  let p1Visited = fromList $ scanl move (0, 0) directions
      p2Directions = divvy directions
      p2SantaVisited = scanl move (0, 0) $ fst p2Directions
      p2RobotVisited = scanl move (0, 0) $ snd p2Directions
      p2Visited = fromList p2SantaVisited `union` fromList p2RobotVisited
  print . (++) "Part 1: " . show $ size p1Visited
  print . (++) "Part 2: " . show $ size p2Visited

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
