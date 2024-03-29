module Y2016.Day2 (solve) where

data Instruction = U | D | L | R

type Grid = [[Char]]

type Coord = (Int, Int)

part1Grid :: Grid
part1Grid =
  [ [ '1', '2', '3' ]
  , [ '4', '5', '6' ]
  , [ '7', '8', '9' ]
  ]

part2Grid :: Grid
part2Grid =
  [ [ ' ', ' ', '1', ' ', ' ' ]
  , [ ' ', '2', '3', '4', ' ' ]
  , [ '5', '6', '7', '8', '9' ]
  , [ ' ', 'A', 'B', 'C', ' ' ]
  , [ ' ', ' ', 'D', ' ', ' ' ]
  ]

parseInstruction :: Char -> Instruction
parseInstruction 'U' = U
parseInstruction 'D' = D
parseInstruction 'L' = L
parseInstruction 'R' = R

nextCoord :: Coord -> Instruction -> Coord
nextCoord (x, y) U = (x - 1, y)
nextCoord (x, y) D = (x + 1, y)
nextCoord (x, y) L = (x, y - 1)
nextCoord (x, y) R = (x, y + 1)

nextBoundedCoord :: Grid -> Coord -> Instruction -> Coord
nextBoundedCoord grid@(g:_) c i
  | x' < 0 || y' < 0 = c
  | x' >= length g || y' >= length grid = c
  | otherwise = c'
  where c'@(x', y') = nextCoord c i

move :: Grid -> Coord -> Instruction -> Coord
move g c i
  | getPoint g c' == ' ' = c
  | otherwise = c'
  where c' = nextBoundedCoord g c i

getPoint :: Grid -> Coord -> Char
getPoint g (x, y) = (g !! x) !! y

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    instructions = map (map parseInstruction) . lines $ input
    part1 = map (getPoint part1Grid . foldl (move part1Grid) (1, 1)) instructions
    part2 = map (getPoint part2Grid . foldl (move part2Grid) (0, 2)) instructions
