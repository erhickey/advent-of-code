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

main = do
  instructions <- map (map parseInstruction) . lines <$> readFile "day2.input"
  let p1 = map (getPoint part1Grid . foldl (move part1Grid) (1, 1)) instructions
      p2 = map (getPoint part2Grid . foldl (move part2Grid) (0, 2)) instructions
  print $ "Part 1: " ++ p1
  print $ "Part 2: " ++ p2
