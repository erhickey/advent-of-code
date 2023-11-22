{-# LANGUAGE TupleSections #-}

module Y2016.Day1 (solve) where

data Direction = R | L

data Heading = N | E | S | W deriving (Enum)

type Instruction = (Direction, Int)

type Coord = (Int, Int)

type Position = (Heading, Coord)

origin :: Coord
origin = (0, 0)

parseInstruction :: String -> Instruction
parseInstruction ('R':xs) = (R, read xs)
parseInstruction ('L':xs) = (L, read xs)

turn :: Heading -> Direction -> Heading
turn W R = N
turn N L = W
turn h R = succ h
turn h L = pred h

move :: Heading -> Coord -> Coord
move N (x, y) = (x + 1, y)
move E (x, y) = (x, y + 1)
move S (x, y) = (x - 1, y)
move W (x, y) = (x, y - 1)

followInstruction :: Position -> Instruction -> [Position]
followInstruction (h, c) (d, n) = map (h',) cs
  where h' = turn h d
        cs = tail . take (n + 1) $ iterate (move h') c

followInstructions :: [Instruction] -> [Position]
followInstructions = flatScanL followInstruction [(N, origin)]

taxicabDistance :: Num a => (a, a) -> (a, a) -> a
taxicabDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- scanl, but the binary operator returns a list
-- the binary operator is passed the last element from the previous result
flatScanL :: (b -> a -> [b]) -> [b] -> [a] -> [b]
flatScanL f acc = concat . scanl go acc
  where go xs = f (last xs)

-- find first duplciate element in a list
firstDupe :: Eq a => [a] -> a
firstDupe (x:xs)
  | x `elem` xs = x
  | otherwise = firstDupe xs

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    path = followInstructions . map parseInstruction . words . filter (/= ',') $ input
    hq = snd $ last path
    fr = firstDupe $ map snd path
    part1 = show $ taxicabDistance origin hq
    part2 = show $ taxicabDistance origin fr
