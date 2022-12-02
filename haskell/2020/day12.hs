data Direction = N | E | S | W | R | L | F deriving (Show)

data Heading = HN | HE | HS | HW deriving (Enum)

type Movement = (Direction, Int)

type Coord = (Int, Int)

type State = (Coord, Heading)

type WaypointState = (Coord, Coord)

parse :: String -> Movement
parse ('N':xs) = (N, read xs)
parse ('E':xs) = (E, read xs)
parse ('S':xs) = (S, read xs)
parse ('W':xs) = (W, read xs)
parse ('R':xs) = (R, read xs)
parse ('L':xs) = (L, read xs)
parse ('F':xs) = (F, read xs)

move :: Movement -> State -> State
move (N, n) ((x, y), h) = ((x, y + n), h)
move (E, n) ((x, y), h) = ((x + n, y), h)
move (S, n) ((x, y), h) = ((x, y - n), h)
move (W, n) ((x, y), h) = ((x - n, y), h)
move (F, n) (c, HN) = move (N, n) (c, HN)
move (F, n) (c, HE) = move (E, n) (c, HE)
move (F, n) (c, HS) = move (S, n) (c, HS)
move (F, n) (c, HW) = move (W, n) (c, HW)
move (R, 0) s = s
move (R, n) (c, HW) = move (R, n - 90) (c, HN)
move (R, n) (c, h) = move (R, n - 90) (c, succ h)
move (L, 0) s = s
move (L, n) (c, HN) = move (L, n - 90) (c, HW)
move (L, n) (c, h) = move (L, n - 90) (c, pred h)

moveWaypoint :: Movement -> Coord -> Coord
moveWaypoint (N, n) (x, y) = (x, y + n)
moveWaypoint (E, n) (x, y) = (x + n, y)
moveWaypoint (S, n) (x, y) = (x, y - n)
moveWaypoint (W, n) (x, y) = (x - n, y)
moveWaypoint (R, 0) c = c
moveWaypoint (R, n) (x, y)
  | x >= 0 && y >= 0 = moveWaypoint (R, n - 90) (y, negate x)
  | x >= 0 && y <= 0 = moveWaypoint (R, n - 90) (y, negate x)
  | x <= 0 && y <= 0 = moveWaypoint (R, n - 90) (y, abs x)
  | x <= 0 && y >= 0 = moveWaypoint (R, n - 90) (y, abs x)
moveWaypoint (L, 0) c = c
moveWaypoint (L, n) (x, y)
  | x >= 0 && y >= 0 = moveWaypoint (L, n - 90) (negate y, x)
  | x <= 0 && y >= 0 = moveWaypoint (L, n - 90) (negate y, x)
  | x <= 0 && y <= 0 = moveWaypoint (L, n - 90) (abs y, x)
  | x >= 0 && y <= 0 = moveWaypoint (L, n - 90) (abs y, x)

waypointMove :: Movement -> WaypointState -> WaypointState
waypointMove (F, n) ((x, y), (wx, wy)) = ((x + n * wx, y + n * wy), (wx, wy))
waypointMove m (c, w) = (c, moveWaypoint m w)

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

moves :: [Movement] -> State
moves = foldl (flip move) ((0, 0), HE)

waypointMoves :: [Movement] -> WaypointState
waypointMoves = foldl (flip waypointMove) ((0, 0), (10, 1))

main = do
  input <- map parse . lines <$> readFile "day12.input"
  print . (++) "Part 1: " . show . manhattanDistance (0, 0) . fst $ moves input
  print . (++) "Part 2: " . show . manhattanDistance (0, 0) . fst $ waypointMoves input
