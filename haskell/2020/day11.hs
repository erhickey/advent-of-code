import Data.Function (on)
import Data.List (groupBy)
import Data.Map (Map)
import qualified Data.Map as M (empty, filter, findMax, findMin, findWithDefault, fromList, lookup, mapWithKey, size, toAscList)

data Pos = Floor | Unoccupied | Occupied deriving (Eq)

type Coord = (Int, Int)

type Seats = Map Coord Pos

parse :: [String] -> Seats
parse xs = M.fromList . concat $ zipWith parseLine [0..] xs
  where parseLine y = zipWith (parseChar y) [0..]
        parseChar y x c
          | c == 'L' = ((y, x), Unoccupied)
          | c == '.' = ((y, x), Floor)

adjacentSpaces :: Coord -> [Coord]
adjacentSpaces (y, x) =
  [ (y - 1, x -1), (y - 1, x), (y - 1, x + 1)
  , (y, x - 1), (y, x + 1)
  , (y + 1, x -1), (y + 1, x), (y + 1, x + 1)
  ]

adjacentOccupied :: Seats -> Coord -> Int
adjacentOccupied m = sum . map (toInt . flip (M.findWithDefault Unoccupied) m) . adjacentSpaces
  where toInt Occupied = 1
        toInt _ = 0

look :: Seats -> Coord -> (Int, Int) -> Int
look m (x, y) l@(lx, ly) =
  let c = (x + lx, y + ly)
      seat = M.lookup c m
  in case seat of
    Nothing -> if fst (M.findMin m) < c || fst (M.findMax m) > c then 0 else look m c l
    Just Floor -> look m c l
    Just Occupied -> 1
    Just Unoccupied -> 0

lookAround :: Seats -> Coord -> Int
lookAround m c = sum $ map (look m c)
  [ (-1, -1), (-1, 0), (-1, 1)
  , (0, -1), (0, 1)
  , (1, -1), (1, 0), (1, 1)
  ]

alterSeat :: Int -> (Seats -> Coord -> Int) -> Seats -> Coord -> Pos -> Pos
alterSeat _ _ _ _ Floor = Floor
alterSeat n f m c Occupied
  | f m c >= n = Unoccupied
  | otherwise = Occupied
alterSeat _ f m c Unoccupied
  | f m c == 0 = Occupied
  | otherwise = Unoccupied

doRound :: Int -> (Seats -> Coord -> Int) -> Seats -> Seats
doRound n f m = M.mapWithKey go m
  where go c p = alterSeat n f m c p

equalize :: (Seats -> Seats) -> Seats -> Seats
equalize f m = fst . head . dropWhile (uncurry (/=)) $ iterate (\(m', _) -> (f m', m')) (m, M.empty)

occupiedCount :: Seats -> Int
occupiedCount = M.size . M.filter (==Occupied)

printSeats :: Seats -> [String]
printSeats = map printRow . groupBy ((==) `on` fst . fst) . M.toAscList
  where printRow = map (printSeat . snd)
        printSeat Floor = '.'
        printSeat Unoccupied = 'L'
        printSeat Occupied = '#'

main = do
  m <- parse . lines <$> readFile "day11.input"
  print . (++) "Part 1: " . show . occupiedCount $ equalize (doRound 4 adjacentOccupied) m
  print . (++) "Part 2: " . show . occupiedCount $ equalize (doRound 5 lookAround) m
