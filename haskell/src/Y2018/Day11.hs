module Y2018.Day11 (solve) where

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M (empty, fromList, insert, lookup)
import Data.Maybe (catMaybes, fromMaybe)
import System.Environment (getArgs)

type Coord = (Int, Int)

type Grid = (Int, Int, Map Coord Int)

gridCoords :: Int -> Int -> [Coord]
gridCoords mx my = [ (x, y) | x <- [1..mx], y <- [1..my] ]

powerLevel :: Coord -> Int -> Int
powerLevel (x, y) n = (rackId * y + n) * rackId `div` 100 `mod` 10 - 5
  where rackId = x + 10

createGrid :: Int -> Int -> Int -> Grid
createGrid mx my sn = (mx, my, plMap)
  where plMap = M.fromList . map (\c -> (c, powerLevel c sn)) $ gridCoords mx my

-- create a summed area table
-- https://en.wikipedia.org/wiki/Summed-area_table
createSAT :: Grid -> Grid
createSAT (mx, my, m) = (mx, my, foldl go M.empty $ gridCoords mx my)
  where go acc c@(x, y) = M.insert c satValue acc
          where satValue = sum $ catMaybes
                  [ M.lookup (x, y) m
                  , M.lookup (x, y - 1) acc
                  , M.lookup (x - 1, y) acc
                  , negate <$> M.lookup (x - 1, y - 1) acc
                  ]

-- sum of a square in a summed area table
squareSum
  :: Coord -- top left x/y of square
  -> Int -- length/width of square
  -> Grid -- summed area table
  -> Maybe Int -- sum, or Nothing if square is invalid
squareSum (x, y) s (mx, my, m)
  | x + s - 1 > mx || y + s - 1 > my = Nothing
  | otherwise = return . sum $ catMaybes
    [ M.lookup (x - 1, y - 1) m
    , M.lookup (x + s - 1, y + s - 1) m
    , negate <$> M.lookup (x + s - 1, y - 1) m
    , negate <$> M.lookup (x - 1, y + s - 1) m
    ]

-- find the square in a summed area table with the largest sum
maxSum
  :: [Int] -- valid size(s) of squares
  -> Grid -- summed area table
  -> (Int, Int, Int, Int) -- (x, y, size, sum) of square with largest sum
maxSum ss sat@(mx, my, _) = foldl' go (0,0,0,0) [(x, y, s) | s <- ss, x <- [1..mx - s + 1], y <- [1..my - s + 1]]
  where go acc@(_, _, _, max) (x, y, s)
          | size > max = (x, y, s, size)
          | otherwise = acc
          where size = fromMaybe 0 $ squareSum (x, y) s sat

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    sat = createSAT . createGrid 300 300 . read $ input
    part1 = show $ maxSum [3] sat
    part2 = show $ maxSum [1..300] sat
