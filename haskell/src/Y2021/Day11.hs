module Y2021.Day11 (solve) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (adjust, filter, fromList, keys, map, size)
import qualified Data.Set as S (deleteAt, elemAt, empty, fromList, null, union)

type Coord = (Int, Int)
type Grid = Map Coord Int

parse :: [String] -> Grid
parse = M.fromList . concat . zipWith parseLine [0..]
  where parseLine y = zipWith (\x n -> ((x, y), read [n])) [0..]

neighbors :: Coord -> [Coord]
neighbors (x, y) =
      [ (x - 1, y - 1), (x - 1, y), (x - 1, y + 1)
      , (x, y - 1), (x, y + 1)
      , (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)
      ]

tick :: Grid -> Grid
tick g = step3
  where
    step1 = M.map (+1) g
    step2 = flashes step1
    step3 = M.map (\o -> if o > 9 then 0 else o) step2

flashes :: Grid -> Grid
flashes = go S.empty
  where
    toFlash s = S.union s . S.fromList . M.keys . M.filter (==10)
    flash g c = M.adjust (+1) c . foldl (flip (M.adjust (+1))) g $ neighbors c
    go s g
      | S.null s' = g
      | otherwise = go (S.deleteAt 0 s') g'
      where
        s' = toFlash s g
        g' = flash g $ S.elemAt 0 s'

flashed :: Grid -> Int
flashed = M.size . M.filter (==0)

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    grid = parse $ lines input
    part1 = show . sum . map flashed . drop 1 . take 101 $ iterate tick grid
    part2 = show . length . takeWhile (\g -> flashed g /= M.size g) $ iterate tick grid
