import Data.Map.Strict ((!), Map)
import qualified Data.Map.Strict as M (fromList, lookup, keys)
import Data.Maybe (mapMaybe)

type Coord = (Int, Int)
type Grid = Map Coord Int

parse :: [String] -> Grid
parse = M.fromList . concat . zipWith parseLine [0..]
  where parseLine y = zipWith (\x n -> ((x, y), read [n])) [0..]

isLowPoint :: Grid -> Coord -> Bool
isLowPoint g c@(x, y) = notElem v neighbors && v == minimum (v:neighbors)
  where
    v = g ! c
    neighbors = mapMaybe (`M.lookup` g) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

part1 :: Grid -> Int
part1 g = sum . map ((+1) . (!) g) . filter (isLowPoint g) $ M.keys g

main = do
  input <- parse . lines <$> readFile "day9.input"
  print . (++) "Part 1: " . show $ part1 input
