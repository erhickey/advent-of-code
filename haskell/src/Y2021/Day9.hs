module Y2021.Day9 (solve) where

import Control.Monad (liftM2)
import Data.List (sort)
import Data.List.Unique (uniq)
import Data.Map.Strict ((!), Map)
import qualified Data.Map.Strict as M (fromList, keys, lookup, member)
import Data.Maybe (mapMaybe)

type Coord = (Int, Int)
type Grid = Map Coord Int

parse :: [String] -> Grid
parse = M.fromList . concat . zipWith parseLine [0..]
  where parseLine y = zipWith (\x n -> ((x, y), read [n])) [0..]

neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

isLowPoint :: Grid -> Coord -> Bool
isLowPoint g c = notElem height ns && height == minimum (height:ns) && height /= 9
  where
    height = g ! c
    ns = mapMaybe (`M.lookup` g) $ neighbors c

lowPoints :: Grid -> [Coord]
lowPoints g = filter (isLowPoint g) $ M.keys g

basin :: Grid -> Coord -> [Coord]
basin g c = snd . head . dropWhile (not . null . fst) $ iterate go ([c], [c])
  where
    go (cs, bs)
      | null ns = ([], uniq $ sort bs)
      | otherwise = (ns, ns ++ bs)
      where ns = filter ((/=9) . (!) g) . filter (liftM2 (&&) (`M.member` g) (`notElem` bs)) $ concatMap neighbors cs

basins :: Grid -> [[Coord]]
basins g = map (basin g) $ lowPoints g

main = do
  input <- parse . lines <$> readFile "day9.input"
  print . (++) "Part 1: " . show . sum . map ((+1) . (!) input) $ lowPoints input
  print . (++) "Part 2: " . show . product . take 3 . reverse . sort . map length $ basins input

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
