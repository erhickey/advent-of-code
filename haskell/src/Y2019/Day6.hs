module Y2019.Day6 (solve) where

import Data.Bifunctor (second)
import Data.Map ((!), Map)
import qualified Data.Map as M (fromListWith, lookup)

import Data.List.Split (splitOn)

parseLine :: String -> (String, String)
parseLine s = let (x:y:_) = splitOn ")" s in (x, y)

sumOrbits :: Map String [String] -> Int -> String -> Int
sumOrbits m n s = n * l + sl
  where ss = M.lookup s m
        l = maybe 0 length ss
        sl = maybe 0 (sum . map (sumOrbits m (n + 1))) ss

-- https://www.reddit.com/r/haskell/comments/4h6c91/path_finding_one_liner/
pathFinder :: (Eq node) => node -> node -> (node -> [node]) -> [node]
pathFinder start end f =
  let paths = [start]:(paths >>= \path@(node:_) -> map (:path) . filter (not . flip elem path) $ f node)
  in  head $ filter ((== end) . head) paths

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ss = map parseLine $ lines input
    p1 = M.fromListWith (++) $ map (second (:[])) ss
    p2 = M.fromListWith (++) $ concatMap (\(x, y) -> [(x, [y]), (y, [x])]) ss
    part1 = show $ sumOrbits p1 1 "COM"
    part2 = show . flip (-) 3 . length $ pathFinder "YOU" "SAN" (p2 !)
