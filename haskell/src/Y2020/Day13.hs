module Y2020.Day13 (solve) where

import Data.List (sort)
import Data.Maybe (catMaybes)

import Data.List.Split (splitOn)

parseBuses :: String -> [(Int, Int)]
parseBuses s = catMaybes $ zipWith go (splitOn "," s) [0..]
  where go "x" _ = Nothing
        go x n = pure (read x, n)

closestTime :: Int -> Int -> (Int, Int)
closestTime target n = (head . filter (>target) $ iterate (+n) n, n)

earliest :: Int -> [Int] -> (Int, Int)
earliest t bs = minimum $ map (closestTime t) bs

p1 :: Int -> (Int, Int) -> Int
p1 t (minute, bus) = bus * (minute - t)

-- https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Search_by_sieving
p2 :: [(Int, Int)] -> Int
p2 = fst . foldl go (0, 1) . reverse . sort
  where
    go (pos, inc) (time, offset)
      | rem (pos + offset) time /= 0 = go (pos + inc, inc) (time, offset)
      | otherwise = (pos, inc * time)

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    (l1:l2:_) = lines input
    depart = read l1
    bs = parseBuses l2
    part1 = show . p1 depart . earliest depart $ map fst bs
    part2 = show $ p2 bs
