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

part1 :: Int -> (Int, Int) -> Int
part1 t (minute, bus) = bus * (minute - t)

-- https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Search_by_sieving
part2 :: [(Int, Int)] -> Int
part2 = fst . foldl go (0, 1) . reverse . sort
  where
    go (pos, inc) (time, offset)
      | rem (pos + offset) time /= 0 = go (pos + inc, inc) (time, offset)
      | otherwise = (pos, inc * time)

main = do
  (l1:l2:_) <- lines <$> readFile "day13.input"
  let depart = read l1
      bs = parseBuses l2
  print . (++) "Part 1: " . show . part1 depart . earliest depart $ map fst bs
  print . (++) "Part 2: " . show $ part2 bs

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
