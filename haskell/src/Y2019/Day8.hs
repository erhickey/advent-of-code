module Y2019.Day8 (solve) where

import Data.Function (on)
import Data.List (minimumBy)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs
  | n < 1 = [xs]
  | otherwise = take n xs : chunks n (drop n xs)

layers :: Int -> Int -> String -> [[Int]]
layers width height s = map (map (read . (:[]))) $ chunks (width * height) s

p1 :: [[Int]] -> Int
p1 xs = length (filter (==1) layer) * length (filter (==2) layer)
  where layer = fst . minimumBy (compare `on` length . snd) $ map (\ys -> (ys, filter (==0) ys)) xs

render :: [[Int]] -> [Int]
render = foldl1 go
  where
    go = zipWith underlay
    underlay x y
      | x /= 2 = x
      | otherwise = y

decode :: Int -> [Int] -> [String]
decode width xs = map (map replace) $ chunks width xs
  where
    replace 1 = '#'
    replace 0 = ' '

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ns = layers 25 6 $ filter (/='\n') input
    part1 = show $ p1 ns
    part2 = ("\n" ++) . unlines . decode 25 $ render ns
