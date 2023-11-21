module Y2018.Day2 (solve) where

import Data.List (group, sort)
import Data.Maybe (catMaybes)

hasRepeat :: Ord a => Int -> [a] -> Bool
hasRepeat n = any ((==n) . length) . group . sort

rdiff :: String -> String -> String
rdiff xs ys = catMaybes $ zipWith (\x y -> if x == y then return x else Nothing) xs ys

-- map a function to each element in the list
-- along with each tailing element
tailMap :: (a -> a -> b) -> [a] -> [b]
tailMap _ [] = []
tailMap f (x:xs) = map (f x) xs ++ tailMap f xs

main = do
  input <- lines <$> readFile "day2.input"
  let doubles = length . filter (==True) $ map (hasRepeat 2) input
      triples = length . filter (==True) $ map (hasRepeat 3) input
  print . (++) "Part 1: " . show $ doubles * triples
  print . (++) "Part 2: " . head . dropWhile ((25 /=) . length) . tailMap rdiff $ input

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
