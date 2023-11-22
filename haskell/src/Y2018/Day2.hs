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

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ls = lines input
    doubles = length . filter (==True) $ map (hasRepeat 2) ls
    triples = length . filter (==True) $ map (hasRepeat 3) ls
    part1 = show $ doubles * triples
    part2 = head . dropWhile ((25 /=) . length) . tailMap rdiff $ ls
