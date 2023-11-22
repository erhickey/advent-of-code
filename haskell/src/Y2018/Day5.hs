module Y2018.Day5 (solve) where

import Data.Char (toUpper)

react :: String -> String
react = foldr step ""
  where step x (y:ys)
          | x /= y && toUpper x == toUpper y = ys
        step x ys = x:ys

-- produce 26 variants of the input
-- where each variant has a different letter of the alphabet removed
p2 :: String -> [String]
p2 = zipWith (\c xs -> filter ((/=c) . toUpper) xs) ['A'..'Z'] . replicate 26

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    cs = filter (/= '\n') input
    part1 = show . length . react $ cs
    part2 = show . minimum . map (length . react) $ p2 cs
