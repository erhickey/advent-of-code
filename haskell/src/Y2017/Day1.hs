module Y2017.Day1 (solve) where

type Index = (Int, Char)

sSum :: String -> Int -> Int -> Index -> Int
sSum xs ixMod n (currIx, c)
  | c == (xs !! ix) = n + read [c]
  | otherwise = n
  where ix
          | ix' >= length xs = ix' - length xs
          | otherwise = ix'
          where ix' = currIx + ixMod

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    line = filter (/= '\n') input
    xs = zip [0..] line
    part1 = show $ foldl (sSum line 1) 0 xs
    part2 = show $ foldl (sSum line $ div (length line) 2) 0 xs
