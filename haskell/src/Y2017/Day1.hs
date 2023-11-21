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

main = do
  input <- filter (/= '\n') <$> readFile "day1.input"
  let xs = zip [0..] input
  print . (++) "Part 1: " . show $ foldl (sSum input 1) 0 xs
  print . (++) "Part 2: " . show $ foldl (sSum input $ div (length input) 2) 0 xs

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
