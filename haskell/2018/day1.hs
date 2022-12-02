import Data.IntSet (empty, insert, member)
import Data.List (scanl')

parseLine :: String -> Int
parseLine ('+':xs) = parseLine xs
parseLine s = read s

-- Use an IntSet and lazy evaluation for a fast result
firstDupe :: [Int] -> Int
firstDupe xs = fst . head . filter (uncurry member) . zip xs . scanl (flip insert) empty $ xs

main = do
  input <- map parseLine . lines <$> readFile "day1.input"
  print . (++) "Part 1: " . show . sum $ input
  print . (++) "Part 2: " . show . firstDupe . scanl' (+) 0 . cycle $ input
