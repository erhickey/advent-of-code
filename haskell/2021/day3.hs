import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.List (transpose)

onesZeroes :: [Int] -> (Int, Int)
onesZeroes xs = (count 1, count 0)
  where count n = length $ filter ((==) n) xs

gammaEpsilon :: [[Int]] -> ([Int], [Int])
gammaEpsilon = foldl go ([], []) . reverse
  where go (gs, es) xs
          | uncurry (>) (onesZeroes xs) = (1:gs, 0:es)
          | otherwise = (0:gs, 1:es)

rating :: Bool -> [[Int]] -> [Int]
rating o2 xs = head . head . filter ((==) 1 . length) $ scanl go xs [0..]
  where go ys ix =
          let bits = map (flip (!!) ix) ys
              f = if o2 then (>=) else (<)
              oneOrZero = fromEnum . uncurry f $ onesZeroes bits
          in filter ((==) oneOrZero . (flip (!!) ix)) ys

binToDec :: [Int] -> Int
binToDec = sum . zipWith (*) (iterate (*2) 1) . reverse

answer :: ([Int], [Int]) -> Int
answer = uncurry (*) . join bimap binToDec

main = do
   input <- map (map digitToInt) . lines <$> readFile "day3.input"
   print . (++) "Part 1: " . show . answer . gammaEpsilon $ transpose input
   print . (++) "Part 2: " . show . answer $ (rating True input, rating False input)
