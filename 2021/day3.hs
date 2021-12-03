import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.List (transpose)

moreOnesThanZeroes :: [Int] -> Bool
moreOnesThanZeroes xs =  (length $ filter (==1) xs) > div (length xs) 2

gammaEpsilon :: [[Int]] -> ([Int], [Int])
gammaEpsilon = foldl go ([], []) . reverse
  where go (gs, es) xs
          | moreOnesThanZeroes xs = (1:gs, 0:es)
          | otherwise = (0:gs, 1:es)

binToDec :: [Int] -> Int
binToDec = fst . foldr go (0, 1)
  where go x (acc, ix) = (x * ix + acc, ix * 2)

main = do
   input <- map (map digitToInt) . lines <$> readFile "day3.input"
   print . (++) "Part 1: " . show . uncurry (*) . join bimap binToDec . gammaEpsilon $ transpose input
