module Y2020.Day9 (solve) where

import Safe (lastDef)

valid :: [Int] -> Int -> Bool
valid xs n = elem n $ [ x + y | x <- xs, y <- xs, x /= y ]

groupByValid :: Int -> [Int] -> ([Int], [Int])
groupByValid n xs = snd . foldl go (fst init, ([], [])) $ snd init
  where init = splitAt n xs
        go (acc@(_:as), (v, i)) x
          | valid acc x = (as ++ [x], (x:v, i))
          | otherwise = (as ++ [x], (v, x:i))

firstInvalid :: Int -> [Int] -> Int
firstInvalid n = last . snd . groupByValid n

findRange :: [Int] -> Int -> [Int]
findRange (x:y:xs) n
  | fst acc == n = snd acc
  | otherwise = findRange (y:xs) n
  where acc = lastDef (n + 1, []) . takeWhile ((n>=) . fst) $ scanl go (x + y, [x,y]) xs
        go (a, zs) z = (a + z, z:zs)

minPlusMax :: (Num a, Ord a) => [a] -> a
minPlusMax xs = minimum xs + maximum xs

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ns = map read $ lines input
    fi = firstInvalid 25 ns
    part1 = show fi
    part2 = show . minPlusMax $ findRange ns fi
