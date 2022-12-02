import Data.List (sort)

chain :: [Int] -> [Int]
chain = reverse . addLast . go [0] . sort
  where
    addLast (x:xs) = x + 3 : x : xs
    go acc [] = acc
    go acc@(x:_) (y:ys)
      | y - x > 3 = acc
      | otherwise = go (y:acc) ys

gapCount :: [Int] -> Int -> Int
gapCount xs n = go xs 0
  where
    go [_] a = a
    go (x:y:ys) a
      | y - x == n = go (y:ys) $ a + 1
      | otherwise = go (y:ys) a

combinations :: [Int] -> Int
combinations xs = snd . head $ foldr go [] xs
  where
    go x [] = [(x, 1),(x, 0), (x, 0)]
    go x acc@((_, e0n):(e1c, e1n):(e2c, e2n):_)
      | e2c - x <= 3 = (x, e2n + e1n + e0n):acc
      | e1c - x <= 3 = (x, e1n + e0n):acc
      | otherwise = (x, e0n):acc

main = do
  input <- map read . lines <$> readFile "day10.input"
  let c = chain input
  print . (++) "Part 1: " . show $ gapCount c 1 * gapCount c 3
  print . (++) "Part 2: " . show $ combinations c
