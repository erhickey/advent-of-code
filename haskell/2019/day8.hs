import Data.Function (on)
import Data.List (minimumBy)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs
  | n < 1 = [xs]
  | otherwise = take n xs : chunks n (drop n xs)

layers :: Int -> Int -> String -> [[Int]]
layers width height s = map (map (read . (:[]))) $ chunks (width * height) s

part1 :: [[Int]] -> Int
part1 xs = length (filter (==1) layer) * length (filter (==2) layer)
  where layer = fst . minimumBy (compare `on` length . snd) $ map (\ys -> (ys, filter (==0) ys)) xs

render :: [[Int]] -> [Int]
render = foldl1 go
  where
    go ys zs = zipWith underlay ys zs
    underlay x y
      | x /= 2 = x
      | otherwise = y

decode :: Int -> [Int] -> [String]
decode width xs = map (map replace) $ chunks width xs
  where
    replace 1 = '#'
    replace 0 = ' '

main = do
  input <- layers 25 6 . filter (/='\n') <$> readFile "day8.input"
  print . (++) "Part 1: " . show $ part1 input
  putStrLn "Part 2:"
  mapM_ putStrLn . decode 25 $ render input
