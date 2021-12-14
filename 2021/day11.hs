import Data.Set (Set)
import qualified Data.Set as S (difference, empty, foldl, fromList, size, union)
import Data.Vector (Vector)
import qualified Data.Vector as V ((!), findIndices, fromList, length, map, toList, unsafeUpd)

type Octopus = (Int, [Int])
type Octopi = Vector Octopus

parse :: [String] -> Octopi
parse ss = V.fromList . concat $ zipWith parseLine [0..] ss
  where
    maxX = length (head ss) - 1
    maxY = length ss - 1
    parseLine y = zipWith (\x n -> (read [n], neighbors (x, y))) [0..]
    inbounds (x, y) = x <= maxX && y <= maxY && x >= 0 && y >= 0
    coordToIndex (x, y) = y * (maxX + 1) + x
    neighbors (x, y) = map coordToIndex $ filter inbounds
      [ (x - 1, y - 1), (x - 1, y), (x - 1, y + 1)
      , (x, y - 1), (x, y + 1)
      , (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)
      ]

step :: Octopi -> (Set Int, Octopi)
step v = (flashed, part3)
  where
    part1 = V.map (\(o, ns) -> (o + 1, ns)) v
    (flashed, part2) = converge done $ iterate flashes (S.empty, part1)
    part3 = V.map (\(o, ns) -> (if o > 9 then 0 else o, ns)) part2
    flashIndices = S.fromList . V.toList . V.findIndices ((>9) . fst)
    done (o1, _) (o2, _) = o1 == o2
    flashes (s, os) = let toFlash = S.difference (flashIndices os) s in (S.union s toFlash, S.foldl flash os toFlash)
    flash os ix = let (_, ns) = os V.! ix in V.unsafeUpd os $ map (\n -> (n, inc os n)) ns
    inc os ix = let (o, ns) = os V.! ix in (o + 1, ns)

converge :: (a -> a -> Bool) -> [a] -> a
converge f (x:y:ys)
  | f x y = x
  | otherwise = converge f (y:ys)

steps :: Octopi -> [(Set Int, Octopi)]
steps os = iterate (step . snd) (S.empty, os)

allFlashed :: [(Set Int, Octopi)] -> [(Set Int, Octopi)]
allFlashed = takeWhile (\(s, v) -> S.size s /= V.length v)

main = do
  input <- parse . lines <$> readFile "day11.input"
  print . (++) "Part 1: " . show . sum . map (S.size . fst) . take 101 $ steps input
  print . (++) "Part 2: " . show . length . allFlashed $ steps input
