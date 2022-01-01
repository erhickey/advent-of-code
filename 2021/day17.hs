import Data.Char (isDigit)
import Data.Maybe (mapMaybe)

type Coord = (Int, Int)
type Velocity = (Int, Int)
type Probe = (Coord, Velocity)

parseTargetArea :: String -> [Coord]
parseTargetArea = targetArea . map read . words . map go
  where
    go c
      | isDigit c = c
      | c == '-' = '-'
      | otherwise = ' '
    targetArea (x1:x2:y1:y2:_) = [ (x, y) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2] ]

step :: Probe -> Probe
step ((cx, cy), (vx, vy)) = ((cx + vx, cy +vy), (max 0 (vx - 1), vy - 1))

fire :: [Coord] -> Velocity -> Maybe [Coord]
fire cs v = if last trajectory `elem` cs then Just trajectory else Nothing
  where
    trajectory = map fst . takeWhile (not . missed . fst) $ iterate step ((0, 0), v)
    maxX = maximum $ map fst cs
    minY = minimum $ map snd cs
    missed (x, y)
      | x > maxX = True
      | y < minY = True
      | otherwise = False

rapidFire :: [Coord] -> [[Coord]]
rapidFire cs = mapMaybe (fire cs) vs
  where
    vs = [ (x, y) | x <- [minXVelocity..maxXVelocity], y <- [minYVelocity..maxYVelocity]]
    minXVelocity = ceiling $ ((-1) + sqrt (fromIntegral (1 + 4 * minimum (map fst cs) * 2))) / 2
    maxXVelocity = maximum $ map fst cs
    minYVelocity = minimum $ map snd cs
    maxYVelocity = subtract 1 . abs . minimum $ map snd cs

main = do
  hits <- rapidFire . parseTargetArea <$> readFile "day17.input"
  print . (++) "Part 1: " . show . maximum . map snd $ concat hits
  print . (++) "Part 2: " . show $ length hits
