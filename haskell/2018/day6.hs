import Data.Bifunctor (second)
import qualified Data.IntMap as M (elems, empty, filter, insertWith, IntMap)
import Data.List (maximumBy)
import Data.Ord (comparing)

type Point = (Int, Int)

data Grid = Grid
  { ulx :: Int
  , uly :: Int
  , lrx :: Int
  , lry :: Int
  , points :: [Point]
  }

parseLine :: String -> Point
parseLine s = read $ "(" ++ s ++ ")"

initGrid :: [Point] -> Grid
initGrid zs = Grid ulx uly lrx lry points
  where xs = map fst zs
        ys = map snd zs
        ulx = minimum xs
        uly = minimum ys
        lrx = maximum xs
        lry = maximum ys
        points = [ (x, y) | x <- [ulx..lrx], y <- [uly..lry] ]

manhattanDistance :: Num a => (a, a) -> (a, a) -> a
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

isFiniteArea :: Grid -> [Point] -> Bool
isFiniteArea g = all interiorPoint
  where interiorPoint (x, y) = x /= ulx g && x /= lrx g && y /= uly g && y /= lry g

buildAreaMap :: Grid -> [Point] -> M.IntMap [Point]
buildAreaMap g xs = foldl go M.empty $ points g
  where ls = zip [1..] xs
        go m p
          | null cs = M.insertWith (++) c [p] m
          | otherwise = m
          where ((c, _):cs) = filter ((==) shortest . snd) ds
                shortest = minimum $ map snd ds
                ds = map (second $ manhattanDistance p) ls

part2 :: Grid -> [Point] -> Int
part2 g xs = length . filter safeDistance $ points g
  where safeDistance p = (>) 10000 . sum . map (manhattanDistance p) $ xs

main = do
  coords <- map parseLine . lines <$> readFile "day6.input"
  let grid = initGrid coords
      areaMap = buildAreaMap grid coords
      p1Candidates = M.elems $ M.filter (isFiniteArea grid) areaMap
      smallestArea = length $ maximumBy (comparing length) p1Candidates
  print . (++) "Part 1: " . show $ smallestArea
  print . (++) "Part 2: " . show $ part2 grid coords
