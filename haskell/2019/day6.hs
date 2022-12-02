import Data.Bifunctor (second)
import Data.Map ((!), Map)
import qualified Data.Map as M (fromListWith, lookup)

import Data.List.Split (splitOn)

parseLine :: String -> (String, String)
parseLine s = let (x:y:_) = splitOn ")" s in (x, y)

sumOrbits :: Map String [String] -> Int -> String -> Int
sumOrbits m n s = n * l + sl
  where ss = M.lookup s m
        l = maybe 0 length ss
        sl = maybe 0 (sum . map (sumOrbits m (n + 1))) ss

-- https://www.reddit.com/r/haskell/comments/4h6c91/path_finding_one_liner/
pathFinder :: (Eq node) => node -> node -> (node -> [node]) -> [node]
pathFinder start end f =
  let paths = [start]:(paths >>= \path@(node:_) -> map (:path) . filter (not . flip elem path) $ f node)
  in  head $ filter ((== end) . head) paths

main = do
  input <- map parseLine . lines <$> readFile "day6.input"
  let p1 = M.fromListWith (++) $ map (second (:[])) input
      p2 = M.fromListWith (++) $ concatMap (\(x, y) -> [(x, [y]), (y, [x])]) input
  print . (++) "Part 1: " . show $ sumOrbits p1 1 "COM"
  print . (++) "Part 2: " . show . flip (-) 3 . length $ pathFinder "YOU" "SAN" (p2 !)
