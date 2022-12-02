import Data.List (elemIndex, intersect, sort)
import Data.List.Split (splitOn)
import Data.IntMap.Strict ((!), IntMap)
import qualified Data.IntMap.Strict as IM (elems, empty, insert)
import Data.Maybe (fromJust)

parseLine :: String -> ([String], [String])
parseLine s = (head ss, last ss)
  where ss = map (map sort . words) $ splitOn "|" s

lineOutput :: ([String], [String]) -> [Int]
lineOutput (xs, ys) = map (fromJust . (`elemIndex` vs)) ys
  where vs = IM.elems $ findValues xs

findValues :: [String] -> IntMap String
findValues xs = fst $ foldl go (IM.empty, xs) [1,4,7,8,9,6,0,3,5,2]
  where
    go (m, acc) n = let patt = find n m acc in (IM.insert n patt m, filter (/=patt) acc)
    ofLength n = filter ((==n) . length)
    isSubset sub super = sub == intersect sub super
    find 0 _ = head . ofLength 6
    find 1 _ = head . ofLength 2
    find 2 _ = head . ofLength 5
    find 3 m = head . filter (isSubset (m ! 1)) . ofLength 5
    find 4 _ = head . ofLength 4
    find 5 m = head . filter (`isSubset` (m ! 9)) . ofLength 5
    find 6 m = head . filter (not . isSubset (m ! 1)) . ofLength 6
    find 7 _ = head . ofLength 3
    find 8 _ = head . ofLength 7
    find 9 m = head . filter (isSubset (m ! 4)) . ofLength 6

main = do
  input <- map (lineOutput . parseLine) . lines <$> readFile "day8.input"
  print . (++) "Part 1: " . show . length $ concatMap (filter (`elem` [1,4,7,8])) input
  print . (++) "Part 2: " . show . sum $ map (read . concatMap show) input
