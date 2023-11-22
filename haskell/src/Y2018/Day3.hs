module Y2018.Day3 (solve) where

import Data.Char (isNumber)
import Data.Function (on)
import Data.List (find, group, groupBy, sort)
import Data.Set (empty, insert, notMember, size)

data Claim = Claim { cid :: Int, coords :: [(Int, Int)] }

toClaim :: [Int] -> Claim
toClaim [i, ulx, uly, w, h] = Claim { cid = i, coords = plot ulx uly w h }
  where plot ulx uly w h = [(x, y) | x <- take w [(ulx + 1)..], y <- take h [(uly + 1)..]]

parseLine :: String -> Claim
parseLine = toClaim . map read . filter (all isNumber) . groupBy ((==) `on` isNumber)

-- remove unique elements from a list
-- list order is not preserved
removeUniques :: Ord a => [a] -> [a]
removeUniques = concat . filter ((>1) . length) . group . sort

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    claims = map parseLine $ lines input
    uniqueDupes = foldl (flip insert) empty . removeUniques . concatMap coords $ claims
    part1 = show . size $ uniqueDupes
    (Just part2) = show . cid <$> find (all (`notMember` uniqueDupes) . coords) claims
