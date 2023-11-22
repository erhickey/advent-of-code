module Y2018.Day8 (solve) where

import Data.Tree (Tree(Node))

parseTree :: [Int] -> Tree [Int]
parseTree xs = head . fst $ go ([], xs)
  where go (ns, c:m:xs) = (Node (take m rs) (reverse ss):ns, drop m rs)
          where (ss, rs) = iterate go ([], xs) !! c

nodeValue :: Tree [Int] -> Int
nodeValue (Node ms []) = sum ms
nodeValue (Node ms cs) = sum [ nodeValue $ cs !! (m - 1) | m <- ms, m <= length cs, m /= 0]

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    tree = parseTree . map read $ words input
    part1 = show . sum $ fmap sum tree
    part2 = show $ nodeValue tree
