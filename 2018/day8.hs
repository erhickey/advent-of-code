import Data.Tree (Tree(Node))

parseTree :: [Int] -> Tree [Int]
parseTree xs = head . fst $ go ([], xs)
  where go (ns, c:m:xs) = (Node (take m rs) (reverse ss):ns, drop m rs)
          where (ss, rs) = iterate go ([], xs) !! c

nodeValue :: Tree [Int] -> Int
nodeValue (Node ms []) = sum ms
nodeValue (Node ms cs) = sum [ nodeValue $ cs !! (m - 1) | m <- ms, m <= length cs, m /= 0]

main = do
    tree <- parseTree . map read . words <$> readFile "day8.input"
    print . (++) "Part 1: " . show . sum $ fmap sum tree
    print . (++) "Part 2: " . show $ nodeValue tree
