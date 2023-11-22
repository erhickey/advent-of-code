module Y2016.Day3 (solve) where

type Triangle = (Int, Int, Int)

parseTriangle :: String -> Triangle
parseTriangle = (\(x:y:z:_) -> (x, y, z)) . map read . words

parseTriangles :: [String] -> [Triangle]
parseTriangles [] = []
parseTriangles (x:y:z:xs) = [(xx, yx, zx), (xy, yy, zy), (xz, yz, zz)] ++ parseTriangles xs
  where (xx, xy, xz) = parseTriangle x
        (yx, yy, yz) = parseTriangle y
        (zx, zy, zz) = parseTriangle z

valid :: Triangle -> Bool
valid (x, y, z)
  | x + y <= z = False
  | x + z <= y = False
  | y + z <= x = False
  | otherwise = True

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = show . length . filter valid . map parseTriangle . lines $ input
    part2 = show . length . filter valid $ parseTriangles . lines $ input
