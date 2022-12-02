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

main = do
  input <- lines <$> readFile "day3.input"
  let part1 = filter valid $ map parseTriangle input
      part2 = filter valid $ parseTriangles input
  print . (++) "Part 1: " . show $ length part1
  print . (++) "Part 2: " . show $ length part2
