main = do
  input <- map read . lines <$> readFile "day1.input"
  print . (++) "Part 1: " . show . head $ [ x * y | x <- input, y <- input, x + y == 2020 ]
  print . (++) "Part 2: " . show . head $ [ x * y * z | x <- input, y <- input, z <- input, x + y + z == 2020 ]
