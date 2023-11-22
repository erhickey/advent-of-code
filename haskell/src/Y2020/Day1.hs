module Y2020.Day1 (solve) where

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ns = map read $ lines input
    part1 = show . head $ [ x * y | x <- ns, y <- ns, x + y == 2020 ]
    part2 = show . head $ [ x * y * z | x <- ns, y <- ns, z <- ns, x + y + z == 2020 ]
