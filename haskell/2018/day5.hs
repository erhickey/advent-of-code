import Data.Char (toUpper)

react :: String -> String
react = foldr step ""
  where step x (y:ys)
          | x /= y && toUpper x == toUpper y = ys
        step x ys = x:ys

-- produce 26 variants of the input
-- where each variant has a different letter of the alphabet removed
part2 :: String -> [String]
part2 = zipWith (\c xs -> filter ((/=c) . toUpper) xs) ['A'..'Z'] . replicate 26

main = do
  input <- filter (/= '\n') <$> readFile "day5.input"
  print . (++) "Part 1: " . show . length . react $ input
  print . (++) "Part 2: " . show . minimum . map (length . react) $ part2 input
