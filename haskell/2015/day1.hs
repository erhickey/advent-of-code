parseInstruction :: Char -> Int
parseInstruction '(' = 1
parseInstruction ')' = -1

main = do
  instructions <- filter (/= '\n') <$> readFile "day1.input"
  print . (++) "Part 1: " . show . sum $ map parseInstruction instructions
  print . (++) "Part 2: " . show . length . takeWhile (/=(-1)) $ scanl (\acc c -> acc + parseInstruction c) 0 instructions
