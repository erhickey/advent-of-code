module Y2015.Day1 (solve) where

parseInstruction :: Char -> Int
parseInstruction '(' = 1
parseInstruction ')' = -1

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    instructions = filter (/= '\n') input
    part1 = show . sum $ map parseInstruction instructions
    part2 = show . length . takeWhile (/=(-1)) $ scanl (\acc c -> acc + parseInstruction c) 0 instructions
