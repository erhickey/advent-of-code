import Data.List.Split (splitOn)

import Intcode (ProgramState(..), runIntcodeXMem)

boost :: [Int] -> Int -> [Int]
boost xs n = psOutput . runIntcodeXMem 1 $ ProgramState xs [n] [] 0 0

main = do
  input <- map read . splitOn "," <$> readFile "day9.input"
  print . (++) "Part 1: " . show $ boost input 1
  print . (++) "Part 2: " . show $ boost input 2
