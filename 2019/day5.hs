import Data.List.Split (splitOn)

import Intcode (ProgramState(..), runIntcode)

main = do
  input <- map read . splitOn "," <$> readFile "day5.input"
  let p1 = ProgramState input [1] [] 0 0
      p2 = ProgramState input [5] [] 0 0
  print . (++) "Part 1: " . show . psOutput $ runIntcode p1
  print . (++) "Part 2: " . show . psOutput $ runIntcode p2
