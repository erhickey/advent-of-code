module Y2019.Day7 (solve) where

import Data.List (permutations)

import Data.List.Split (splitOn)

import Y2019.Intcode (runIntcode, ProgramState(..))

part1 :: [Int] -> Int
part1 xs = maximum . map (last . go) $ permutations [0..4]
  where go = foldl (\input phase -> psOutput . runIntcode $ ProgramState xs (phase:input) [] 0 0) [0]

part2 :: [Int] -> Int
part2 xs = maximum . map (feedbackLoop [0] 0 . map (\n -> ProgramState xs [n] [] 0 0)) $ permutations [5..9]

feedbackLoop :: [Int] -> Int -> [ProgramState] -> Int
feedbackLoop input n xs =
  let pos = n `mod` length xs
      amp = xs !! pos
      newState = runIntcode $ ProgramState (psIntcode amp) (psInput amp ++ input) [] (psIndex amp) (psRBase amp)
      output = psOutput newState
      newAmps = take pos xs ++ newState:drop (pos + 1) xs
  in  if complete newAmps then last output else feedbackLoop output (n + 1) newAmps
  where complete xs = (==) (length xs) . length . filter (== -1) $ map psIndex xs

main = do
  input <- map read . splitOn "," <$> readFile "day7.input"
  print . (++) "Part 1: " . show $ part1 input
  print . (++) "Part 2: " . show $ part2 input

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
