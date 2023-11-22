module Y2021.Day4 (solve) where

import Data.Bifunctor (second)
import Data.List (inits, sort, transpose)
import Data.List.Split (splitOn)
import Data.List.Unique (uniq)

type Board = [[Int]]

parse :: String -> ([Int], [Board])
parse s = (map read . splitOn "," $ head ss, map parseBoard $ tail ss)
  where ss = splitOn "\n\n" s

parseBoard :: String -> Board
parseBoard s = rows ++ transpose rows
  where rows = map (map read . words) $ lines s

won :: [Int] -> Board -> Bool
won ns = any (all (`elem` ns))

winner :: [Int] -> [Board] -> ([Int], Board)
winner ns bs = second head . head . dropWhile (null . snd) . map go $ inits ns
  where go xs = (xs, filter (won xs) bs)

loser :: [Int] -> [Board] -> Board
loser ns bs = head . head . dropWhile ((/=) 1 . length) . scanl go bs $ inits ns
  where go bs' ns' = filter (not . won ns') bs'

score :: ([Int], Board) -> Int
score (ns, b) = last ns * (sum . uniq . sort . filter (not . (`elem` ns)) $ concat b)

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    board = parse input
    part1 = show . score $ uncurry winner board
    part2 = show . score . winner (fst board) . (:[]) $ uncurry loser board
