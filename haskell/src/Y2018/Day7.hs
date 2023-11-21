module Y2018.Day7 (solve) where

import Data.Char (ord)
import Data.Maybe (isJust)
import qualified Data.Map as M (delete, filter, fromListWith, keys, map, Map, null)
import qualified Data.Set as S (empty, filter, Set, singleton, union)

type Deps = M.Map Char (S.Set Char)

parse :: String -> Deps
parse = M.fromListWith S.union . concatMap (\l -> [(l !! 5, S.empty), (l !! 36, S.singleton $ l !! 5)]) . lines

nextStep :: Deps -> Maybe Char
nextStep m
  | null available = Nothing
  | otherwise = pure . head $ M.keys available
  where available = M.filter (==S.empty) m

completeStep :: Char -> Deps -> Deps
completeStep c = M.map (S.filter (/=c)) . M.delete c

stepOrder :: Deps -> String
stepOrder m
  | M.null m = []
  | otherwise = next:stepOrder (completeStep next m)
  where Just next = nextStep m

part2 :: Deps -> Int
part2 = go 0 []
  where go n ws m
          | M.null m = (+) n . maximum . map fst $ ws
          | length ws < 5 && isJust next = let (Just c) = next in go n ((ord c - 4, c):ws) $ M.delete c m
          | otherwise = go (n + fst done) (dec (fst done) . filter (/=done) $ ws) $ completeStep (snd done) m
          where next = nextStep m
                done = minimum ws
                dec i = map (\(x,y) -> (x - i, y))

main = do
    input <- parse <$> readFile "day7.input"
    print . (++) "Part 1: " $ stepOrder input
    print . (++) "Part 2: " . show . part2 $ input

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
