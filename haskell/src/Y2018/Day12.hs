module Y2018.Day12 (solve) where

import Data.Map (Map)
import qualified Data.Map as M (findWithDefault, fromList)

type Rules = Map String Char

type State = (Int, Int, Bool, String)

parseNotes :: [String] -> Rules
parseNotes = M.fromList . map (\s -> (takeWhile (/= ' ') s, last s))

applyRules :: Rules -> String -> Char
applyRules r s = M.findWithDefault (s !! 2) s r

-- returns all n-sized chunks of a list
-- example:
--  chunks [1..5] 2 = [[1,2],[2,3],[3,4],[4,5]]
chunks :: Int -> [a] -> [[a]]
chunks n xs
  | length xs <= n = [xs]
  | otherwise = take n xs : chunks n (tail xs)

advance :: State -> Rules -> State
advance (it, pos, _, cState) rules = (it + 1, nPos, trimmed == cState, trimmed)
  where pre = "...." ++ cState ++ "...."
        nState = map (applyRules rules) $ chunks 5 pre
        nPos = (pos - 2) + length (takeWhile (=='.') nState)
        trimmed = trim '.' nState

-- trim all leading/trailing instances of character
trim :: Char -> String -> String
trim c = reverse . dropWhile (==c) . reverse . dropWhile (==c)

stateSum :: State -> Int
stateSum (_, pos, _, s) = sum $ zipWith go [pos..] s
    where go n c
            | c == '#' = n
            | otherwise = 0

interpolateSum :: Int -> Rules -> State -> Int
interpolateSum n rules state@(it, _, _, _) =
    let currSum = stateSum state
        nextSum = stateSum $ advance state rules
        mult = nextSum - currSum
    in currSum + (n - it) * mult

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ls = filter (not . null) $ lines input
    initialState = (0, 0, False, drop 15 $ head ls)
    rules = parseNotes $ tail ls
    generations = iterate (`advance` rules) initialState
    part1 = show . stateSum $ generations !! 20
    part2 = show . interpolateSum 50000000000 rules . head $ dropWhile (\(_,_,c,_) -> not c) generations
