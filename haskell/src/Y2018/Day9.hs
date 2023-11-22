module Y2018.Day9 (solve) where

import qualified Data.IntMap as M (elems, empty, insertWith, IntMap)
import Data.List (foldl')
import Data.Sequence ((<|), (><), Seq, viewl, ViewL((:<)))
import qualified Data.Sequence as S (length, singleton, splitAt)
import System.Environment (getArgs)

data GameState = GameState
  { maxPlayers :: Int
  , marbles :: Seq Int
  , scores :: M.IntMap Int
  }

move :: Int -> Seq a -> Seq a
move n s = r >< l
  where (l, r) = S.splitAt (n `mod` S.length s) s

doTurn :: GameState -> Int -> GameState
doTurn gs t = GameState
  { maxPlayers = maxPlayers gs
  , marbles = cMarbles
  , scores = cScores
  }
  where scoringTurn = t `rem` 23 == 0
        (score, cMarbles)
          | scoringTurn = let (m :< ms) = viewl $ move (-7) (marbles gs) in (m + t, ms)
          | otherwise = (0, t <| move 2 (marbles gs))
        player = t `mod` maxPlayers gs
        cScores = M.insertWith (+) player score (scores gs)

parseInput :: String -> (Int, Int)
parseInput input = (read players, read turns)
  where [players, _, _, _, _, _, turns, _] = words input

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    (players, maxTurns) = parseInput input
    gs = GameState
        { maxPlayers = players
        , marbles = S.singleton 0
        , scores = M.empty
        }
    p1Results = foldl' doTurn gs [1..maxTurns]
    p2Results = foldl' doTurn gs [1..(maxTurns * 100)]
    part1 = show . maximum . M.elems $ scores p1Results
    part2 = show . maximum . M.elems $ scores p2Results
