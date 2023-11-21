module Y2020.Day15 (solve) where

import Control.Monad (zipWithM_)
import Control.Monad.ST (runST, ST)

import Control.Monad.Loops (iterateUntilM)
import Data.List.Split (splitOn)
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as V (replicate, unsafeRead, unsafeWrite)

type GameState = (Int, Int)

doTurn :: MVector s Int -> GameState -> ST s GameState
doTurn vec (ln, t) = do
  lp <- V.unsafeRead vec ln
  let n = if lp == -1 then 0 else t - lp
  V.unsafeWrite vec ln t
  pure (n, t + 1)

play :: Int -> [Int] -> Int
play n xs = runST $ do
  vec <- V.replicate n (-1)
  zipWithM_ (V.unsafeWrite vec) (init xs) [1..]
  let state = (last xs, length xs)
  (ln,_) <- iterateUntilM ((== n) . snd) (doTurn vec) state
  pure ln

main = do
  input <- map read . splitOn "," . filter (/='\n') <$> readFile "day15.input"
  print . (++) "Part 1: " . show $ play 2020 input
  print . (++) "Part 2: " . show $ play 30000000 input

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
