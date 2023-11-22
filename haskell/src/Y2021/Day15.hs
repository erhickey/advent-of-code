module Y2021.Day15 (solve) where

import Control.Monad (filterM)
import Control.Monad.ST (ST)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.MArray (newArray)
import Data.Array.ST (runSTUArray, STUArray)
import Data.Array.Unboxed (bounds, UArray)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Vector (Vector)
import qualified Data.Vector as V (fromList, length, unsafeIndex)
import qualified Data.PQueue.Min as PQ (insert, minView, MinQueue, null, singleton)

type Coord = (Int, Int)
type Grid = [(Coord, Int)]
type Point = (Int, [Int])
type Points = [(Int, Point)]

parse :: [String] -> Grid
parse = concat . zipWith parseLine [0..]
  where parseLine y = zipWith (\x n -> ((y, x), read [n])) [0..]

expandGrid :: Grid -> Grid
expandGrid g =  concatMap (gridToDown (concatMap (gridToRight g) [0..4])) [0..4]
  where
    mx = maxX g + 1
    my = maxY g + 1
    inc n = map (\(c, v) -> if v + n > 9 then (c, v + n - 9) else (c, v + n))
    gridToRight g' n = inc n $ map (\((y, x), v) -> ((y, x + mx * n), v)) g'
    gridToDown g' n = inc n $ map (\((y, x), v) -> ((y + my * n, x), v)) g'

maxX :: Grid -> Int
maxX = maximum . map (snd . fst)

maxY :: Grid -> Int
maxY = maximum . map (fst . fst)

-- map of available nodes to distance from start
dijkstra
  :: Int              -- start
  -> Int              -- end
  -> [Int]            -- available nodes
  -> (Int -> [Int])   -- neighbors of node
  -> (Int -> Int)     -- distance to node
  -> UArray Int Int   -- nodes to distance from start
dijkstra start end as neighbF distF =
  runSTUArray $ do
    arr <- newArray (minimum as, maximum as) (-1)
    unsafeWrite arr start 0
    go (PQ.singleton (0, start)) arr
  where
     -- update array with new distance if it is lower
    updateArr :: STUArray s Int Int -> (Int, Int) -> ST s ()
    updateArr arr (v, n) = do
      v' <- unsafeRead arr n
      if v' == -1 then unsafeWrite arr n v else unsafeWrite arr n (min v v')
    isUnvisited :: STUArray s Int Int -> Int -> ST s Bool
    isUnvisited arr n = (==(-1)) <$> unsafeRead arr n
    go :: PQ.MinQueue (Int, Int) -> STUArray s Int Int -> ST s (STUArray s Int Int)
    go pq arr
      | PQ.null pq || n == end = pure arr
      | otherwise = do
        -- current node distance
        d <- unsafeRead arr n
        -- unvisited neighbors of current node
        uns <- filterM (isUnvisited arr) $ neighbF n
        -- unvisited neighbors of current node and their distances
        let unvs = map (\un -> (d + distF un, un)) uns
        -- update arry with new distances
        mapM_ (updateArr arr) unvs
        -- add unvisited neighbors to priority queue
        let pq'' = foldl (flip PQ.insert) pq' unvs
        go pq'' arr
      where Just ((_, n), pq') = PQ.minView pq

distances :: Grid -> UArray Int Int
distances g = dijkstra 0 end ixs (snd . V.unsafeIndex psVec) (fst . V.unsafeIndex psVec)
  where
    psVec = gridToPoints g
    end = V.length psVec - 1
    ixs = [0..end]

gridToPoints :: Grid -> Vector Point
gridToPoints g = V.fromList ps
  where
    ps = map (\(c, v) -> (v, map (flatIndex (mx + 1)) $ neighbors c)) $ sort g
    mx = maxX g
    my = maxY g
    -- verbose for the sake of efficiency
    neighbors c = catMaybes [l c, r c, d c, u c]
    l (y, x) = if x > 0 then pure (y, x - 1) else Nothing
    r (y, x) = if x < mx then pure (y, x + 1) else Nothing
    u (y, x) = if y > 0 then pure (y - 1, x) else Nothing
    d (y, x) = if y < my then pure (y + 1, x) else Nothing

flatIndex :: Int -> (Int, Int) -> Int
flatIndex mx (y, x) = (y * mx) + x

answer :: Grid -> Int
answer g = unsafeAt arr end
  where
    arr = distances g
    (_, end) = bounds arr

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    grid = parse $ lines input
    part1 = show $ answer grid
    part2 = show . answer $ expandGrid grid
