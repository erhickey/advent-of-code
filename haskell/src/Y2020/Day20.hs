module Y2020.Day20 (solve) where

import Data.Char (isNumber)
import Data.Maybe (catMaybes)
import Data.Foldable (asum)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM ((!), delete, elems, keysSet, map, singleton, unions)
import Data.IntSet (IntSet)
import qualified Data.IntSet as S (filter, foldl, fromList, isSubsetOf, map, notMember, null, size, toList, union, unions)
import Data.List (transpose)
import Data.Map ((!), Map)
import qualified Data.Map as M (empty, insert, keys)

import Data.Bits (setBit)
import Data.List.Split (splitOn)

data Edges = Edges
  { topEdge :: Int
  , rightEdge :: Int
  , botEdge :: Int
  , leftEdge :: Int
  }
  deriving (Show)

data Tile = Tile
  { tileNumber :: Int
  , tileArray :: [String]
  , orientation :: Edges
  }
  deriving (Show)

type Puzzle = Map (Int, Int) Tile

parse :: [String] -> IntMap [Tile]
parse (t:xs) = IM.singleton tNum . map (uncurry (Tile tNum)) $ parseEdges xs
  where
    tNum = read $ filter isNumber t

parseEdges :: [String] -> [([String], Edges)]
parseEdges xs = map (\ss -> (ss, edges ss)) $ transformations xs
  where
    parseEdge = foldl go 0 . zip [0..]
    go acc (_, '.') = acc
    go acc (n, '#') = setBit acc n
    edges ss = let
        topEdge = parseEdge $ head ss
        botEdge = parseEdge $ last ss
        rightEdge = parseEdge $ map last ss
        leftEdge = parseEdge $ map head ss
      in Edges topEdge rightEdge botEdge leftEdge

transformations :: [[a]] -> [[[a]]]
transformations xs = concat
  [ rotations xs
  , rotations $ flipHorizontally xs
  , rotations $ flipVertically xs
  , rotations $ flipHAndV xs
  ]

rotations :: [[a]] -> [[[a]]]
rotations = take 4 . iterate (transpose . reverse)

flipHorizontally :: [[a]] -> [[a]]
flipHorizontally = map reverse

flipVertically :: [[a]] -> [[a]]
flipVertically = transpose . map reverse . transpose

flipHAndV :: [[a]] -> [[a]]
flipHAndV = flipVertically . flipHorizontally

uniqueTiles :: [Edges -> Int] -> IntMap [Tile] -> IntSet
uniqueTiles fs tm = S.fromList . map tileNumber . filter unique . concat $ IM.elems tm
  where
    sm = IM.map uniqueEdges tm
    uniqueEdges = S.fromList . concatMap ((\(Edges t r b l) -> [t,r,b,l]) . orientation)
    unique t = all (flip S.notMember (S.unions . IM.elems $ IM.delete (tileNumber t) sm) . ($ orientation t)) fs

solvePuzzle :: IntMap [Tile] -> Maybe Puzzle
solvePuzzle tm = go M.empty corners sides interiors (1, 1) $ S.size sides `div` 4 + 2
  where
    deleteTile t xs = S.filter (/=tileNumber t) xs
    deleteTiles m ts = S.foldl (flip IM.delete) m ts
    corners = uniqueTiles [topEdge, leftEdge] tm
    cornersRem = deleteTiles tm corners
    sides = uniqueTiles [leftEdge] cornersRem
    interiors = IM.keysSet $ deleteTiles cornersRem sides
    go acc cs ss is (y, x) size
      | S.null cs = pure acc
      | null ts = Nothing
      | otherwise = asum $ map go2 ts
      where
        go2 tile = go (M.insert (y, x) tile acc) (deleteTile tile cs) (deleteTile tile ss) (deleteTile tile is) (y', x') size
        leftSide = rightEdge . orientation $ acc ! (y, x - 1)
        topSide = botEdge . orientation $ acc ! (y - 1, x)
        cornerTiles = concatMap (tm IM.!) $ S.toList cs
        sideTiles = concatMap (tm IM.!) $ S.toList ss
        interiorTiles = concatMap (tm IM.!) $ S.toList is
        (ts, y', x')
          | x == 1 && y == 1 = (cornerTiles, y, x + 1)
          | x == size && y == 1 = (findTile [(==leftSide) . leftEdge] cornerTiles, y + 1, 1)
          | x == 1 && y == size = (findTile [(==topSide) . topEdge] cornerTiles, y, x + 1)
          | x == size && y == size = (findTile [(==leftSide) . leftEdge] cornerTiles, 0, 0)
          | y == 1 = (findTile [(==leftSide) . leftEdge] sideTiles, y, x + 1)
          | x == 1 = (findTile [(==topSide) . topEdge] sideTiles, y, x + 1)
          | x == size = (findTile [(==leftSide) . leftEdge, (==topSide) . topEdge] sideTiles, y + 1, 1)
          | y == size = (findTile [(==leftSide) . leftEdge, (==topSide) . topEdge] sideTiles, y, x + 1)
          | otherwise = (findTile [(==leftSide) . leftEdge, (==topSide) . topEdge] interiorTiles, y, x + 1)

findTile :: [Edges -> Bool] -> [Tile] -> [Tile]
findTile fs = filter (\t -> all ($ orientation t) fs)

assemblePuzzle :: Puzzle -> [String]
assemblePuzzle m = concatMap (foldl1 combine) . foldl go [] $ M.keys m
  where
    go _ k@(1, 1) = [[trim $ tileArray (m ! k)]]
    go xs k@(_, 1) = xs ++ [[trim $ tileArray (m ! k)]]
    go xs k = init xs ++ [last xs ++ [trim $ tileArray (m ! k)]]
    trim = transpose . init . tail . transpose . init . tail
    combine xs ys = zipWith (++) xs ys

seaMonster :: (IntSet, IntSet)
seaMonster = (S.fromList [2,7,8,13,14,19,20,21], S.fromList [23,26,29,32,35,38])

numMonsters :: [String] -> Int
numMonsters xs = S.foldl findMonster 0 sea
  where
    monsterMargin = length (head xs) - 20
    sea = S.fromList . catMaybes . zipWith transform [0..] $ concat xs
    transform _ '.' = Nothing
    transform n '#' = Just n
    findMonster acc n = if S.isSubsetOf monster sea then acc + 1 else acc
      where monster = S.union (S.map (+(n+monsterMargin)) $ fst seaMonster) (S.map (+(n+monsterMargin+monsterMargin)) $ snd seaMonster)

p2 :: IntMap [Tile] -> Int
p2 tm = numWaves - monsters * 15
  where
    (Just p) = solvePuzzle tm
    puzzle = assemblePuzzle p
    ps = transformations puzzle
    monsters = maximum $ map numMonsters ps
    numWaves = length . filter (=='#') $ concat puzzle

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    es = IM.unions . map (parse . lines) . filter (not . null) $ splitOn "\n\n" input
    part1 = show . product . S.toList $ uniqueTiles [topEdge, leftEdge] es
    part2 = show $ p2 es
