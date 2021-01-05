import Data.Char (isNumber)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M (delete, elems, map, singleton, unions)
import qualified Data.Set as S (fromList, notMember, unions)

import Data.Bits (setBit)
import Data.List.Split (splitOn)

data Edges = Edges
  { topEdge :: Int
  , rightEdge :: Int
  , botEdge :: Int
  , leftEdge :: Int
  }

data Tile = Tile
  { tileNumber :: Int
  , tileArray :: [String]
  , orientation :: Edges
  }

parse :: [String] -> IntMap [Tile]
parse (t:xs) = M.singleton tNum . map (Tile tNum xs) $ parseEdges xs
  where
    tNum = read $ filter isNumber t

parseEdges :: [String] -> [Edges]
parseEdges xs = concat
  [ rotations $ edges (topEdge, rightEdge, botEdge, leftEdge)
  , rotations $ edges (reverse topEdge, rightEdge, reverse botEdge, leftEdge)
  , rotations $ edges (topEdge, reverse rightEdge, botEdge, reverse leftEdge)
  ]
  where
    topEdge = head xs
    botEdge = last xs
    rightEdge = map last xs
    leftEdge = map head xs
    edges (t, r, b, l) = Edges (parseEdge t) (parseEdge r) (parseEdge b) (parseEdge l)
    rotations = take 4 . iterate (\(Edges t r b l) -> Edges l t r b)
    parseEdge = foldl go 0 . zip [0..]
      where go acc (_, '.') = acc
            go acc (n, '#') = setBit acc n

uniqueTiles :: [Edges -> Int] -> IntMap [Tile] -> [Tile]
uniqueTiles fs tm = filter unique . concat $ M.elems tm
  where
    sm = M.map uniqueEdges tm
    uniqueEdges = S.fromList . concatMap ((\(Edges t r b l) -> [t,r,b,l]) . orientation)
    unique t = all (flip S.notMember (S.unions . M.elems $ M.delete (tileNumber t) sm) . ($ orientation t)) fs

part1 :: IntMap [Tile] -> Int
part1 = product . S.fromList . map tileNumber . uniqueTiles [topEdge,leftEdge]

main = do
  input <- M.unions . map (parse . lines) . filter (not . null) . splitOn "\n\n" <$> readFile "day20.input"
  print . (++) "Part 1: " . show $ part1 input
