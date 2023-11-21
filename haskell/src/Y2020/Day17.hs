module Y2020.Day17 (solve) where

import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S (fromList, intersection, map, member, toList, union)

data Coord =
    ThreeD (Int, Int, Int)
  | FourD (Int, Int, Int, Int)
  deriving (Eq, Ord)

parse :: [String] -> Set Coord
parse = S.fromList . catMaybes . concat . zipWith parseLine [0..]
  where
    parseLine y xs = zipWith (parseChar y) [0..] xs
    parseChar y x '#' = pure $ ThreeD (x, y, 0)
    parseChar _ _ '.' = Nothing

addDimension :: Coord -> Coord
addDimension (ThreeD (x, y, z)) = FourD (x, y, z, 0)

neighbors :: Coord -> Set Coord
neighbors (ThreeD (x, y, z)) = S.fromList [ ThreeD (x', y', z') | x' <- [x-1,x,x+1], y' <- [y-1,y,y+1], z' <- [z-1,z,z+1], (x, y, z) /= (x', y', z') ]
neighbors (FourD (x, y, z, w)) = S.fromList [ FourD (x', y', z', w') | x' <- [x-1,x,x+1], y' <- [y-1,y,y+1], z' <- [z-1,z,z+1], w' <- [w-1,w,w+1], (x, y, z, w) /= (x', y', z', w') ]

allNeighbors :: Set Coord -> Set Coord
allNeighbors = go . go
  where go = foldr1 S.union . map neighbors . S.toList

activeNeighbors :: Coord -> Set Coord -> Set Coord
activeNeighbors c s = S.intersection s $ neighbors c

staysActive :: Coord -> Set Coord -> Bool
staysActive c s
  | l == 2 || l == 3 = True
  | otherwise = False
  where l = length $ activeNeighbors c s

becomesActive :: Coord -> Set Coord -> Bool
becomesActive c s
  | l == 3 = True
  | otherwise = False
  where l = length $ activeNeighbors c s

doCycle :: Set Coord -> Set Coord
doCycle s = S.fromList . mapMaybe go . S.toList $ allNeighbors s
  where
    go c
      | S.member c s = if staysActive c s then pure c else Nothing
      | otherwise = if becomesActive c s then pure c else Nothing

main = do
  input <- parse . lines <$> readFile "day17.test"
  print . (++) "Part 1: " . show . length $ iterate doCycle input !! 6
  print . (++) "Part 2: " . show . length $ iterate doCycle (S.map addDimension input) !! 6

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
