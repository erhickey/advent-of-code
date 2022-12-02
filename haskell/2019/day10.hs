import Data.Function (on)
import Data.List (maximumBy, sortBy)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S (delete, filter, fromList, intersection, map, toList)

import Fraction (Fraction(..), reduce)

type Coord = (Int, Int)

parse :: [String] -> Set Coord
parse = S.fromList . concat . zipWith parseLine [0..]
  where
    parseLine y = catMaybes . zipWith (parseChar y) [0..]
    parseChar y x '#' = pure (x, y)
    parseChar _ _ _ = Nothing

between :: Coord -> Coord -> Set Coord
between (x1, y1) (x2, y2) = S.fromList $ zip xs ys
  where
    (rise :-: run) = reduce $ fromIntegral (abs (y1 - y2)) :-: fromIntegral (abs (x1 - x2))
    upSlope a b c = [a + c,a + c * 2..b - c]
    downSlope a b c = [a - c,a - c * 2..b + c]
    ys
      | y1 >= y2 = downSlope y1 y2 $ fromIntegral rise
      | otherwise = upSlope y1 y2 $ fromIntegral rise
    xs
      | x1 >= x2 = downSlope x1 x2 $ fromIntegral run
      | otherwise = upSlope x1 x2 $ fromIntegral run

isVisible :: Set Coord -> Coord -> Coord -> Bool
isVisible xs origin target = null objectsBetween
  where objectsBetween = S.intersection xs $ between origin target

getVisible :: Set Coord -> Coord -> Set Coord
getVisible xs origin = S.filter (isVisible xs origin) $ S.delete origin xs

best :: Set Coord -> (Coord, Set Coord)
best xs = maximumBy (compare `on` length . snd) ds
  where ds = S.map (\c -> (c, getVisible xs c)) xs

sortClockwise :: Coord -> Set Coord -> [Coord]
sortClockwise (oX, oY) = map snd . sortBy radSort . map (\c -> (radians c, c)) . S.toList
  where
    radians (x, y) = atan2 (fromIntegral $ x - oX) (fromIntegral $ y - oY)
    radSort (p1Rads, _) (p2Rads, _)
      | p1Rads > 0 && p2Rads > 0 = if p1Rads > p2Rads then LT else GT
      | p1Rads < 0 && p2Rads < 0 = if p1Rads > p2Rads then LT else GT
      | p1Rads > 0 && p2Rads < 0 = LT
      | p1Rads < 0 && p2Rads > 0 = GT
      | p1Rads == 0 = LT
      | p2Rads == 0 = GT

vaporize :: Coord -> Set Coord -> [Coord]
vaporize origin xs = foldl go [] $ sortClockwise origin xs
  where
    go acc target
      | isVisible xs origin target = acc ++ [target]
      | otherwise = acc

main = do
  input <- parse . lines <$> readFile "day10.input"
  let (station, visible) = best input
  print . (++) "Part 1: " . show $ length visible
  print . (++) "Part 2: " . show . (\(x, y) -> 100 * x + y) $ vaporize station visible !! 199
