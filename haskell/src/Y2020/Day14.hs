module Y2020.Day14 (solve) where

import Data.Bits (clearBit, setBit)
import Data.Char (isNumber)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M (elems, empty, insert)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)

data MaskBit = ZERO | ONE | X deriving (Eq)

type MaskPair = (Int, MaskBit)
type Mask = [MaskPair]
type Value = (Int, Int)
type Section = (Mask, [Value])
type Program = [Section]

parse :: [String] -> Program
parse = reverse . go []
  where
    go ps xs
      | null xs = ps
      | otherwise = go (ss:ps) rem
      where (ss, rem) = parseSection xs

parseSection :: [String] -> (Section, [String])
parseSection (x:xs) = ((parseMask x, map parseValue vs), rem)
  where (vs, rem) = break ("mask" `isPrefixOf`) xs

parseMask :: String -> Mask
parseMask = catMaybes . zipWith go [0..] . reverse
  where go n '1' = pure (n, ONE)
        go n '0' = pure (n, ZERO)
        go n 'X' = pure (n, X)
        go _ _ = Nothing

parseValue :: String -> Value
parseValue s = (read $ filter isNumber a, read $ filter isNumber v)
  where (a, v) = break (=='=') s

initialize :: (Mask -> IntMap Int -> Value -> IntMap Int) -> Program -> IntMap Int
initialize f = foldl go M.empty
  where go m (mask, vs) = foldl (f mask) m vs

version1 :: Mask -> IntMap Int -> Value -> IntMap Int
version1 ms m (k, v) = M.insert k (foldl go v ms) m
  where
    go v (n, ONE) = setBit v n
    go v (n, ZERO) = clearBit v n
    go v _ = v

v2Mask :: Mask -> Int -> [Int]
v2Mask ms v = applyFloating $ foldl go v ms
  where
    go v (n, ONE) = setBit v n
    go v _ = v
    floatingBits = map fst $ filter ((==X) . snd) ms
    applyFloating x = foldl applyFloatingBit [x] floatingBits
    applyFloatingBit ys n = concatMap (\y -> [setBit y n, clearBit y n]) ys

version2 :: Mask -> IntMap Int -> Value -> IntMap Int
version2 ms m (k, v) = foldl go m $ v2Mask ms k
  where go acc x = M.insert x v acc

main = do
  input <- parse . lines <$> readFile "day14.input"
  print . (++) "Part 1: " . show . sum . M.elems $ initialize version1 input
  print . (++) "Part 2: " . show . sum . M.elems $ initialize version2 input

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
