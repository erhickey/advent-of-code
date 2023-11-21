module Y2020.Day21 (solve) where

-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TupleSections #-}
-- import Control.Applicative ((<|>))
-- import Data.Bifunctor (second)
-- import Data.Functor ((<&>))
-- import Data.List (intercalate)
-- import Data.Map (Map)
-- import qualified Data.Map as M (delete, elems, filter, fromList, fromListWith, map, toList)
-- import Data.Set (Set)
-- import qualified Data.Set as S (delete, findMin, fromList, intersection, size)
-- import qualified Data.Text.IO as T (readFile)

-- import Data.Attoparsec.Text (char, letter, many', manyTill, parseOnly, Parser, sepBy, string)

-- ingredientsParser :: Parser ([String], Map String [Set String])
-- ingredientsParser = do
--   xs <- many' lineParser
--   pure (concatMap fst xs, M.fromListWith (++) $ concatMap (map (second pure) . snd) xs)

-- lineParser :: Parser ([String], [(String, Set String)])
-- lineParser = do
--   is <- filter (/="") <$> many' letter `sepBy` char ' '
--   string "(contains "
--   as <- many' letter `sepBy` string ", " <|> (manyTill letter (char ')') <&> pure)
--   string ")\n"
--   pure (is, map (,S.fromList is) as)

-- determineAllergens :: Map String [Set String] -> Map String String
-- determineAllergens x = M.fromList $ go x []
--   where
--     go m acc
--       | null m = acc
--       | otherwise = go m' acc ++ as
--       where
--         as = findAllergens m
--         m' = foldl del m as
--         del m (k, v) = M.map (map (S.delete v)) $ M.delete k m

-- findAllergens :: Map String [Set String] -> [(String, String)]
-- findAllergens = M.toList . M.map S.findMin . M.filter ((==1) . S.size) . M.map (foldl1 S.intersection)

-- filterAllergens :: [String] -> Map String [Set String] -> [String]
-- filterAllergens is m = filter (`notElem` allergens) is
--   where allergens = M.elems $ determineAllergens m

-- main = do
--   (Right (ingredients, allergens)) <- parseOnly ingredientsParser <$> T.readFile "day21.input"
--   print . (++) "Part 1: " . show . length $ filterAllergens ingredients allergens
--   print . (++) "Part 2: " . intercalate "," . M.elems $ determineAllergens allergens

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
