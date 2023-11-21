module Y2018.Day14 (solve) where

import Data.Foldable (toList)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Sequence ((|>), ViewR((:>)), Seq)
import qualified Data.Sequence as S (drop, empty, index, length, viewr)
import System.Environment (getArgs)

type RecipeState = (Int, Int, Seq Int)

combineRecipes :: RecipeState -> RecipeState
combineRecipes (ix1, ix2, rs) =
  let r1 = S.index rs ix1
      r2 = S.index rs ix2
      newRecipe = r1 + r2
      newRecipes
        | newRecipe >= 10 = [1, newRecipe - 10]
        | otherwise = [newRecipe]
      newSet = foldl (|>) rs newRecipes
      newIx1 = (ix1 + r1 + 1) `mod` S.length newSet
      newIx2 = (ix2 + r2 + 1) `mod` S.length newSet
  in (newIx1, newIx2, newSet)

endMatches :: Eq a => [a] -> Seq a -> Bool
endMatches xs s = go s $ reverse xs
  where go _ [] = True
        go s [x] = let (_ :> r) = S.viewr s in x == r
        go s (x:xs)
          | r == x = go s' xs
          | otherwise = False
          where (s' :> r) = S.viewr s

dropLast :: Seq a -> Seq a
dropLast s = let (x :> _) = S.viewr s in x

main = do
  args <- getArgs
  let input = head args
      numRecipes = read input
      toMatch = map (read . (:[])) input
      recipes = map (\(_,_,rs) -> rs) $ iterate combineRecipes (0, 1, S.empty |> 3 |> 7)
      p1 = S.drop numRecipes . head $ dropWhile ((numRecipes + 10 >) . S.length) recipes
      p2 = fromJust . find (endMatches toMatch) $ concatMap (\s -> [s, dropLast s]) recipes
  print . (++) "Part 1: " . concatMap show $ toList p1
  print . (++) "Part 2: " . show $ S.length p2 - length toMatch

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
