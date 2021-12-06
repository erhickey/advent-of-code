import Data.IntMap.Strict ((!), IntMap)
import qualified Data.IntMap.Strict as IM (fromList)

passDays :: Int -> Int -> Int
passDays 0 _ = 1
passDays t 0 = passDays (t - 1) 6 + passDays (t - 1) 8
passDays t f = if t > f then passDays (t - f) 0 else 1

buildMap :: Int -> IntMap Int
buildMap n = IM.fromList
  [ (1, passDays n 1)
  , (2, passDays n 2)
  , (3, passDays n 3)
  , (4, passDays n 4)
  , (5, passDays n 5)
  ]

answer :: Int -> [Int] -> Int
answer t = sum . map (fm !)
  where fm = buildMap t

main = do
  input <- read . (\s -> "[" ++ s ++ "]") <$> readFile "day6.input" :: IO [Int]
  print . (++) "Part 1: " . show $ answer 80 input
  print . (++) "Part 2: " . show $ answer 256 input
