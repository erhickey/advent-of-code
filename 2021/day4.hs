import Data.List (inits, sort, transpose)
import Data.List.Split (splitOn)
import Data.List.Unique (uniq)

type Board = [[Int]]

parse :: String -> ([Int], [Board])
parse s = (map read . splitOn "," $ head ss, map parseBoard $ tail ss)
  where ss = splitOn "\n\n" s

parseBoard :: String -> Board
parseBoard s = rows ++ (transpose rows)
  where rows = map (map read . words) $ lines s

won :: [Int] -> Board -> Bool
won ns = any ((all (`elem` ns)))

winner :: [Int] -> [Board] -> ([Int], Board)
winner ns bs = (\(x, y) -> (x, head y)) . head . dropWhile (null . snd) . map go $ inits ns
  where go xs = (xs, filter (won xs) bs)

loser :: [Int] -> [Board] -> Board
loser ns bs = head . head . dropWhile ((/=) 1 . length) . scanl go bs $ inits ns
  where go bs' ns' = filter (not . won ns') bs'

score :: ([Int], Board) -> Int
score (ns, b) = last ns * (sum . uniq . sort . filter (not . (`elem` ns)) $ concat b)

main = do
  input <- parse <$> readFile "day4.input"
  print . (++) "Part 1: " . show . score $ uncurry winner input
  print . (++) "Part 2: " . show . score . winner (fst input) . (:[]) $ uncurry loser input
