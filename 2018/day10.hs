import Data.Char (isNumber)
import qualified Data.Set as S (fromList, map, member, Set)

data Light = Light (Int, Int) (Int, Int)

instance Eq Light where
  (Light (x1, y1) _) == (Light (x2, y2) _) = x1 == x2 && y1 == y2

instance Ord Light where
  (Light (x1, y1) _) `compare` (Light (x2, y2) _)
    | x1 == x2 = y1 `compare` y2
    | otherwise = x1 `compare` x2

parse :: String -> Light
parse s = let ns = words . filter (\c -> isNumber c || c == ' ' || c == '-') $ s
          in Light (read $ head ns, read $ ns !! 1) (read $ ns !! 2, read $ ns !! 3)

move :: Int -> Light -> Light
move n (Light (x, y) (h, v)) = Light (x + h * n, y + v * n) (h, v)

containsMessage :: S.Set Light -> Bool
containsMessage s = all touching s
  where touching (Light (x, y) v) =
          S.member (Light (x - 1, y - 1) v) s || S.member (Light (x - 1, y) v) s || S.member (Light (x - 1, y + 1) v) s ||
          S.member (Light (x + 1, y - 1) v) s || S.member (Light (x + 1, y) v) s || S.member (Light (x + 1, y + 1) v) s ||
          S.member (Light (x, y - 1) v) s || S.member (Light (x, y + 1) v) s

display :: S.Set Light -> [String]
display s =
  let xs = S.map (\(Light (x, _) _) -> x) s
      ys = S.map (\(Light (_, y) _) -> y) s
  in [[ if S.member (Light (x, y) (0, 0)) s then '#' else ' ' | x <- [minimum xs..maximum xs]] | y <- [minimum ys..maximum ys]]

main = do
  input <- S.fromList . map parse . lines <$> readFile "day10.input"
  let (dur, msg) = break containsMessage $ map (\n -> S.map (move n) input) [0..]
  mapM_ putStrLn . display . head $ msg
  print . (++) "Part 2: " . show $ length dur
