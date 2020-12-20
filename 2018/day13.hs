import Data.List (delete, sort)
import Data.Map.Strict (keys, Map)
import qualified Data.Map.Strict as M (fromList, lookup)
import Data.Maybe (fromJust, mapMaybe)

data Heading = N | E | S | W deriving (Enum, Eq, Ord)

data Intent = L | C | R deriving (Eq, Ord)

type Coord = (Int, Int)

type Track = Map Coord Char

type Cart = (Coord, Heading, Intent)

type TrackState = (Track, [Cart])

parseTrack :: [String] -> TrackState
parseTrack xs = (M.fromList $ map fst parsed, mapMaybe snd parsed)
  where parsed = concat $ zipWith parseLine [0..] xs
        parseLine x = zipWith (parseChar x) [0..]
        parseChar x y c
          | c == '^' = ((xy, '|'), pure (xy, N, L))
          | c == '>' = ((xy, '-'), pure (xy, E, L))
          | c == 'v' = ((xy, '|'), pure (xy, S, L))
          | c == '<' = ((xy, '-'), pure (xy, W, L))
          | otherwise = ((xy, c), Nothing)
          where xy = (x, y)

turn :: Cart -> Cart
turn (c, h, i)
  | i == C = (c, h, R)
  | i == R = (c, if h == W then N else succ h, L)
  | i == L = (c, if h == N then W else pred h, C)

move :: Cart -> Track -> Cart
move ((y,x), h, i) tm =
  let newPos = case h of
        N -> (y - 1, x)
        E -> (y, x + 1)
        S -> (y + 1, x)
        W -> (y, x - 1)
      nextPiece = fromJust $ M.lookup newPos tm
      newCart
        | nextPiece == '-' || nextPiece == '|' = (newPos, h, i)
        | nextPiece == '/' && h == N = (newPos, E, i)
        | nextPiece == '/' && h == E = (newPos, N, i)
        | nextPiece == '/' && h == W = (newPos, S, i)
        | nextPiece == '/' && h == S = (newPos, W, i)
        | nextPiece == '\\' && h == N = (newPos, W, i)
        | nextPiece == '\\' && h == E = (newPos, S, i)
        | nextPiece == '\\' && h == W = (newPos, N, i)
        | nextPiece == '\\' && h == S = (newPos, E, i)
        | nextPiece == '+' = turn (newPos, h, i)
  in newCart

moveAll :: TrackState -> ([Coord], TrackState)
moveAll trackState@(_, cs) = foldl go ([], trackState) $ sort cs
  where go acc@(crashes, (tm, cs)) cart@(cartCoord,_,_)
          | cartCoord `elem` crashes = acc
          | otherwise =
            let newCart@(ncCoord, _, _) = move cart tm
                oldCartsCoords = map (\(occs,_,_) -> occs) cs
                crashed = elem ncCoord oldCartsCoords
                crashedCarts = cart:filter (\(coord,_,_) -> coord == ncCoord) cs
                newCarts = if crashed then foldl (flip delete) cs crashedCarts else newCart:delete cart cs
            in (if crashed then ncCoord:crashes else crashes, (tm, newCarts))

main = do
  input <- parseTrack . lines <$> readFile "day13.input"
  print . (++) "Part 1: " . show . (\(y,x) -> (x,y)) . minimum . fst . head . dropWhile (null . fst) $ iterate (moveAll . snd) ([], input)
  print . (++) "Part 2: " . show . (\((y,x),_,_) -> (x,y)) . head . snd . snd . head . dropWhile ((>1) . length . snd . snd) $ iterate (moveAll . snd) ([], input)
