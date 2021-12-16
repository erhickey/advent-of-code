import Data.Map ((!), Map)
import qualified Data.Map as M (alter, elems, filter, findWithDefault, fromList, keys, map, mapKeys, restrictKeys, unions)
import Data.Set (Set)
import qualified Data.Set as S (delete, intersection, fromList)

type Coord = (Int, Int)
type Grid = Map Coord Int

parse :: [String] -> Grid
parse = M.fromList . concat . zipWith parseLine [0..]
  where parseLine y = zipWith (\x n -> ((x, y), read [n])) [0..]

neighbors :: Coord -> Set Coord
neighbors (y, x) = S.fromList [(y -1, x), (y + 1, x), (y, x + 1), (y, x - 1)]

-- map of available nodes to distance from start
dijkstra :: (Ord node)
  => node -- start
  -> node -- end
  -> Set node -- available nodes
  -> (node -> Set node) -- get neighbors of node
  -> (node -> Int) -- get distance to node
  -> Map node Int -- nodes to distance from start
dijkstra start end us neighbF distF = go us (M.fromList [(start, 0)]) start
  where
    -- update map with new distance if it is lower
    update x m n = M.alter (pure . maybe x (min x)) n m
    -- traverse nodes, updating distances as we go
    go s m n
      | n == end = m
      | otherwise = go s' m' n'
      where
        -- current node distance from start
        d = M.findWithDefault 0 n m
        -- delete current node from set of unvisited
        s' = S.delete n s
        -- unvisited neighbors of current node
        uns = S.intersection s' $ neighbF n
        -- update map with new distances for unvisited neighbors
        m' = foldl (\acc un -> update (d + distF un) acc un) m uns
        -- next node candidates
        cm = M.restrictKeys m' s'
        -- next node, unvisited node with shortest distance
        (n':_) = M.keys $ minMembers cm

minMembers :: Ord a => Map k a -> Map k a
minMembers m = M.filter (==e) m
  where e = minimum $ M.elems m

getPath :: Grid -> Map Coord Int
getPath g = dijkstra (0, 0) (maximum $ M.keys g) (S.fromList $ M.keys g) neighbors (g !)

augment :: Grid -> Grid
augment g = M.unions $ map (`gridToDown` (M.unions $ map (`gridToRight` g) [0..4])) [0..4]
  where
    maxX = (+) 1 . maximum . map fst $ M.keys g
    maxY = (+) 1 . maximum . map snd $ M.keys g
    inc n = M.map (\v -> if v + n > 9 then v + n - 9 else v + n)
    gridToRight n = inc n . M.mapKeys (\(x, y) -> (x + maxX * n, y))
    gridToDown n = inc n . M.mapKeys (\(x, y) -> (x, y + maxY * n))

main = do
  input <- parse . lines <$> readFile "day15.input"
  print . (++) "Part 1: " . show . last . M.elems $ getPath input
  print . (++) "Part 2: " . show . last . M.elems . getPath $ augment input
