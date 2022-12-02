import Data.Function (on)
import Data.List (sortBy)
import Data.Map ((!), Map)
import qualified Data.Map as M (alter, elems, filter, findWithDefault, fromList, keys, member, restrictKeys)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S (delete, difference, empty, filter, findMax, findMin, intersection, fromList, map, member, notMember, singleton, toList, union)

type Coord = (Int, Int)

data Faction = G | E deriving (Eq, Show)

-- battle status flag
data SFlag =
    Battling -- battle still ongoing
  | Partial -- battle complete before round could complete
  | Full -- battle complete, round complete
  deriving (Eq)

data Unit = Unit
  { coord :: Coord
  , faction :: Faction
  , hp :: Int
  , power :: Int
  } deriving (Show)

data BattleState = BattleState
    -- coords having open space, no need to remember where walls are
    -- may contain coords that units are occupying
  { open :: Set Coord
  , units :: [Unit]
    -- status of battle
  , status :: SFlag -- status of battle
  }

-- space at coord can only hold a single unit
-- this is sufficient for equality
instance Eq Unit where
  (==) x y = coord x == coord y

-- parse input into initial BattleState
parseBattle :: [String] -> BattleState
parseBattle xs = BattleState os us  Battling
  where os = S.fromList $ mapMaybe fst parsed
        us = mapMaybe snd parsed
        parsed = concat $ zipWith parseLine [0..] xs
        parseLine y = zipWith (parseChar y) [0..]
        parseChar y x c
          | c == 'G' = (pure yx, pure $ Unit yx G 200 3)
          | c == 'E' = (pure yx, pure $ Unit yx E 200 3)
          | c == '.' = (pure yx, Nothing)
          | otherwise = (Nothing, Nothing)
          where yx = (y, x)

-- all units of given faction
getFaction :: Faction -> [Unit] -> [Unit]
getFaction f = filter ((==f) . faction)

-- True when one faction remains
battleComplete :: [Unit] -> Bool
battleComplete us = null (getFaction E us) || null (getFaction G us)

-- opposing faction
opposition :: Faction -> Faction
opposition G = E
opposition E = G

-- all units of the opposing faction
getOpposition :: Unit -> [Unit] -> [Unit]
getOpposition u = getFaction (opposition $ faction u)

-- coords to the top, left, bottom, and right
adjacentSpaces :: Coord -> Set Coord
adjacentSpaces (y, x) = S.fromList [(y -1, x), (y + 1, x), (y, x + 1), (y, x - 1)]

-- spaces occupied by units
occupiedSpaces :: BattleState -> Set Coord
occupiedSpaces = S.fromList . map coord . units

-- open spaces that are also unoccupied
unoccupiedSpaces :: BattleState -> Set Coord
unoccupiedSpaces bs = S.difference (open bs) $ occupiedSpaces bs

-- coords to the top, left, bottom, and right that are open and unoccupied spaces
openNeighbors :: BattleState -> Coord -> Set Coord
openNeighbors bs = S.intersection (unoccupiedSpaces bs) . adjacentSpaces

-- all coords that are reachable from the given coord
reachableArea :: Coord -> BattleState -> Set Coord
reachableArea c bs = go (S.empty, S.singleton c)
  where
    go (as, cs)
      | null cs = as
      | otherwise =
        let as' = S.union as cs
            cs' = S.filter (`S.notMember` as') $ foldMap (openNeighbors bs) cs
        in go (as', cs')

-- shortest path from start to end
dijkstra :: (Ord node)
  => node -- start
  -> node -- end
  -> Set node -- available nodes
  -> (node -> Set node) -- valid neighbors
  -> [node]
dijkstra start end as f = go [end]
  where dm = dijkstraDistance start end as f
        go acc@(n:_)
          -- finish when current node is next to start node
          | dm ! n == 1 = acc
          | otherwise = go $ n':acc
          where
            -- neighboring nodes with shortest distance
            mm = minMembers . M.restrictKeys dm $ f n
            -- next node
            -- neighbor to current node with shortest distance
            -- prioritized by reading order
            n' = minimum $ M.keys mm

-- map of available nodes to distance from start
dijkstraDistance :: (Ord node)
  => node -- start
  -> node -- end
  -> Set node -- available nodes
  -> (node -> Set node) -- valid neighbors
  -> Map node Int -- nodes to distance from start
dijkstraDistance start end as f = go as (M.fromList [(start, 0)]) start
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
        uns = S.intersection s' $ f n
        -- update map with new distances for unvisited neighbors
        m' = foldl (update (d + 1)) m uns
        -- next node candidates
        cm = M.restrictKeys m' s'
        -- next node, unvisited node with shortest distance
        (n':_) = M.keys $ minMembers cm

-- shortest path from unit's current coord to given coord
getPath :: BattleState -> Unit -> Coord -> [Coord]
getPath bs u c = dijkstra (coord u) c rs (S.intersection us . adjacentSpaces)
  where rs = reachableArea c bs
        us = unoccupiedSpaces bs

-- map with members having minimum element
minMembers :: Ord a => Map k a -> Map k a
minMembers m = M.filter (==e) m
  where e = minimum $ M.elems m

-- list of coordinates unit can attack from
-- may contain unreachable coordinates
contestCoords :: Unit -> [Unit] -> Set Coord
contestCoords u = foldMap (adjacentSpaces . coord) . getOpposition u

-- True if an enemy unit is to the top, left, bottom, or right
canAttack :: Unit -> [Unit] -> Bool
canAttack u = S.member (coord u) . contestCoords u

-- enemies to the top, left, bottom, and right
adjacentEnemies :: Unit -> [Unit] -> [Unit]
adjacentEnemies u = filter (\x -> S.member (coord x) (adjacentSpaces $ coord u)) . getOpposition u

-- determine target of unit's attack
target :: Unit -> [Unit] -> Maybe Unit
target u us
  | null es = Nothing
  | otherwise = pure c
  where es = adjacentEnemies u us
        lowestHp = minimum $ map hp es
        -- enemy with lowest hp, prioritized by reading order
        (c:_) = sortBy (compare `on` coord) $ filter ((==lowestHp) . hp) es

-- Next coord unit will move to to reach closest attacking coord
-- Nothing if the unit can attack
-- Nothing if there are no coords that allow an attack on an opposing unit
nextMove :: Unit -> BattleState -> Maybe Coord
nextMove u bs
  | canAttack u (units bs) = Nothing
  | null rs = Nothing
  | otherwise = pure nc
  where
    -- coords unit can contest enemy from
    cs = contestCoords u $ units bs
    -- coords unit can contest enemy from that are also reachable
    rs = S.intersection cs $ reachableArea (coord u) bs
    -- shortest path prioritized by reading order of destination
    (nc:_) = shortestPath bs u $ S.toList rs

-- find the shortest paths from unit's postion to multiple end candidates
shortestPaths :: BattleState -> Unit -> [Coord] -> [[Coord]]
shortestPaths bs u es = go [] ds
  where
    -- list of tuples
    -- (manhattan distance between unit and end coord, end coord)
    -- sorted ascending by manhattan distance
    ds = sortBy (compare `on` fst) $ map (\c -> (manhattanDistance (coord u) c, c)) es
    -- find shortest path to each coordinate
    -- removing any candidate with a largeer manhattan distance
    -- than the shortest distance found as we go
    go acc [] = acc
    go acc ((_, x):xs)
      | null acc = go [path] xs'
      | pl < cl = go [path] xs'
      | pl > cl = go acc xs
      | pl == cl = go (path:acc) xs
      where
        path = getPath bs u x
        pl = length path
        cl = length $ head acc
        -- remove candidates with manhattan distance > path length
        xs' = filter ((<pl) . fst) xs

-- find the shortest path from unit's postion to multiple end candidates
-- ties are broken by reading order
shortestPath :: BattleState -> Unit -> [Coord] -> [Coord]
shortestPath bs u es = path
  where ps = shortestPaths bs u es
        -- shortest path prioritized by reading order of destination
        path
          | length ps == 1 = concat ps
          -- path with end coordinate first in reading order
          | otherwise = head $ filter ((==cd) . last) ps
          -- end coordinate that is first in reading order
          where cd = minimum $ map last ps

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- shortest list(s) in a set
shortest :: Set [a] -> Set [a]
shortest s = S.filter ((==sn) . length) s
  where sn = S.findMin $ S.map length s

-- update unit's power
setPower :: Int -> Unit -> Unit
setPower p u = Unit (coord u) (faction u) (hp u) p

-- update unit's coord
setCoord :: Coord -> Unit -> Unit
setCoord c u = Unit c (faction u) (hp u) (power u)

-- update battlestate's units
setUnits :: BattleState -> [Unit] -> BattleState
setUnits bs us = BattleState (open bs) us (status bs)

-- subtract from unit's hp
hit :: Int -> Unit -> Unit
hit damage u = Unit (coord u) (faction u) (hp u - damage) (power u)

-- edit element in list in place
editElement :: Eq a => (a -> a) -> a -> [a] -> [a]
editElement f x xs = ys ++ f z:zs
  where (ys, z:zs) = break (==x) xs

-- move unit if possible
move :: Unit -> BattleState -> (Coord, BattleState)
move u bs
  | canAttack u (units bs) = (coord u, bs)
  | otherwise =
    case nextMove u bs of
      Nothing -> (coord u, bs)
      Just c -> (c, setUnits bs (editElement (setCoord c) u $ units bs))

-- attack unit if possible
attack :: Unit -> BattleState -> BattleState
attack u bs
  | not $ canAttack u (units bs) = bs
  | otherwise =
    case target u (units bs) of
      Nothing -> bs
      Just t ->
        if hp t > power u
          then setUnits bs (editElement (hit (power u)) t $ units bs)
          else setUnits bs (filter (/=t) $ units bs)

-- perform move and attack for single unit
takeAction :: BattleState -> Unit -> BattleState
takeAction bs u
  | u `notElem` units bs = bs
  | otherwise = attack (setCoord c u) mbs
  where (c, mbs) = move u bs

-- perform single move and attack for each unit
doRound :: BattleState -> BattleState
doRound st = foldl go st . sortBy (compare `on` coord) $ units st
  where go bs u
          | status bs == Partial = bs
          | battleComplete (units bs) = BattleState (open bs) (units bs) Partial
          | otherwise = takeAction bs u

-- convert battle state to list of strings
-- suitable for printing to screen
printBattle :: BattleState -> [String]
printBattle bs = health : map printLine [0..my]
  where um = M.fromList $ map (\u -> (coord u, fc $ faction u)) $ units bs
        mx = S.findMax . S.map snd $ open bs
        my = S.findMax . S.map fst $ open bs
        printLine y = map (printChar y) [0..mx]
        printChar y x
          | (y, x) `M.member` um = um ! (y, x)
          | (y, x) `S.member` open bs = '.'
          | otherwise = '#'
        fc G = 'G'
        fc E = 'E'
        health = concatMap (\u -> (fc (faction u):": ") ++ show (hp u) ++ ", ") . sortBy (compare `on` coord) $ units bs

-- useful for debugging
pb :: BattleState -> IO ()
pb = mapM_ print . printBattle

outcome :: BattleState -> Int
outcome bs = battleDuration * (sum . map hp $ units e)
  where (fs, e:_) = break (battleComplete . units) $ iterate doRound bs
        battleDuration = length fs - (if Partial == status e then 1 else 0)

-- very lazy solution for this part
-- could at least try to determine a better starting point than 4
-- but I'm sick of this problem
part2 :: BattleState -> Int
part2 bs = outcome . fixElves bs . power . head . getFaction E $ units domination
  where elfCount = length . getFaction E . units
        originalElfCount = elfCount bs
        doBattle = head . dropWhile (not . battleComplete . units) . iterate doRound
        domination = head . filter ((==originalElfCount) . elfCount) $ map doBattle fixedBs
        fixedBs = map (fixElves bs) [4..]
        fixElves bs' n = setUnits bs' . map (powerUp n) $ units bs'
        powerUp n u
          | faction u == E = setPower n u
          | otherwise = u

main = do
  bs <- parseBattle . lines <$> readFile "day15.input"
  print . (++) "Part 1: " . show $ outcome bs
  print . (++) "Part 2: " . show $ part2 bs
