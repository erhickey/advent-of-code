module Y2020.Day5 (solve) where

data FBLR = F | B | L | R deriving (Eq, Read)

parsePass :: String -> ([FBLR], [FBLR])
parsePass s = (map readChar $ take 7 s, map readChar $ drop 7 s)

readChar :: Read a => Char -> a
readChar = read . (:[])

half :: (Int, Int) -> FBLR -> (Int, Int)
half (min, max) fblr
  | fblr == F || fblr == L = (min, lowerUB)
  | fblr == B || fblr == R = (upperLB, max)
  where lowerUB = floor mid
        upperLB = ceiling mid
        mid = fromIntegral min + (fromIntegral (max - min) / 2)

seatPos :: ([FBLR], [FBLR]) -> (Int, Int)
seatPos (rs, cs) = (fst row, fst col)
  where row = foldl half (0, 127) rs
        col = foldl half (0, 7) cs

seatId :: (Int, Int) -> Int
seatId (row, col) = row * 8 + col

idsBetweenRows :: Int -> Int -> [Int]
idsBetweenRows min max = [ seatId (r,c) | r <- [min..max], c <- [0..7] ]

findSeat :: [Int] -> Int
findSeat ss = head . head . filter ((==1) . length) . map (filter (`notElem` ss)) $ zipWith idsBetweenRows [0..] [127,126..]

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    ids = map (seatId . seatPos . parsePass) $ lines input
    part1 = show $ maximum ids
    part2 = show $ findSeat ids
