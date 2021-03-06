import Data.Char (isAlpha, isNumber)
import Data.List (isInfixOf, group, sort)

data Room = Room
  { encryptedName :: String
  , sectorId :: Int
  , hashCode :: String
  }

parseLine :: String -> Room
parseLine s = Room {encryptedName = name, sectorId = id, hashCode = hc}
  where name = init $ takeWhile (not . isNumber) s
        id = read $ filter isNumber s
        hc = filter isAlpha $ dropWhile (/='[') s

hash :: String -> String
hash = take 5 . map snd . sort . map (\l@(x:_) -> (negate . length $ l, x)) . group . sort . filter isAlpha

isValid :: Room -> Bool
isValid Room {encryptedName = n, hashCode = h} = hash n == h

decrypt :: Room -> String
decrypt Room {encryptedName = n, sectorId = s} = map (rotate s) n

rotate :: Int -> Char -> Char
rotate _ '-' = ' '
rotate 0 c = c
rotate n 'z' = rotate (n - 1) 'a'
rotate n c = rotate (n - 1) $ succ c

main = do
  validRooms <- filter isValid . map parseLine . lines <$> readFile "day4.input"
  print . (++) "Part 1: " . show . sum . map sectorId $ validRooms
  print . (++) "Part 2: " . show . filter (("north" `isInfixOf`) . snd) . zip (map sectorId validRooms) . map decrypt $ validRooms
