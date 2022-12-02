import Control.Applicative ((<|>), liftA2)
import Data.Either (rights)
import Data.List (sort)
import qualified Data.Text as T (lines)
import qualified Data.Text.IO as T (readFile)
import Data.Attoparsec.Text (char, endOfInput, many', parseOnly, Parser)

data Chunk = Chunk { open :: Char, close :: Char, children :: [Chunk] }

chunksParser :: Parser [Chunk]
chunksParser = many' chunkParser

chunkParser :: Parser Chunk
chunkParser = do
  o <- char '(' <|> char '[' <|> char '{' <|> char '<'
  cs <- many' chunkParser
  c <- char ')' <|> char ']' <|> char '}' <|> char '>' <|> (endOfInput >> pure ' ')
  pure (Chunk o c cs)

corruptedLine :: [Chunk] -> Bool
corruptedLine = any corruptedChunks
  where corruptedChunks c = corruptedChunk c || any corruptedChunks (children c)

corruptedChunk :: Chunk -> Bool
corruptedChunk c = liftA2 (&&) (/=' ') (/= (match $ open c)) $ close c

corruptedClosers :: [Chunk] -> [Char]
corruptedClosers = concatMap go
  where go c
          | corruptedChunk c = close c:concatMap go (children c)
          | otherwise = concatMap go (children c)

completeLine :: [Chunk] -> [Char]
completeLine = reverse . concatMap go
  where go c
          | close c == ' ' = match (open c) : concatMap go (children c)
          | otherwise = concatMap go (children c)

match :: Char -> Char
match '(' = ')'
match '[' = ']'
match '{' = '}'
match '<' = '>'

scoreErrors :: [Char] -> Int
scoreErrors = sum . map go
  where
    go ')' = 3
    go ']' = 57
    go '}' = 1197
    go '>' = 25137

scoreFixes :: [Char] -> Int
scoreFixes = foldl (\x c -> x * 5 + go c) 0
  where
    go ')' = 1
    go ']' = 2
    go '}' = 3
    go '>' = 4

middle :: [a] -> a
middle xs = xs !! index
  where index = length xs `div` 2

main = do
  input <- rights . map (parseOnly chunksParser) . T.lines <$> T.readFile "day10.input"
  print . (++) "Part 1: " . show . scoreErrors . map (head . corruptedClosers) $ filter corruptedLine input
  print . (++) "Part 2: " . show . middle . sort . map (scoreFixes . completeLine) $ filter (not . corruptedLine) input
