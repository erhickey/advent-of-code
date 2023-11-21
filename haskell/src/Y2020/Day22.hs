module Y2020.Day22 (solve) where

import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.Sequence ((|>), Seq, viewl, ViewL(..))
import qualified Data.Sequence as S (fromList)

type Deck = Seq Int

parseDeck :: [String] -> Deck
parseDeck = S.fromList . map read . filter (\s -> s /= "" && notElem ':' s)

playRound :: (Deck, Deck) -> (Deck, Deck)
playRound (d1, d2)
  | c1 > c2 = (d1' |> c1 |> c2, d2')
  | c2 > c1 = (d1', d2' |> c2 |> c1)
  where
    c1 :< d1' = viewl d1
    c2 :< d2' = viewl d2

playGame :: (Deck, Deck) -> [(Deck, Deck)]
playGame ds = rs ++ [e]
  where (rs, e:es) = break (\(d1, d2) -> null d1 || null d2) $ iterate playRound ds

winningDeck :: (Deck, Deck) -> Deck
winningDeck ds
  | null d1 = d2
  | null d2 = d1
  where (d1, d2) = last $ playGame ds

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse . toList

main = do
  decks <- bimap parseDeck parseDeck . break (=="") . lines <$> readFile "day22.input"
  print . (++) "Part 1: " . show . score $ winningDeck decks

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    part1 = ""
    part2 = ""
