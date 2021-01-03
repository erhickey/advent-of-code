{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<|>))
import Data.IntMap ((!), IntMap)
import qualified Data.IntMap as M (fromList, insert)
import Data.Either (rights)
import Data.Text (Text)
import qualified Data.Text as T (length, lines)
import qualified Data.Text.IO as T (readFile)

import Data.Attoparsec.Text (char, decimal, endOfInput, IResult(..), parse, parseOnly, Parser, sepBy, string)

data Rule = Letter Char | Single [Int] | Double ([Int], [Int])

ruleLineParser :: Parser (Int, Rule)
ruleLineParser = do
  n <- decimal
  string ": "
  rule <- ruleParser
  pure (n, rule)

ruleParser :: Parser Rule
ruleParser = do
  string "\"a\"" >> pure (Letter 'a')
  <|> (string "\"b\"" >> pure (Letter 'b'))
  <|> do
    rs1 <- decimal `sepBy` char ' '
    (string " | " >> do
      rs2 <- decimal `sepBy` char ' '
      pure $ Double (rs1, rs2))
      <|> pure (Single rs1)

ruleValidator :: IntMap Rule -> Rule -> [Int] -> Parser [Int]
ruleValidator _ (Letter c) ns = char c >> pure ns
ruleValidator m (Single rs) ns = do
  ns' <- mapM (\x -> (endOfInput >> pure []) <|> ruleValidator m (m ! x) [x]) rs
  pure $ ns ++ concat ns'
ruleValidator m (Double (rs1, rs2)) ns = ruleValidator m (Single rs1) ns <|> ruleValidator m (Single rs2) ns

isValid :: IntMap Rule -> Text -> Bool
isValid m = go . parse (ruleValidator m (m ! 0) [0])
  where
    go x = case x of
      Fail {} -> False
      Partial f -> go $ f ""
      Done i r -> T.length i == 0 && 31 `elem` r && 42 `elem` r

numValid :: IntMap Rule -> [Text] -> Int
numValid m = length . filter (==True) . map (isValid m)

main = do
  input <- T.lines <$> T.readFile "day19.input"
  let (rules, _:msgs) = break (=="") input
      ruleMap = M.fromList . rights $ map (parseOnly ruleLineParser) rules
      p2Map = M.insert 8 (Double ([42], [42,8])) $ M.insert 11 (Double ([42,31], [42,11,31])) ruleMap
  print . (++) "Part 1: " . show $ numValid ruleMap msgs
  print . (++) "Part 2: " . show $ numValid p2Map msgs
